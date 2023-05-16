//! Navigate an endless amount of content with a scrollbar.
use iced::advanced::{Clipboard, layout, Layout, renderer, Shell, widget, Widget};
use iced::event::{self, Event};
use iced::{Background, Color, Command, Element, keyboard, Length, Pixels, Point, Rectangle, Size, Vector};
use iced::mouse;
use iced::overlay;
use iced::touch;
use iced::advanced::widget::operation::{self, Operation};
use iced::advanced::widget::tree::{self, Tree};

use iced::widget::scrollable::{RelativeOffset, Scrollbar, StyleSheet};

/// A widget that can vertically display an infinite amount of content with a
/// scrollbar.
#[allow(missing_debug_implementations)]
pub struct FixedScrollable<'a, Message, Renderer>
  where
    Renderer: iced::advanced::Renderer,
    Renderer::Theme: StyleSheet,
{
  id: Option<Id>,
  width: Length,
  height: Length,
  vertical: Properties,
  horizontal: Option<Properties>,
  content: Element<'a, Message, Renderer>,
  on_scroll:
  Option<Box<dyn Fn(Viewport, Option<(usize, usize)>) -> Message + 'a>>,
  style: <Renderer::Theme as StyleSheet>::Style,
  pub element_height: u64,
  pub elements: usize,
}

/// The amount of absolute offset in each direction of a [`Scrollable`].
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct AbsoluteOffset<T = f32> {
  /// The amount of horizontal offset
  pub x: T,
  /// The amount of vertical offset
  pub y: T,
}

impl<'a, Message, Renderer> FixedScrollable<'a, Message, Renderer>
  where
    Renderer: iced::advanced::Renderer,
    Renderer::Theme: StyleSheet,
{
  /// Creates a new [`Scrollable`].
  pub fn new(
    element_height: u64,
    elements: usize,
    content: impl Into<Element<'a, Message, Renderer>>,
  ) -> Self {
    FixedScrollable {
      id: None,
      width: Length::Shrink,
      height: Length::Shrink,
      vertical: Properties::default(),
      horizontal: None,
      content: content.into(),
      on_scroll: None,
      style: Default::default(),
      element_height,
      elements,
    }
  }

  /// Sets the [`Id`] of the [`Scrollable`].
  pub fn id(mut self, id: Id) -> Self {
    self.id = Some(id);
    self
  }

  /// Sets the width of the [`Scrollable`].
  pub fn width(mut self, width: impl Into<Length>) -> Self {
    self.width = width.into();
    self
  }

  /// Sets the height of the [`Scrollable`].
  pub fn height(mut self, height: impl Into<Length>) -> Self {
    self.height = height.into();
    self
  }

  /// Configures the vertical scrollbar of the [`Scrollable`] .
  pub fn vertical_scroll(mut self, properties: Properties) -> Self {
    self.vertical = properties;
    self
  }

  /// Configures the horizontal scrollbar of the [`Scrollable`] .
  pub fn horizontal_scroll(mut self, properties: Properties) -> Self {
    self.horizontal = Some(properties);
    self
  }

  /// Sets a function to call when the [`Scrollable`] is scrolled.
  ///
  /// The function takes the [`Viewport`] of the [`Scrollable`]
  pub fn on_scroll(
    mut self,
    f: impl Fn(Viewport, Option<(usize, usize)>) -> Message + 'a,
  ) -> Self {
    self.on_scroll = Some(Box::new(f));
    self
  }

  /// Sets the style of the [`Scrollable`] .
  pub fn style(
    mut self,
    style: impl Into<<Renderer::Theme as StyleSheet>::Style>,
  ) -> Self {
    self.style = style.into();
    self
  }
}

/// Properties of a scrollbar within a [`Scrollable`].
#[derive(Debug)]
pub struct Properties {
  width: f32,
  margin: f32,
  scroller_width: f32,
}

impl Default for Properties {
  fn default() -> Self {
    Self {
      width: 10.0,
      margin: 0.0,
      scroller_width: 10.0,
    }
  }
}

impl Properties {
  /// Creates new [`Properties`] for use in a [`Scrollable`].
  pub fn new() -> Self {
    Self::default()
  }

  /// Sets the scrollbar width of the [`Scrollable`] .
  /// Silently enforces a minimum width of 1.
  pub fn width(mut self, width: impl Into<Pixels>) -> Self {
    self.width = width.into().0.max(1.0);
    self
  }

  /// Sets the scrollbar margin of the [`Scrollable`] .
  pub fn margin(mut self, margin: impl Into<Pixels>) -> Self {
    self.margin = margin.into().0;
    self
  }

  /// Sets the scroller width of the [`Scrollable`] .
  /// Silently enforces a minimum width of 1.
  pub fn scroller_width(mut self, scroller_width: impl Into<Pixels>) -> Self {
    self.scroller_width = scroller_width.into().0.max(1.0);
    self
  }
}

impl<'a, Message, Renderer> Widget<Message, Renderer>
for FixedScrollable<'a, Message, Renderer>
  where
    Renderer: iced::advanced::Renderer,
    Renderer::Theme: StyleSheet,
{
  fn tag(&self) -> tree::Tag {
    tree::Tag::of::<State>()
  }

  fn state(&self) -> tree::State {
    tree::State::new(State::new())
  }

  fn children(&self) -> Vec<Tree> {
    vec![Tree::new(&self.content)]
  }

  fn diff(&self, tree: &mut Tree) {
    tree.diff_children(std::slice::from_ref(&self.content))
  }

  fn width(&self) -> Length {
    self.width
  }

  fn height(&self) -> Length {
    self.height
  }

  fn layout(
    &self,
    renderer: &Renderer,
    limits: &layout::Limits,
  ) -> layout::Node {
    layout(
      renderer,
      limits,
      self.width,
      self.height,
      self.horizontal.is_some(),
      |renderer, limits| {
        self.content.as_widget().layout(renderer, limits)
      },
    )
  }

  // TODO: This function probably needs fixed.
  fn operate(
    &self,
    tree: &mut Tree,
    layout: Layout<'_>,
    renderer: &Renderer,
    operation: &mut dyn Operation<Message>,
  ) {
    //let state = tree.state.downcast_mut::<State>();

    // The scrollable trait wants f32 offsets.
    //operation.scrollable(state, self.id.as_ref().map(|id| &id.0));

    operation.container(
      self.id.as_ref().map(|id| &id.0),
      &mut |operation| {
        self.content.as_widget().operate(
          &mut tree.children[0],
          layout.children().next().unwrap(),
          renderer,
          operation,
        );
      },
    );
  }

  fn on_event(
    &mut self,
    tree: &mut Tree,
    event: Event,
    layout: Layout<'_>,
    cursor_position: Point,
    renderer: &Renderer,
    clipboard: &mut dyn Clipboard,
    shell: &mut Shell<'_, Message>,
  ) -> event::Status {
    update(
      tree.state.downcast_mut::<State>(),
      event,
      layout,
      cursor_position,
      clipboard,
      shell,
      &self.vertical,
      self.horizontal.as_ref(),
      &self.on_scroll,
      |event, layout, cursor_position, clipboard, shell| {
        self.content.as_widget_mut().on_event(
          &mut tree.children[0],
          event,
          layout,
          cursor_position,
          renderer,
          clipboard,
          shell,
        )
      },
      self.element_height,
      self.elements,
    )
  }

  fn draw(
    &self,
    tree: &Tree,
    renderer: &mut Renderer,
    theme: &Renderer::Theme,
    style: &renderer::Style,
    layout: Layout<'_>,
    cursor_position: Point,
    _viewport: &Rectangle,
  ) {
    draw(
      tree.state.downcast_ref::<State>(),
      renderer,
      theme,
      layout,
      cursor_position,
      &self.vertical,
      self.horizontal.as_ref(),
      &self.style,
      |renderer, layout, cursor_position, viewport| {
        self.content.as_widget().draw(
          &tree.children[0],
          renderer,
          theme,
          style,
          layout,
          cursor_position,
          viewport,
        )
      },
      self.element_height,
      self.elements,
    )
  }

  fn mouse_interaction(
    &self,
    tree: &Tree,
    layout: Layout<'_>,
    cursor_position: Point,
    _viewport: &Rectangle,
    renderer: &Renderer,
  ) -> mouse::Interaction {
    mouse_interaction(
      tree.state.downcast_ref::<State>(),
      layout,
      cursor_position,
      &self.vertical,
      self.horizontal.as_ref(),
      |layout, cursor_position, viewport| {
        self.content.as_widget().mouse_interaction(
          &tree.children[0],
          layout,
          cursor_position,
          viewport,
          renderer,
        )
      },
      self.element_height,
      self.elements,
    )
  }

  fn overlay<'b>(
    &'b mut self,
    tree: &'b mut Tree,
    layout: Layout<'_>,
    renderer: &Renderer,
  ) -> Option<overlay::Element<'b, Message, Renderer>> {
    self.content
      .as_widget_mut()
      .overlay(
        &mut tree.children[0],
        layout.children().next().unwrap(),
        renderer,
      )
      .map(|overlay| {
        let bounds = layout.bounds();
        let content_layout = layout.children().next().unwrap();
        let content_bounds = Rectangle::<u64> {
          x: content_layout.bounds().x as u64,
          y: content_layout.bounds().y as u64,
          width: content_layout.bounds().width as u64,
          height: (self.elements as u64 * self.element_height),
        };
        let offset = tree
          .state
          .downcast_ref::<State>()
          .offset(bounds, content_bounds);

        overlay.translate(Vector::new(-(offset.x as f32), -(offset.y as f32)))
      })
  }
}

impl<'a, Message, Renderer> From<FixedScrollable<'a, Message, Renderer>>
for Element<'a, Message, Renderer>
  where
    Message: 'a,
    Renderer: 'a + iced::advanced::Renderer,
    Renderer::Theme: StyleSheet,
{
  fn from(
    text_input: FixedScrollable<'a, Message, Renderer>,
  ) -> Element<'a, Message, Renderer> {
    Element::new(text_input)
  }
}

/// The identifier of a [`Scrollable`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Id(widget::Id);

impl Id {
  /// Creates a custom [`Id`].
  pub fn new(id: impl Into<std::borrow::Cow<'static, str>>) -> Self {
    Self(widget::Id::new(id))
  }

  /// Creates a unique [`Id`].
  ///
  /// This function produces a different [`Id`] every time it is called.
  pub fn unique() -> Self {
    Self(widget::Id::unique())
  }
}

impl From<Id> for widget::Id {
  fn from(id: Id) -> Self {
    id.0
  }
}

/// Produces a [`Command`] that snaps the [`Scrollable`] with the given [`Id`]
/// to the provided `percentage` along the x & y axis.
pub fn snap_to<Message: 'static>(
  id: Id,
  offset: RelativeOffset,
) -> Command<Message> {
  Command::widget(operation::scrollable::snap_to(id.0, offset))
}

// TODO: Scrollable trait is not generic on offset.
/// Produces a [`Command`] that scrolls the [`Scrollable`] with the given [`Id`]
/// to the provided [`AbsoluteOffset`] along the x & y axis.
/*pub fn scroll_to<Message: 'static>(
  id: Id,
  offset: AbsoluteOffset,
) -> Command<Message> {
  Command::widget(operation::scrollable::scroll_to(id.0, offset))
}*/

/// Computes the layout of a [`Scrollable`].
pub fn layout<Renderer>(
  renderer: &Renderer,
  limits: &layout::Limits,
  width: Length,
  height: Length,
  horizontal_enabled: bool,
  layout_content: impl FnOnce(&Renderer, &layout::Limits) -> layout::Node,
) -> layout::Node {
  let limits = limits.width(width).height(height);

  let child_limits = layout::Limits::new(
    Size::new(limits.min().width, 0.0),
    Size::new(
      if horizontal_enabled {
        f32::INFINITY
      } else {
        limits.max().width
      },
      f32::MAX,
    ),
  );

  let content = layout_content(renderer, &child_limits);
  let size = limits.resolve(content.size());

  layout::Node::with_children(size, vec![content])
}

/// Processes an [`Event`] and updates the [`State`] of a [`Scrollable`]
/// accordingly.
/// Any events generated for the [`Scrollable`] content will be relative to the current start index.
pub fn update<Message>(
  state: &mut State,
  event: Event,
  layout: Layout<'_>,
  cursor_position: Point,
  clipboard: &mut dyn Clipboard,
  shell: &mut Shell<'_, Message>,
  vertical: &Properties,
  horizontal: Option<&Properties>,
  on_scroll: &Option<Box<dyn Fn(Viewport, Option<(usize, usize)>) -> Message + '_>>,
  update_content: impl FnOnce(
    Event,
    Layout<'_>,
    Point,
    &mut dyn Clipboard,
    &mut Shell<'_, Message>,
  ) -> event::Status,
  element_height: u64,
  elements: usize,
) -> event::Status {
  let bounds = layout.bounds();
  let mouse_over_scrollable = bounds.contains(cursor_position);

  let content = layout.children().next().unwrap();
  let content_bounds = Rectangle::<u64> {
    x: content.bounds().x as u64,
    y: content.bounds().y as u64,
    width: content.bounds().width as u64,
    height: (elements as u64 * element_height),
  };

  let scrollbars =
    Scrollbars::new(state, vertical, horizontal, bounds, content_bounds);

  let (mouse_over_y_scrollbar, mouse_over_x_scrollbar) =
    scrollbars.is_mouse_over(cursor_position);

  let event_status = {
    let cursor_position = if mouse_over_scrollable
      && !(mouse_over_y_scrollbar || mouse_over_x_scrollbar)
    {
      cursor_position + view_offset(state, bounds, content_bounds, element_height)
    } else {
      // TODO: Make `cursor_position` an `Option<Point>` so we can encode
      // cursor availability.
      // This will probably happen naturally once we add multi-window
      // support.
      Point::new(-1.0, -1.0)
    };

    update_content(
      event.clone(),
      content,
      cursor_position,
      clipboard,
      shell,
    )
  };

  if let event::Status::Captured = event_status {
    return event::Status::Captured;
  }

  if let Event::Keyboard(keyboard::Event::ModifiersChanged(modifiers)) = event
  {
    state.keyboard_modifiers = modifiers;

    return event::Status::Ignored;
  }

  if mouse_over_scrollable {
    match event {
      Event::Mouse(mouse::Event::WheelScrolled { delta }) => {
        let delta = match delta {
          mouse::ScrollDelta::Lines { x, y } => {
            // TODO: Configurable speed/friction (?)
            let movement = if state.keyboard_modifiers.shift() {
              Vector::new(y, x)
            } else {
              Vector::new(x, y)
            };

            movement * 60.0
          }
          mouse::ScrollDelta::Pixels { x, y } => Vector::new(x, y),
        };

        state.scroll(delta, bounds, content_bounds);

        notify_on_scroll(
          state,
          on_scroll,
          bounds,
          content_bounds,
          shell,
          element_height,
          elements,
        );

        return event::Status::Captured;
      }
      Event::Touch(event)
      if state.scroll_area_touched_at.is_some()
        || !mouse_over_y_scrollbar && !mouse_over_x_scrollbar =>
        {
          match event {
            touch::Event::FingerPressed { .. } => {
              state.scroll_area_touched_at = Some(cursor_position);
            }
            touch::Event::FingerMoved { .. } => {
              if let Some(scroll_box_touched_at) =
                state.scroll_area_touched_at
              {
                let delta = Vector::new(
                  cursor_position.x - scroll_box_touched_at.x,
                  cursor_position.y - scroll_box_touched_at.y,
                );

                state.scroll(delta, bounds, content_bounds);

                state.scroll_area_touched_at =
                  Some(cursor_position);

                notify_on_scroll(
                  state,
                  on_scroll,
                  bounds,
                  content_bounds,
                  shell,
                  element_height,
                  elements,
                );
              }
            }
            touch::Event::FingerLifted { .. }
            | touch::Event::FingerLost { .. } => {
              state.scroll_area_touched_at = None;
            }
          }

          return event::Status::Captured;
        }
      _ => {}
    }
  }

  if let Some(scroller_grabbed_at) = state.y_scroller_grabbed_at {
    match event {
      Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))
      | Event::Touch(touch::Event::FingerLifted { .. })
      | Event::Touch(touch::Event::FingerLost { .. }) => {
        state.y_scroller_grabbed_at = None;

        return event::Status::Captured;
      }
      Event::Mouse(mouse::Event::CursorMoved { .. })
      | Event::Touch(touch::Event::FingerMoved { .. }) => {
        if let Some(scrollbar) = scrollbars.y {
          state.scroll_y_to(
            scrollbar.scroll_percentage_y(
              scroller_grabbed_at,
              cursor_position,
            ),
            bounds,
            content_bounds,
          );

          notify_on_scroll(
            state,
            on_scroll,
            bounds,
            content_bounds,
            shell,
            element_height,
            elements,
          );

          return event::Status::Captured;
        }
      }
      _ => {}
    }
  } else if mouse_over_y_scrollbar {
    match event {
      Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))
      | Event::Touch(touch::Event::FingerPressed { .. }) => {
        if let (Some(scroller_grabbed_at), Some(scrollbar)) =
          (scrollbars.grab_y_scroller(cursor_position), scrollbars.y)
        {
          state.scroll_y_to(
            scrollbar.scroll_percentage_y(
              scroller_grabbed_at,
              cursor_position,
            ),
            bounds,
            content_bounds,
          );

          state.y_scroller_grabbed_at = Some(scroller_grabbed_at);

          notify_on_scroll(
            state,
            on_scroll,
            bounds,
            content_bounds,
            shell,
            element_height,
            elements,
          );
        }

        return event::Status::Captured;
      }
      _ => {}
    }
  }

  if let Some(scroller_grabbed_at) = state.x_scroller_grabbed_at {
    match event {
      Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))
      | Event::Touch(touch::Event::FingerLifted { .. })
      | Event::Touch(touch::Event::FingerLost { .. }) => {
        state.x_scroller_grabbed_at = None;

        return event::Status::Captured;
      }
      Event::Mouse(mouse::Event::CursorMoved { .. })
      | Event::Touch(touch::Event::FingerMoved { .. }) => {
        if let Some(scrollbar) = scrollbars.x {
          state.scroll_x_to(
            scrollbar.scroll_percentage_x(
              scroller_grabbed_at,
              cursor_position,
            ),
            bounds,
            content_bounds,
          );

          notify_on_scroll(
            state,
            on_scroll,
            bounds,
            content_bounds,
            shell,
            element_height,
            elements,
          );
        }

        return event::Status::Captured;
      }
      _ => {}
    }
  } else if mouse_over_x_scrollbar {
    match event {
      Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))
      | Event::Touch(touch::Event::FingerPressed { .. }) => {
        if let (Some(scroller_grabbed_at), Some(scrollbar)) =
          (scrollbars.grab_x_scroller(cursor_position), scrollbars.x)
        {
          state.scroll_x_to(
            scrollbar.scroll_percentage_x(
              scroller_grabbed_at,
              cursor_position,
            ),
            bounds,
            content_bounds,
          );

          state.x_scroller_grabbed_at = Some(scroller_grabbed_at);

          notify_on_scroll(
            state,
            on_scroll,
            bounds,
            content_bounds,
            shell,
            element_height,
            elements,
          );

          return event::Status::Captured;
        }
      }
      _ => {}
    }
  }

  event::Status::Ignored
}

/// Computes the current [`mouse::Interaction`] of a [`Scrollable`].
pub fn mouse_interaction(
  state: &State,
  layout: Layout<'_>,
  cursor_position: Point,
  vertical: &Properties,
  horizontal: Option<&Properties>,
  content_interaction: impl FnOnce(
    Layout<'_>,
    Point,
    &Rectangle,
  ) -> mouse::Interaction,
  element_height: u64,
  elements: usize,
) -> mouse::Interaction {
  let bounds = layout.bounds();
  let mouse_over_scrollable = bounds.contains(cursor_position);

  let content_layout = layout.children().next().unwrap();
  let content_bounds = Rectangle::<u64> {
    x: content_layout.bounds().x as u64,
    y: content_layout.bounds().y as u64,
    width: content_layout.bounds().width as u64,
    height: (elements as u64 * element_height),
  };

  let scrollbars =
    Scrollbars::new(state, vertical, horizontal, bounds, content_bounds);

  let (mouse_over_y_scrollbar, mouse_over_x_scrollbar) =
    scrollbars.is_mouse_over(cursor_position);

  if (mouse_over_x_scrollbar || mouse_over_y_scrollbar)
    || state.scrollers_grabbed()
  {
    mouse::Interaction::Idle
  } else {
    let offset = view_offset(state, bounds, content_bounds, element_height);

    let cursor_position = if mouse_over_scrollable
      && !(mouse_over_y_scrollbar || mouse_over_x_scrollbar)
    {
      cursor_position + offset
    } else {
      Point::new(-1.0, -1.0)
    };
    content_interaction(
      content_layout,
      cursor_position,
      &Rectangle {
        y: bounds.y + offset.y as f32,
        x: bounds.x + offset.x as f32,
        ..bounds
      },
    )
  }
}

/// Get the offset relative the scrollable content.
fn view_offset(
  state: &State,
  bounds: Rectangle,
  content_bounds: Rectangle<u64>,
  element_height: u64,
) -> Vector<f32>
{
  let mut offset = state.offset(bounds, content_bounds);

  let discrete_offset_y = offset.y / element_height;
  let discrete_offset_y_height = element_height * discrete_offset_y;
  offset.y = u64::min(offset.y - discrete_offset_y_height, element_height);

  Vector::new(offset.x as f32, offset.y as f32)
}

/// Draws a [`Scrollable`].
pub fn draw<Renderer>(
  state: &State,
  renderer: &mut Renderer,
  theme: &Renderer::Theme,
  layout: Layout<'_>,
  cursor_position: Point,
  vertical: &Properties,
  horizontal: Option<&Properties>,
  style: &<Renderer::Theme as StyleSheet>::Style,
  draw_content: impl FnOnce(&mut Renderer, Layout<'_>, Point, &Rectangle),
  element_height: u64,
  elements: usize,
) where
  Renderer: iced::advanced::Renderer,
  Renderer::Theme: StyleSheet,
{
  let bounds = layout.bounds();
  let content_layout = layout.children().next().unwrap();
  let content_bounds = Rectangle::<u64> {
    x: content_layout.bounds().x as u64,
    y: content_layout.bounds().y as u64,
    width: content_layout.bounds().width as u64,
    height: (elements as u64 * element_height),
  };

  let scrollbars =
    Scrollbars::new(state, vertical, horizontal, bounds, content_bounds);

  let mouse_over_scrollable = bounds.contains(cursor_position);
  let (mouse_over_y_scrollbar, mouse_over_x_scrollbar) =
    scrollbars.is_mouse_over(cursor_position);

  let offset = view_offset(state, bounds, content_bounds, element_height);

  let cursor_position = if mouse_over_scrollable
    && !(mouse_over_x_scrollbar || mouse_over_y_scrollbar)
  {
    cursor_position + offset
  } else {
    Point::new(-1.0, -1.0)
  };

  // Draw inner content
  if scrollbars.active() {
    renderer.with_layer(bounds, |renderer| {
      renderer.with_translation(
        Vector::new(-(offset.x as f32), -(offset.y as f32)),
        |renderer| {
          draw_content(
            renderer,
            content_layout,
            cursor_position,
            &Rectangle {
              y: bounds.y as f32 + (offset.y as f32),
              x: bounds.x + offset.x as f32,
              ..bounds
            },
          );
        },
      );
    });

    let draw_scrollbar =
      |renderer: &mut Renderer,
       style: Scrollbar,
       scrollbar: &internals::Scrollbar| {
        //track
        if style.background.is_some()
          || (style.border_color != Color::TRANSPARENT
          && style.border_width > 0.0)
        {
          renderer.fill_quad(
            renderer::Quad {
              bounds: scrollbar.bounds,
              border_radius: style.border_radius.into(),
              border_width: style.border_width,
              border_color: style.border_color,
            },
            style
              .background
              .unwrap_or(Background::Color(Color::TRANSPARENT)),
          );
        }

        //thumb
        if style.scroller.color != Color::TRANSPARENT
          || (style.scroller.border_color != Color::TRANSPARENT
          && style.scroller.border_width > 0.0)
        {
          renderer.fill_quad(
            renderer::Quad {
              bounds: scrollbar.scroller.bounds,
              border_radius: style.scroller.border_radius.into(),
              border_width: style.scroller.border_width,
              border_color: style.scroller.border_color,
            },
            style.scroller.color,
          );
        }
      };

    renderer.with_layer(
      Rectangle {
        width: bounds.width + 2.0,
        height: bounds.height + 2.0,
        ..bounds
      },
      |renderer| {
        //draw y scrollbar
        if let Some(scrollbar) = scrollbars.y {
          let style = if state.y_scroller_grabbed_at.is_some() {
            theme.dragging(style)
          } else if mouse_over_scrollable {
            theme.hovered(style, mouse_over_y_scrollbar)
          } else {
            theme.active(style)
          };

          draw_scrollbar(renderer, style, &scrollbar);
        }

        //draw x scrollbar
        if let Some(scrollbar) = scrollbars.x {
          let style = if state.x_scroller_grabbed_at.is_some() {
            theme.dragging_horizontal(style)
          } else if mouse_over_scrollable {
            theme.hovered_horizontal(style, mouse_over_x_scrollbar)
          } else {
            theme.active_horizontal(style)
          };

          draw_scrollbar(renderer, style, &scrollbar);
        }
      },
    );
  } else {
    draw_content(
      renderer,
      content_layout,
      cursor_position,
      &Rectangle {
        x: bounds.x + offset.x as f32,
        y: bounds.y + offset.y as f32,
        ..bounds
      },
    );
  }
}

fn notify_on_scroll<Message>(
  state: &mut State,
  on_scroll: &Option<Box<dyn Fn(Viewport, Option<(usize, usize)>) -> Message + '_>, >,
  bounds: Rectangle,
  content_bounds: Rectangle<u64>,
  shell: &mut Shell<'_, Message>,
  element_height: u64,
  elements: usize,
) {
  if let Some(on_scroll) = on_scroll {
    if content_bounds.width <= bounds.width as u64
      && content_bounds.height <= bounds.height as u64
    {
      return;
    }

    let viewport = Viewport {
      offset_x: state.offset_x,
      offset_y: state.offset_y,
      bounds,
      content_bounds,
    };

    // Todo: Investigate. Disabled because it will cause scrolling at the end to bug out for large lists.
    // Don't publish redundant viewports to shell
    /*        if let Some(last_notified) = state.last_notified {
                let last_relative_offset = last_notified.relative_offset();
                let current_relative_offset = viewport.relative_offset();

                let unchanged = |a: f32, b: f32| {
                    (a - b).abs() <= f32::EPSILON || (a.is_nan() && b.is_nan())
                };

                if unchanged(last_relative_offset.x, current_relative_offset.x)
                    && unchanged(last_relative_offset.y, current_relative_offset.y)
                {
                    return;
                }
            }*/

    let start_offset = viewport.absolute_offset().y / element_height;
    let end_offset = u64::min(
      (viewport.absolute_offset().y + viewport.bounds.height as u64)
        / element_height
        + 1, // +1 to make sure we always render the last element. If this overflows, we shall crash.
      elements as u64 - 1,
    );

    assert!(start_offset <= end_offset);

    shell.publish(on_scroll(
      viewport,
      Some((start_offset as usize, end_offset as usize)),
    ));
    state.last_notified = Some(viewport);
  }
}

/// The local state of a [`Scrollable`].
#[derive(Debug, Clone, Copy)]
pub struct State {
  scroll_area_touched_at: Option<Point>,
  offset_y: Offset,
  y_scroller_grabbed_at: Option<f32>,
  offset_x: Offset,
  x_scroller_grabbed_at: Option<f32>,
  keyboard_modifiers: keyboard::Modifiers,
  last_notified: Option<Viewport>,
}

impl Default for State {
  fn default() -> Self {
    Self {
      scroll_area_touched_at: None,
      offset_y: Offset::Absolute(0),
      y_scroller_grabbed_at: None,
      offset_x: Offset::Absolute(0),
      x_scroller_grabbed_at: None,
      keyboard_modifiers: keyboard::Modifiers::default(),
      last_notified: None,
    }
  }
}

// The scrollable trait wants f32 offsets.
/*impl operation::Scrollable for State {
    fn snap_to(&mut self, offset: RelativeOffset) {
        State::snap_to(self, offset);
    }

    fn scroll_to(&mut self, offset: AbsoluteOffset<u64>) {
        State::scroll_to(self, offset)
    }
}*/

#[derive(Debug, Clone, Copy)]
enum Offset {
  Absolute(u64),
  Relative(f32),
}

impl Offset {
  fn absolute(self, viewport: u64, content: u64) -> u64 {
    match self {
      Offset::Absolute(absolute) => {
        absolute.min(content.checked_sub(viewport).unwrap_or(0))
      }
      Offset::Relative(percentage) => {
        (content.checked_sub(viewport).unwrap_or(0) as f32 * percentage).max(0.0) as u64
      }
    }
  }
}

/// The current [`Viewport`] of the [`Scrollable`].
#[derive(Debug, Clone, Copy)]
pub struct Viewport {
  offset_x: Offset,
  offset_y: Offset,
  pub bounds: Rectangle,
  pub content_bounds: Rectangle<u64>,
}

impl Viewport {
  /// Returns the [`AbsoluteOffset`] of the current [`Viewport`].
  pub fn absolute_offset(&self) -> AbsoluteOffset<u64> {
    let x = self
      .offset_x
      .absolute(self.bounds.width as u64, self.content_bounds.width);
    let y = self
      .offset_y
      .absolute(self.bounds.height as u64, self.content_bounds.height);

    AbsoluteOffset::<u64> { x, y }
  }

  /// Returns the [`RelativeOffset`] of the current [`Viewport`].
  pub fn relative_offset(&self) -> RelativeOffset {
    let AbsoluteOffset { x, y } = self.absolute_offset();

    let x = x as f32 / (self.content_bounds.width.checked_sub(self.bounds.width as u64).unwrap_or(0)) as f32;
    let y = y as f32 / (self.content_bounds.height.checked_sub(self.bounds.height as u64).unwrap_or(0)) as f32;

    RelativeOffset { x, y }
  }
}

impl State {
  /// Creates a new [`State`] with the scrollbar(s) at the beginning.
  pub fn new() -> Self {
    State::default()
  }

  /// Apply a scrolling offset to the current [`State`], given the bounds of
  /// the [`Scrollable`] and its contents.
  pub fn scroll(
    &mut self,
    delta: Vector<f32>,
    bounds: Rectangle,
    content_bounds: Rectangle<u64>,
  ) {
    if (bounds.height as u64) < content_bounds.height {
      let max_offset_y = content_bounds.height - bounds.height as u64;
      let offset_y = self.offset_y.absolute(bounds.height as u64, content_bounds.height);
      self.offset_y = Offset::Absolute((if delta.y < 0.0 {
        offset_y + ((-delta.y) as u64)
      } else {
        offset_y.checked_sub(delta.y as u64).unwrap_or(0)
      }).clamp(0, max_offset_y))
    }

    if (bounds.width as u64) < content_bounds.width {
      let max_offset_x = content_bounds.width - bounds.width as u64;
      let offset_x = self.offset_x.absolute(bounds.width as u64, content_bounds.width);
      self.offset_x = Offset::Absolute(
        (if delta.x < 0.0 {
          offset_x + ((-delta.x) as u64)
        } else {
          offset_x.checked_sub(delta.x as u64).unwrap_or(0)
        })
          .clamp(0, max_offset_x),
      )
    }
  }

  /// Scrolls the [`Scrollable`] to a relative amount along the y axis.
  ///
  /// `0` represents scrollbar at the beginning, while `1` represents scrollbar at
  /// the end.
  pub fn scroll_y_to(
    &mut self,
    percentage: f32,
    bounds: Rectangle,
    content_bounds: Rectangle<u64>,
  ) {
    self.offset_y = Offset::Relative(percentage.clamp(0.0, 1.0));
    self.unsnap(bounds, content_bounds);
  }

  /// Scrolls the [`Scrollable`] to a relative amount along the x axis.
  ///
  /// `0` represents scrollbar at the beginning, while `1` represents scrollbar at
  /// the end.
  pub fn scroll_x_to(
    &mut self,
    percentage: f32,
    bounds: Rectangle,
    content_bounds: Rectangle<u64>,
  ) {
    self.offset_x = Offset::Relative(percentage.clamp(0.0, 1.0));
    self.unsnap(bounds, content_bounds);
  }

  /// Snaps the scroll position to a [`RelativeOffset`].
  pub fn snap_to(&mut self, offset: RelativeOffset) {
    self.offset_x = Offset::Relative(offset.x.clamp(0.0, 1.0));
    self.offset_y = Offset::Relative(offset.y.clamp(0.0, 1.0));
  }

  /// Scroll to the provided [`AbsoluteOffset`].
  pub fn scroll_to(&mut self, offset: AbsoluteOffset<u64>) {
    self.offset_x = Offset::Absolute(offset.x.max(0));
    self.offset_y = Offset::Absolute(offset.y.max(0));
  }

  /// Unsnaps the current scroll position, if snapped, given the bounds of the
  /// [`Scrollable`] and its contents.
  pub fn unsnap(&mut self, bounds: Rectangle, content_bounds: Rectangle<u64>,
  ) {
    self.offset_x = Offset::Absolute(
      self.offset_x.absolute(bounds.width as u64, content_bounds.width),
    );
    self.offset_y = Offset::Absolute(
      self.offset_y.absolute(bounds.height as u64, content_bounds.height),
    );
  }

  /// Returns the scrolling offset of the [`State`], given the bounds of the
  /// [`Scrollable`] and its contents.
  pub fn offset(
    &self,
    bounds: Rectangle,
    content_bounds: Rectangle<u64>,
  ) -> Vector<u64> {
    Vector::new(
      self.offset_x.absolute(bounds.width as u64, content_bounds.width as u64),
      self.offset_y.absolute(bounds.height as u64, content_bounds.height as u64),
    )
  }

  /// Returns whether any scroller is currently grabbed or not.
  pub fn scrollers_grabbed(&self) -> bool {
    self.x_scroller_grabbed_at.is_some()
      || self.y_scroller_grabbed_at.is_some()
  }
}

#[derive(Debug)]
/// State of both [`Scrollbar`]s.
struct Scrollbars {
  y: Option<internals::Scrollbar>,
  x: Option<internals::Scrollbar>,
}

impl Scrollbars {
  /// Create y and/or x scrollbar(s) if content is overflowing the [`Scrollable`] bounds.
  fn new(
    state: &State,
    vertical: &Properties,
    horizontal: Option<&Properties>,
    bounds: Rectangle,
    content_bounds: Rectangle<u64>,
  ) -> Self {
    let offset = state.offset(bounds, content_bounds);

    let show_scrollbar_x = horizontal.and_then(|h| {
      if content_bounds.width > bounds.width as u64 {
        Some(h)
      } else {
        None
      }
    });

    let y_scrollbar = if content_bounds.height > bounds.height as u64 {
      let Properties {
        width,
        margin,
        scroller_width,
      } = *vertical;

      // Adjust the height of the vertical scrollbar if the horizontal scrollbar
      // is present
      let x_scrollbar_height = show_scrollbar_x
        .map_or(0.0, |h| h.width.max(h.scroller_width) + h.margin);

      let total_scrollbar_width =
        width.max(scroller_width) + 2.0 * margin;

      // Total bounds of the scrollbar + margin + scroller width
      let total_scrollbar_bounds = Rectangle {
        x: bounds.x + bounds.width - total_scrollbar_width,
        y: bounds.y,
        width: total_scrollbar_width,
        height: (bounds.height - x_scrollbar_height).max(0.0),
      };

      // Bounds of just the scrollbar
      let scrollbar_bounds = Rectangle {
        x: bounds.x + bounds.width
          - total_scrollbar_width / 2.0
          - width / 2.0,
        y: bounds.y,
        width,
        height: (bounds.height - x_scrollbar_height).max(0.0),
      };

      let ratio = bounds.height / content_bounds.height as f32;
      // min height for easier grabbing with super tall content
      let scroller_height = (bounds.height * ratio).max(2.0);
      let scroller_offset = offset.y as f32 * ratio;

      let scroller_bounds = Rectangle {
        x: bounds.x + bounds.width
          - total_scrollbar_width / 2.0
          - scroller_width / 2.0,
        y: (scrollbar_bounds.y + scroller_offset - x_scrollbar_height)
          .max(0.0),
        width: scroller_width,
        height: scroller_height,
      };

      Some(internals::Scrollbar {
        total_bounds: total_scrollbar_bounds,
        bounds: scrollbar_bounds,
        scroller: internals::Scroller {
          bounds: scroller_bounds,
        },
      })
    } else {
      None
    };

    let x_scrollbar = if let Some(horizontal) = show_scrollbar_x {
      let Properties {
        width,
        margin,
        scroller_width,
      } = *horizontal;

      // Need to adjust the width of the horizontal scrollbar if the vertical scrollbar
      // is present
      let scrollbar_y_width = y_scrollbar.map_or(0.0, |_| {
        vertical.width.max(vertical.scroller_width) + vertical.margin
      });

      let total_scrollbar_height =
        width.max(scroller_width) + 2.0 * margin;

      // Total bounds of the scrollbar + margin + scroller width
      let total_scrollbar_bounds = Rectangle {
        x: bounds.x,
        y: bounds.y + bounds.height - total_scrollbar_height,
        width: (bounds.width - scrollbar_y_width).max(0.0),
        height: total_scrollbar_height,
      };

      // Bounds of just the scrollbar
      let scrollbar_bounds = Rectangle {
        x: bounds.x,
        y: bounds.y + bounds.height
          - total_scrollbar_height / 2.0
          - width / 2.0,
        width: (bounds.width - scrollbar_y_width).max(0.0),
        height: width,
      };

      let ratio = bounds.width / content_bounds.width as f32;
      // min width for easier grabbing with extra wide content
      let scroller_length = (bounds.width * ratio).max(2.0);
      let scroller_offset = offset.x as f32 * ratio;

      let scroller_bounds = Rectangle {
        x: (scrollbar_bounds.x + scroller_offset - scrollbar_y_width)
          .max(0.0),
        y: bounds.y + bounds.height
          - total_scrollbar_height / 2.0
          - scroller_width / 2.0,
        width: scroller_length,
        height: scroller_width,
      };

      Some(internals::Scrollbar {
        total_bounds: total_scrollbar_bounds,
        bounds: scrollbar_bounds,
        scroller: internals::Scroller {
          bounds: scroller_bounds,
        },
      })
    } else {
      None
    };

    Self {
      y: y_scrollbar,
      x: x_scrollbar,
    }
  }

  fn is_mouse_over(&self, cursor_position: Point) -> (bool, bool) {
    (
      self.y
        .as_ref()
        .map(|scrollbar| scrollbar.is_mouse_over(cursor_position))
        .unwrap_or(false),
      self.x
        .as_ref()
        .map(|scrollbar| scrollbar.is_mouse_over(cursor_position))
        .unwrap_or(false),
    )
  }

  fn grab_y_scroller(&self, cursor_position: Point) -> Option<f32> {
    self.y.and_then(|scrollbar| {
      if scrollbar.total_bounds.contains(cursor_position) {
        Some(if scrollbar.scroller.bounds.contains(cursor_position) {
          (cursor_position.y - scrollbar.scroller.bounds.y)
            / scrollbar.scroller.bounds.height
        } else {
          0.5
        })
      } else {
        None
      }
    })
  }

  fn grab_x_scroller(&self, cursor_position: Point) -> Option<f32> {
    self.x.and_then(|scrollbar| {
      if scrollbar.total_bounds.contains(cursor_position) {
        Some(if scrollbar.scroller.bounds.contains(cursor_position) {
          (cursor_position.x - scrollbar.scroller.bounds.x)
            / scrollbar.scroller.bounds.width
        } else {
          0.5
        })
      } else {
        None
      }
    })
  }

  fn active(&self) -> bool {
    self.y.is_some() || self.x.is_some()
  }
}

pub(super) mod internals {
  use iced::{Point, Rectangle};

  /// The scrollbar of a [`Scrollable`].
  #[derive(Debug, Copy, Clone)]
  pub struct Scrollbar {
    /// The total bounds of the [`Scrollbar`], including the scrollbar, the scroller,
    /// and the scrollbar margin.
    pub total_bounds: Rectangle,

    /// The bounds of just the [`Scrollbar`].
    pub bounds: Rectangle,

    /// The state of this scrollbar's [`Scroller`].
    pub scroller: Scroller,
  }

  impl Scrollbar {
    /// Returns whether the mouse is over the scrollbar or not.
    pub fn is_mouse_over(&self, cursor_position: Point) -> bool {
      self.total_bounds.contains(cursor_position)
    }

    /// Returns the y-axis scrolled percentage from the cursor position.
    pub fn scroll_percentage_y(
      &self,
      grabbed_at: f32,
      cursor_position: Point,
    ) -> f32 {
      if cursor_position.x < 0.0 && cursor_position.y < 0.0 {
        // cursor position is unavailable! Set to either end or beginning of scrollbar depending
        // on where the thumb currently is in the track
        (self.scroller.bounds.y / self.total_bounds.height).round()
      } else {
        (cursor_position.y
          - self.bounds.y
          - self.scroller.bounds.height * grabbed_at)
          / (self.bounds.height - self.scroller.bounds.height)
      }
    }

    /// Returns the x-axis scrolled percentage from the cursor position.
    pub fn scroll_percentage_x(
      &self,
      grabbed_at: f32,
      cursor_position: Point,
    ) -> f32 {
      if cursor_position.x < 0.0 && cursor_position.y < 0.0 {
        (self.scroller.bounds.x / self.total_bounds.width).round()
      } else {
        (cursor_position.x
          - self.bounds.x
          - self.scroller.bounds.width * grabbed_at)
          / (self.bounds.width - self.scroller.bounds.width)
      }
    }
  }

  /// The handle of a [`Scrollbar`].
  #[derive(Debug, Clone, Copy)]
  pub struct Scroller {
    /// The bounds of the [`Scroller`].
    pub bounds: Rectangle,
  }
}
