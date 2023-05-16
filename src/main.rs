mod fixed_scrollable;

use iced::widget::container::Appearance;
use fixed_scrollable::Viewport;
use iced::widget::{Column, Container, TextInput};
use iced::{executor, Application, Color, Command, Element, Length, Settings, Theme};
use crate::fixed_scrollable::FixedScrollable;

struct CustomTheme {}

impl iced::widget::container::StyleSheet for CustomTheme {
  type Style = Theme;

  fn appearance(&self, _style: &Self::Style) -> Appearance {
    Appearance {
      text_color: None,
      background: None,
      border_radius: 0.0,
      border_width: 1.0,
      border_color: Color::new(1.0, 0.0, 0.0, 1.0),
    }
  }
}

pub fn main() -> iced::Result {
  HelloWorld::run(Settings::default())
}

#[derive(Debug, Clone)]
enum Message {
  Scrolled(Viewport, Option<(usize, usize)>),
  TextChanged(String, usize),
}

struct HelloWorld {
  data: Vec<String>,
  scroll_offset_y: f32,
  start_index: usize,
  end_index: usize,
}

const ROWS: usize = 100_000_000;
const HEIGHT: f32 = 100.0;

impl<'a> Application for HelloWorld {
  type Executor = executor::Default;
  type Message = Message;
  type Theme = Theme;
  type Flags = ();

  fn new(_flags: Self::Flags) -> (Self, Command<Self::Message>) {
    let mut random_strings = Vec::<String>::with_capacity(ROWS);

    for i in 0..ROWS {
      random_strings.push((1 + i).to_string());
    }
    println!("Done generating strings");

    (
      HelloWorld {
        data: random_strings.clone(),
        scroll_offset_y: 0.0,
        start_index: 0,
        end_index: 50,
      },
      Command::none(),
    )
  }

  fn title(&self) -> String {
    String::from("Fixed Scrollable")
  }

  fn update(&mut self, message: Message) -> Command<Self::Message> {
    match message {
      Message::Scrolled(viewport, scroll_offsets) => {
        self.scroll_offset_y = viewport.absolute_offset().y as f32;
        match scroll_offsets {
          None => {}
          Some(scroll_offsets) => {
            self.start_index = scroll_offsets.0;
            self.end_index = scroll_offsets.1;
          }
        }
      }
      Message::TextChanged(str, index) => {
        self.data[self.start_index + index] = str;
      }
    }

    Command::none()
  }

  fn view(&self) -> Element<Message> {
    let data_view = Column::with_children(
      self.data[(self.start_index as usize)..=(self.end_index as usize)]
        .iter()
        .enumerate()
        .map(|(index, str)| {
          Container::new(TextInput::new("", str).on_input(move |new_str| {
            Message::TextChanged(new_str, index)
          }))
            .width(Length::Fixed(2500.0))
            .height(Length::Fixed(HEIGHT))
            .style(iced::theme::Container::Custom(Box::new(CustomTheme {})))
            .into()
        })
        .collect(),
    ).width(1200);

    let scrollable = FixedScrollable::new(HEIGHT as u64, self.data.len(), Column::with_children(vec![
      data_view.into(),
    ]))
      .width(Length::Fill)
      .height(Length::Fill)
      .on_scroll(Message::Scrolled);

    scrollable.into()
  }

  fn theme(&self) -> Theme {
    Theme::default()
  }
}
