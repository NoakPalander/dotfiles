use hyprland::data::Client;
use hyprland::dispatch;
use hyprland::dispatch::{Direction, Dispatch, DispatchType, WindowSwitchDirection};
use hyprland::prelude::HyprDataActiveOptional;
use std::env;

fn move_focus(direction: Direction) -> hyprland::Result<()> {
    dispatch!(MoveFocus, direction)
}

fn move_group(direction: Direction) -> hyprland::Result<()> {
    let window_direction = match direction {
        Direction::Left => WindowSwitchDirection::Back,
        _ => WindowSwitchDirection::Forward
    };

    dispatch!(ChangeGroupActive, window_direction)
}

fn main() -> hyprland::Result<()> {
    let args = env::args().collect::<Vec<String>>();

    let direction = match args.get(1).unwrap().as_str() {
        "l" => Direction::Left,
        "r" => Direction::Right,
        "u" => Direction::Up,
        _ => Direction::Down,
    };

    let active_window = Client::get_active()?.unwrap();

    let grouped = active_window
        .grouped
        .into_iter()
        .map(|b| b.to_string())
        .collect::<Vec<String>>();

    let command = if grouped.len() == 0 {
        move_focus
    }
    else {
        let address = active_window.address.to_string();
        let index = grouped.iter().position(|a| *a == address).unwrap();

        if grouped.len() == 1 {
            move_focus
        }
        else if index + 1 == grouped.len() {
            match direction {
                Direction::Right => move_focus,
                _ => move_group
            }
        }
        else if index == 0 {
            match direction {
                Direction::Left => move_focus,
                _ => move_group
            }
        }
        else {
            move_group
        }
    };

    command(direction)
}
