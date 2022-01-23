:-include('play.pl').
:-include('board.pl').
:-include('bot.pl').
:-include('input.pl').
:-include('main.pl').
:-include('move.pl').
:-use_module(library(lists)).
:-use_module(library(between)).
:-use_module(library(random)).
:-use_module(library(system)).

play :- 
    chooseMode.