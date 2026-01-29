# TDA Wildfire Simulation Project
# This project has been refactored and updated into a modular structure to improve code organization, maintainability, and readability.
# File Structure
app.R - Main application entry point. Sources all modules and launches the Shiny app.

## Core Components

ui.R - Complete user interface definition including control panels, map, and results dashboard
server.R - Main server function that coordinates all reactive logic and event handling

## Module Files (modules/ directory)

### initialization.R - System Initialization and Management

Initializes server state and reactive values
Checks system readiness
Sets up error handlers
Processes initial wildfire data

### ui_management.R - UI State Management and Visibility Control

Manages UI visibility and transitions
Updates conditional panels
Resets UI state
Validates UI readiness


### reactive_handlers.R - Reactive Value Management

Sets up cascade reactives
Manages fire data reactives
Handles filter reactives
Manages TDA reactives
Sets up map reactives

### input_validation.R - Input Validation and Safety

Safe input access with fallbacks
Input existence validation
Input readiness checking
Input retrieval with fallbacks


### ui_renderers.R - Dynamic UI Rendering

Renders state selection UI
Renders intensity selection UI
Renders filter options UI
Renders fire selection UI
Renders analysis options UI

### event_handlers.R - Event Handler Functions

State selection handling
Intensity selection handling
Filter change handling
Multiple fire selection handling
Cascade execution handling
TDA analysis handling


### observers.R - Observer Management

Initialization observers
Data observers
UI update observers
Map observers
Analysis observers


### map_management.R - Map Management and Leaflet Functions

Base map initialization
Map updates and layer management
Legend creation and updates
Fire perimeter visualization
Grid network display

### data_processing.R - Data Processing and Analysis Helpers

Wildfire data processing
Bus proximity calculations
Cascade result summarization
TDA result summarization


### outputs_and_analysis.R - Output Rendering and Analysis

TDA comparison plots
Cascade progression plots
System status outputs
Resilience plots
Fire timeline plots
Download handlers

## Supporting Files
global.R - Global configuration, data loading, and utilities
AttackAndCascade.R - Cascade failure simulation logic
TopologicalDataWorkflowWF.R - TDA analysis workflow

## How to Run
Option 1: Run the new modular version
R# Make sure all dependencies are installed
shiny::runApp("app.R")

Option 2: Continue using the original file
The original WildfireServer.R file remains unchanged and can still be run:
Rshiny::runApp("WildfireServer.R")
Benefits of Modular Structure

Improved Organization - Related functions are grouped together logically
Easier Maintenance - Bugs can be isolated to specific modules
Better Collaboration - Multiple developers can work on different modules
Code Reusability - Modules can be easily reused in other projects
Simpler Testing - Individual modules can be tested independently
Reduced Clutter - Each file has a clear, focused purpose

Module Dependencies
The modules have the following dependencies:

All modules depend on the base Shiny libraries loaded in app.R
ui_renderers.R uses functions from data_processing.R
event_handlers.R uses functions from map_management.R and observers.R
outputs_and_analysis.R uses functions from data_processing.R
server.R orchestrates all modules together

Making Changes

When making changes to the application:

Identify the right module - Find which module contains the function you need to modify
Edit the module - Make your changes in the appropriate module file
Test thoroughly - Ensure your changes don't break functionality in dependent modules
Update documentation - Add comments explaining your changes

Migrating Custom Changes
If you have custom changes in the original WildfireServer.R:

Identify which section your changes are in (based on the section headers)
Find the corresponding module file
Apply your changes to that module
Test the application using app.R

## Notes
## The original WildfireServer.R file is preserved and fully functional, but i would NOT use it, as its incredibly cluttered with multiple functions and features jammed into one file, making it 4000+ lines.
 - All functionality is maintained that WildfireServer.R had the modular files have, 
 - The modular structure uses identical logic, just reorganized for clarity, but both versions can coexist in the same directory
