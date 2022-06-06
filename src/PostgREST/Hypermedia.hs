{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Hypermedia where

import PostgREST.AppState
import PostgREST.Config
import Protolude
import qualified Network.Wai as Wai
-- import qualified Network.HTTP.Types as HTTP
import qualified Okapi
import System.Directory (listDirectory, makeRelativeToCurrentDirectory, removeFile) -- , doesFileExist), {- getCurrentDirectory, -} makeRelativeToCurrentDirectory)
import qualified Data.Text as T
import Data.Aeson (FromJSON)
import qualified Text.InterpolatedString.Perl6 as Perl
import qualified Data.ByteString.Lazy as LBS
-- import qualified System.Posix.IO as Posix
import qualified System.Posix as Posix

type Okapi a = Okapi.OkapiT IO a

app :: AppState -> AppConfig -> Wai.Application
app _ _ = Okapi.makeOkapiApp identity hypermedia

hypermedia :: Okapi Okapi.Result
hypermedia =
  editor <|>
  home <|>
  dashboard <|> -- For checking health of hypermedia tool
  templates <|> -- For managing HTML templates
  database <|> -- For managing database
  createTemplate <|>
  create <|>
  editTemplate <|>
  save <|>
  deleteTemplate

editor :: Okapi Okapi.Result
editor = do
  Okapi.get
  Okapi.seg "editor.js"
  scriptPath <- liftIO $ makeRelativeToCurrentDirectory "postgrest/hypermedia/editor.bundle.js"
  Okapi.okFile [("Content-Type", "application/json")] scriptPath

home :: Okapi Okapi.Result
home = do
  Okapi.get
  let
    homePage :: LBS.ByteString =
      [Perl.qq|
        <!DOCTYPE html>
        <html lang="en">
          <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <meta http-equiv="X-UA-Compatible" content="ie=edge">
            <title>Hypermedia Tool</title>
            <script defer src="/editor.js"></script>
            <script defer src="https://unpkg.com/htmx.org@1.7.0" integrity="sha384-EzBXYPt0/T6gxNp0nuPtLkmRpmDBbjg6WmCUZRLXBBwYYmwAUxzlSGej0ARHX0Bo" crossorigin="anonymous"></script>
            <script defer src="https://unpkg.com/htmx.org/dist/ext/json-enc.js"></script>
            <script defer src="https://unpkg.com/hyperscript.org@0.9.5"></script>
            <script src="https://cdn.tailwindcss.com"></script>
          </head>
          <body>
            <!-- This example requires Tailwind CSS v2.0+ -->
            <!--
              This example requires updating your template:

              ```
              <html class="h-full bg-gray-100">
              <body class="h-full">
              ```
            -->
            <div>
              <!-- Off-canvas menu for mobile, show/hide based on off-canvas menu state. -->
              <div class="relative z-40 md:hidden" role="dialog" aria-modal="true">
                <!--
                  Off-canvas menu backdrop, show/hide based on off-canvas menu state.

                  Entering: "transition-opacity ease-linear duration-300"
                    From: "opacity-0"
                    To: "opacity-100"
                  Leaving: "transition-opacity ease-linear duration-300"
                    From: "opacity-100"
                    To: "opacity-0"
                -->
                <div class="fixed inset-0 bg-gray-600 bg-opacity-75"></div>

                <div class="fixed inset-0 flex z-40">
                  <!--
                    Off-canvas menu, show/hide based on off-canvas menu state.

                    Entering: "transition ease-in-out duration-300 transform"
                      From: "-translate-x-full"
                      To: "translate-x-0"
                    Leaving: "transition ease-in-out duration-300 transform"
                      From: "translate-x-0"
                      To: "-translate-x-full"
                  -->
                  <div class="relative flex-1 flex flex-col max-w-xs w-full bg-gray-800">
                    <!--
                      Close button, show/hide based on off-canvas menu state.

                      Entering: "ease-in-out duration-300"
                        From: "opacity-0"
                        To: "opacity-100"
                      Leaving: "ease-in-out duration-300"
                        From: "opacity-100"
                        To: "opacity-0"
                    -->
                    <div class="absolute top-0 right-0 -mr-12 pt-2">
                      <button type="button" class="ml-1 flex items-center justify-center h-10 w-10 rounded-full focus:outline-none focus:ring-2 focus:ring-inset focus:ring-white">
                        <span class="sr-only">Close sidebar</span>
                        <!-- Heroicon name: outline/x -->
                        <svg class="h-6 w-6 text-white" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                          <path stroke-linecap="round" stroke-linejoin="round" d="M6 18L18 6M6 6l12 12" />
                        </svg>
                      </button>
                    </div>

                    <div class="flex-1 h-0 pt-5 pb-4 overflow-y-auto">
                      <div class="flex-shrink-0 flex items-center px-4">
                        <img class="h-8 w-auto" src="https://tailwindui.com/img/logos/workflow-logo-indigo-500-mark-white-text.svg" alt="Workflow">
                      </div>
                      <nav class="mt-5 px-2 space-y-1">
                        <!-- Current: "bg-gray-900 text-white", Default: "text-gray-300 hover:bg-gray-700 hover:text-white" -->
                        <a href="#" class="bg-gray-900 text-white group flex items-center px-2 py-2 text-base font-medium rounded-md">
                          <!--
                            Heroicon name: outline/home

                            Current: "text-gray-300", Default: "text-gray-400 group-hover:text-gray-300"
                          -->
                          <svg class="text-gray-300 mr-4 flex-shrink-0 h-6 w-6" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                            <path stroke-linecap="round" stroke-linejoin="round" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6" />
                          </svg>
                          Dashboard
                        </a>

                        <a href="#" class="text-gray-300 hover:bg-gray-700 hover:text-white group flex items-center px-2 py-2 text-base font-medium rounded-md">
                          <!-- Heroicon name: outline/users -->
                          <svg class="text-gray-400 group-hover:text-gray-300 mr-4 flex-shrink-0 h-6 w-6" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                            <path stroke-linecap="round" stroke-linejoin="round" d="M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z" />
                          </svg>
                          Team
                        </a>

                        <a href="#" class="text-gray-300 hover:bg-gray-700 hover:text-white group flex items-center px-2 py-2 text-base font-medium rounded-md">
                          <!-- Heroicon name: outline/folder -->
                          <svg class="text-gray-400 group-hover:text-gray-300 mr-4 flex-shrink-0 h-6 w-6" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                            <path stroke-linecap="round" stroke-linejoin="round" d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z" />
                          </svg>
                          Projects
                        </a>

                        <a href="#" class="text-gray-300 hover:bg-gray-700 hover:text-white group flex items-center px-2 py-2 text-base font-medium rounded-md">
                          <!-- Heroicon name: outline/calendar -->
                          <svg class="text-gray-400 group-hover:text-gray-300 mr-4 flex-shrink-0 h-6 w-6" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                            <path stroke-linecap="round" stroke-linejoin="round" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z" />
                          </svg>
                          Calendar
                        </a>

                        <a href="#" class="text-gray-300 hover:bg-gray-700 hover:text-white group flex items-center px-2 py-2 text-base font-medium rounded-md">
                          <!-- Heroicon name: outline/inbox -->
                          <svg class="text-gray-400 group-hover:text-gray-300 mr-4 flex-shrink-0 h-6 w-6" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                            <path stroke-linecap="round" stroke-linejoin="round" d="M20 13V6a2 2 0 00-2-2H6a2 2 0 00-2 2v7m16 0v5a2 2 0 01-2 2H6a2 2 0 01-2-2v-5m16 0h-2.586a1 1 0 00-.707.293l-2.414 2.414a1 1 0 01-.707.293h-3.172a1 1 0 01-.707-.293l-2.414-2.414A1 1 0 006.586 13H4" />
                          </svg>
                          Documents
                        </a>

                        <a href="#" class="text-gray-300 hover:bg-gray-700 hover:text-white group flex items-center px-2 py-2 text-base font-medium rounded-md">
                          <!-- Heroicon name: outline/chart-bar -->
                          <svg class="text-gray-400 group-hover:text-gray-300 mr-4 flex-shrink-0 h-6 w-6" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                            <path stroke-linecap="round" stroke-linejoin="round" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
                          </svg>
                          Reports
                        </a>
                      </nav>
                    </div>
                    <div class="flex-shrink-0 flex bg-gray-700 p-4">
                      <a href="#" class="flex-shrink-0 group block">
                        <div class="flex items-center">
                          <div>
                            <img class="inline-block h-10 w-10 rounded-full" src="https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80" alt="">
                          </div>
                          <div class="ml-3">
                            <p class="text-base font-medium text-white">Tom Cook</p>
                            <p class="text-sm font-medium text-gray-400 group-hover:text-gray-300">View profile</p>
                          </div>
                        </div>
                      </a>
                    </div>
                  </div>

                  <div class="flex-shrink-0 w-14">
                    <!-- Force sidebar to shrink to fit close icon -->
                  </div>
                </div>
              </div>

              <!-- Static sidebar for desktop -->
              <div class="hidden md:flex md:w-64 md:flex-col md:fixed md:inset-y-0">
                <!-- Sidebar component, swap this element with another sidebar if you like -->
                <div class="flex-1 flex flex-col min-h-0 bg-gray-800">
                  <div class="flex-1 flex flex-col pt-5 pb-4 overflow-y-auto">
                    <div class="flex items-center flex-shrink-0 px-4">
                      <img class="h-8 w-auto" src="https://tailwindui.com/img/logos/workflow-logo-indigo-500-mark-white-text.svg" alt="Workflow">
                    </div>
                    <nav id="sidenav" class="mt-5 flex-1 px-2 space-y-1">
                      <!-- Current: "bg-gray-900 text-white", Default: "text-gray-300 hover:bg-gray-700 hover:text-white" -->
                      <a
                        _="on click add .bg-gray-900 .text-white end
                           on click from the next <a/> remove .bg-gray-900 .text-white from me then add .text-gray-300 .hover:bg-gray-700 .hover:text-white to me end
                           on click from the last <a/> in #sidenav remove .bg-gray-900 .text-white from me then add .text-gray-300 .hover:bg-gray-700 .hover:text-white to me end"
                        hx-get="/dashboard"
                        hx-target="#screen"
                        class="bg-gray-900 text-white group flex items-center px-2 py-2 text-sm font-medium rounded-md"
                      >
                        <!--
                          Heroicon name: outline/home

                          Current: "text-gray-300", Default: "text-gray-400 group-hover:text-gray-300"
                        -->
                        <svg class="text-gray-300 mr-3 flex-shrink-0 h-6 w-6" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                          <path stroke-linecap="round" stroke-linejoin="round" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6" />
                        </svg>
                        Dashboard
                      </a>

                      <a
                        _="on click add .bg-gray-900 .text-white end
                           on click from the previous <a/> remove .bg-gray-900 .text-white from me then add .text-gray-300 .hover:bg-gray-700 .hover:text-white to me end
                           on click from the next <a/> remove .bg-gray-900 .text-white from me then add .text-gray-300 .hover:bg-gray-700 .hover:text-white to me end"
                        hx-get="/templates" hx-target="#screen" class="text-gray-300 hover:bg-gray-700 hover:text-white group flex items-center px-2 py-2 text-sm font-medium rounded-md">
                        <!-- Heroicon name: outline/users -->
                        <svg class="text-gray-400 group-hover:text-gray-300 mr-3 flex-shrink-0 h-6 w-6" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                          <path stroke-linecap="round" stroke-linejoin="round" d="M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z" />
                        </svg>
                        Templates
                      </a>

                      <a
                        _="on click add .bg-gray-900 .text-white end
                           on click from the previous <a/> remove .bg-gray-900 .text-white from me then add .text-gray-300 .hover:bg-gray-700 .hover:text-white to me end
                           on click from the first <a/> in #sidenav remove .bg-gray-900 .text-white from me then add .text-gray-300 .hover:bg-gray-700 .hover:text-white to me end"
                        hx-get="/database" hx-target="#screen" class="text-gray-300 hover:bg-gray-700 hover:text-white group flex items-center px-2 py-2 text-sm font-medium rounded-md">
                        <!-- Heroicon name: outline/folder -->
                        <svg class="text-gray-400 group-hover:text-gray-300 mr-3 flex-shrink-0 h-6 w-6" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                          <path stroke-linecap="round" stroke-linejoin="round" d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z" />
                        </svg>
                        Database
                      </a>
                    </nav>
                  </div>
                  <div class="flex-shrink-0 flex bg-gray-700 p-4">
                    <a href="#" class="flex-shrink-0 w-full group block">
                      <div class="flex items-center">
                        <div>
                          <img class="inline-block h-9 w-9 rounded-full" src="https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80" alt="">
                        </div>
                        <div class="ml-3">
                          <p class="text-sm font-medium text-white">Tom Cook</p>
                          <p class="text-xs font-medium text-gray-300 group-hover:text-gray-200">View profile</p>
                        </div>
                      </div>
                    </a>
                  </div>
                </div>
              </div>
              <div class="md:pl-64 flex flex-col flex-1">
                <div class="sticky top-0 z-10 md:hidden pl-1 pt-1 sm:pl-3 sm:pt-3 bg-gray-100">
                  <button type="button" class="-ml-0.5 -mt-0.5 h-12 w-12 inline-flex items-center justify-center rounded-md text-gray-500 hover:text-gray-900 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500">
                    <span class="sr-only">Open sidebar</span>
                    <!-- Heroicon name: outline/menu -->
                    <svg class="h-6 w-6" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" aria-hidden="true">
                      <path stroke-linecap="round" stroke-linejoin="round" d="M4 6h16M4 12h16M4 18h16" />
                    </svg>
                  </button>
                </div>
                <main id="screen" class="flex-1">
                  <div class="py-6">
                    <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
                      <h1 class="text-2xl font-semibold text-gray-900">Dashboard</h1>
                    </div>
                    <div class="max-w-7xl mx-auto px-4 sm:px-6 md:px-8">
                      <!-- Replace with your content -->
                      <div class="py-4">
                        <div class="border-4 border-dashed border-gray-200 rounded-lg h-96">
                          <h1 class="text-green-400 text-xl">
                            Hypermedia Tool Online
                          </h1>
                        </div>
                      </div>
                      <!-- /End replace -->
                    </div>
                  </div>
                </main>
              </div>
            </div>
          </body>
        </html>
      |]
  Okapi.okHTML
    []
    homePage

dashboard :: Okapi Okapi.Result
dashboard = do
  Okapi.get
  Okapi.seg "dashboard"
  let
    screen :: LBS.ByteString =
      [Perl.q|
        <div class="py-6">
          <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
            <h1 class="text-2xl font-semibold text-gray-900">Dashboard</h1>
          </div>
          <div class="max-w-7xl mx-auto px-4 sm:px-6 md:px-8">
            <!-- Replace with your content -->
            <div class="py-4">
              <div class="border-4 border-dashed border-gray-200 rounded-lg h-96">
                <h1 class="text-green-400 text-xl">
                  Hypermedia Tool Online
                </h1>
              </div>
            </div>
            <!-- /End replace -->
          </div>
        </div>
      |]
    in
      Okapi.okHTML [] screen

templates :: Okapi Okapi.Result
templates = do
  Okapi.get
  Okapi.seg "templates"
  templateScreen

templateScreen :: Okapi Okapi.Result
templateScreen = do
  templateFilenames <- liftIO $ listDirectory "templates"
  let
    templateNames = map (takeWhile (/= '.')) templateFilenames
    templateList :: LBS.ByteString =
      foldl (<>) "" $
        map
          (\tn ->
            [Perl.qc|
              <li id="{tn}-item">
                <div class="px-4 py-4 sm:px-6 flex items-center justify-between">
                  <p class="text-lg font-medium text-indigo-600 truncate">{tn}</p>
                  <div class="flex items-center">
                    <button hx-get="/templates/{tn}" hx-target="#screen" type="button" class="inline-flex items-center px-4 py-2 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-green-600 hover:bg-green-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-green-500">
                      Edit
                    </button>
                    <button hx-delete="/templates/{tn}" hx-target="#{tn}-item" hx-swap="outerHTML" type="button" class="ml-4 inline-flex items-center px-4 py-2 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-red-600 hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500">
                      Delete
                    </button>
                  </div>
                </div>
              </li>
            |]
          )
          templateNames
    screen :: LBS.ByteString =
      [Perl.qq|
        <div class="py-6">
          <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
            <h1 class="text-2xl font-semibold text-gray-900">Templates</h1>
          </div>
          <div class="max-w-7xl mx-auto px-4 sm:px-6 md:px-8">
            <!-- Replace with your content -->
            <div class="py-4">
              <!-- This example requires Tailwind CSS v2.0+ -->
              <div class="bg-white shadow overflow-hidden sm:rounded-md">
                <ul role="list" class="divide-y divide-gray-200">
                  {templateList}
                  <li>
                    <div class="px-4 py-4 sm:px-6 flex items-center justify-between">
                      <button hx-get="/templates/create" hx-target="#screen" type="button" class="inline-flex items-center px-4 py-2 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-green-600 hover:bg-green-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-green-500">
                        Create New Template
                      </button>
                      <div>
                      </div>
                    </div>
                  </li>
                </ul>
              </div>
            </div>
            <!-- /End replace -->
          </div>
        </div>
      |]
    in
      Okapi.okHTML [] screen

-- <button hx-get="/templates/create" hx-target="#screen">
--   Add New
-- </button>

database :: Okapi Okapi.Result
database = do
  Okapi.get
  Okapi.seg "database"
  let
    screen :: LBS.ByteString =
      [Perl.qc|
        <div class="py-6">
          <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
            <h1 class="text-2xl font-semibold text-gray-900">Database</h1>
          </div>
          <div class="max-w-7xl mx-auto px-4 sm:px-6 md:px-8">
            <!-- Replace with your content -->
            <div class="py-4">
              <div class="border-4 border-dashed border-gray-200 rounded-lg h-96">
                <div id="editor"></div>
              </div>
            </div>
            <!-- /End replace -->
          </div>
        </div>
      |]
    in
      Okapi.okHTML [] screen

editTemplate :: Okapi Okapi.Result
editTemplate = do
  Okapi.get
  Okapi.seg "templates"
  templateName <- Okapi.segParam
  templatePath <- liftIO $ makeRelativeToCurrentDirectory ("templates/" <> templateName <> ".mustache")
  template <- liftIO $ readFile templatePath
  let
    screen =
      [Perl.qc|
        <div class="py-6">
          <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
            <h1 class="text-2xl font-semibold text-gray-900">Template Editor</h1>
          </div>
          <div class="max-w-7xl mx-auto px-4 sm:px-6 md:px-8">
            <!-- Replace with your content -->
            <div class="py-4">
              <div class="border-4 border-dashed border-gray-200 rounded-lg h-96">
                <form hx-put="/templates/{templateName}" hx-ext="json-enc" hx-params="*" hx-target="#screen">
                  <label for="editFormContent" class="block text-lg font-medium text-gray-700">{templateName}</label>
                  <textarea rows="4" name="editFormContent" id="editFormContent" class="shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md">{template}</textarea>
                  <button class="inline-flex items-center px-4 py-2 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-green-600 hover:bg-green-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-green-500">
                    Save
                  </button>
                </form>
                <button hx-get="/templates" hx-target="#screen" type="button" class="inline-flex items-center px-4 py-2 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-red-600 hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500">
                  Cancel
                </button>
              </div>
            </div>
            <!-- /End replace -->
          </div>
        </div>
      |]
    in Okapi.okHTML [] screen

deleteTemplate :: Okapi Okapi.Result
deleteTemplate = do
  Okapi.delete
  Okapi.seg "templates"
  templateName <- Okapi.segParam
  liftIO $ do
    templatePath <- makeRelativeToCurrentDirectory ("templates/" <> templateName <> ".mustache")
    removeFile templatePath
  Okapi.okHTML [] ""

createTemplate :: Okapi Okapi.Result
createTemplate = do
  Okapi.get
  Okapi.seg "templates"
  Okapi.seg "create"
  let
    screen =
      [Perl.qc|
        <div class="py-6">
          <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
            <h1 class="text-2xl font-semibold text-gray-900">Add New Template</h1>
          </div>
          <div class="max-w-7xl mx-auto px-4 sm:px-6 md:px-8">
            <!-- Replace with your content -->
            <div class="py-4">
              <div class="border-4 border-dashed border-gray-200 rounded-lg h-96">
                <form hx-post="/templates" hx-ext="json-enc" hx-params="*" hx-target="#screen">
                  <label for="newFormName" class="block text-lg font-medium text-gray-700">New Template Name</label>
                  <input name="newFormName" class="shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md"></input>
                  <label for="newFormContent" class="block text-lg font-medium text-gray-700">New Template Content</label>
                  <textarea rows="4" name="newFormContent" class="shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md">
                  </textarea>
                  <button class="inline-flex items-center px-4 py-2 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-green-600 hover:bg-green-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-green-500">
                    Create
                  </button>
                </form>
                <button hx-get="/templates" hx-target="#screen" type="button" class="inline-flex items-center px-4 py-2 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-red-600 hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500">
                  Cancel
                </button>
              </div>
            </div>
            <!-- /End replace -->
          </div>
        </div>
      |]
    in Okapi.okHTML [] screen

save :: Okapi Okapi.Result
save = do
  Okapi.put
  Okapi.seg "templates"
  templateName <- Okapi.segParam
  EditForm{..} <- Okapi.bodyJSON @EditForm
  liftIO $ do
    templatePath <- makeRelativeToCurrentDirectory $ "templates/" <> templateName <> ".mustache"
    writeFile templatePath editFormContent
  templateScreen

create :: Okapi Okapi.Result
create = do
  Okapi.post
  Okapi.seg "templates"
  NewForm{..} <- Okapi.bodyJSON @NewForm
  liftIO $ do
    templatePath <- liftIO $ makeRelativeToCurrentDirectory . T.unpack $ "templates/" <> newFormName <> ".mustache"
    fd <- liftIO $ Posix.createFile templatePath Posix.stdFileMode
    liftIO $ Posix.closeFd fd
    liftIO $ writeFile templatePath newFormContent
  templateScreen

data EditForm = EditForm
  { editFormContent :: Text
  } deriving (Eq, Show, Generic, FromJSON)

data NewForm = NewForm
  { newFormName :: Text
  , newFormContent :: Text
  } deriving (Eq, Show, Generic, FromJSON)

{-
templateByName :: Okapi Okapi.Result
templateByName = do
  Okapi.get
  Okapi.seg "templates"
  templateName <- Okapi.queryParam @FilePath "name"
  templatePath <- liftIO $ makeRelativeToCurrentDirectory ("templates/" <> templateName <> ".mustache")
  fileExists <- liftIO $ doesFileExist templatePath
  content <-
    if fileExists
      then do
        liftIO $ readFile templatePath
      else do
        pure ""
  Okapi.okLucid [] $ do
    input_
      [ id_ "template-name-input"
      , name_ "templateName"
      , value_ $ pack templateName
      ]
    textarea_
      [ id_ "template-content-input"
      , name_ "templateContent"
      , hxSwapOob_ "true"
      ]
      $ toHtml content
-}