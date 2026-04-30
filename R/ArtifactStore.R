# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @title Abstract Artifact Store
#'
#' @description
#' Abstract R6 interface for storing and retrieving analysis artifacts.
#' Subclass this to implement custom storage backends (e.g., S3, database).
#'
#' @export
ArtifactStore <- R6::R6Class(
  "ArtifactStore",
  public = list(
    #' @description Check if an artifact exists.
    #' @param key Character. The artifact key (relative path).
    #' @return Logical.
    exists = function(key) {
      stop("Abstract method: must be implemented by subclass")
    },

    #' @description Read an RDS artifact.
    #' @param key Character. The artifact key.
    #' @return The deserialized R object.
    readRDS = function(key) {
      stop("Abstract method: must be implemented by subclass")
    },

    #' @description Save an RDS artifact.
    #' @param object The R object to save.
    #' @param key Character. The artifact key.
    saveRDS = function(object, key) {
      stop("Abstract method: must be implemented by subclass")
    },

    #' @description Read an Andromeda (zip) artifact.
    #' @param key Character. The artifact key.
    #' @return An Andromeda object.
    readAndromeda = function(key) {
      stop("Abstract method: must be implemented by subclass")
    },

    #' @description Save an Andromeda (zip) artifact.
    #' @param object The Andromeda object to save.
    #' @param key Character. The artifact key.
    saveAndromeda = function(object, key) {
      stop("Abstract method: must be implemented by subclass")
    },

    #' @description List artifacts matching a prefix.
    #' @param prefix Character or NULL. Filter to keys starting with this prefix.
    #' @return Character vector of keys.
    listArtifacts = function(prefix = NULL) {
      stop("Abstract method: must be implemented by subclass")
    },

    #' @description Delete an artifact.
    #' @param key Character. The artifact key.
    delete = function(key) {
      stop("Abstract method: must be implemented by subclass")
    },

    #' @description Ensure a directory exists for a given key.
    #' @param key Character. The artifact key.
    ensureDir = function(key) {
      stop("Abstract method: must be implemented by subclass")
    }
  )
)

#' @title Local Filesystem Artifact Store
#'
#' @description
#' Implementation of [ArtifactStore] that reads and writes artifacts to a local
#' filesystem directory. This is the default storage backend.
#'
#' @export
LocalArtifactStore <- R6::R6Class(
  "LocalArtifactStore",
  inherit = ArtifactStore,
  public = list(
    #' @description Create a new local artifact store.
    #' @param basePath Character. The root directory for artifact storage.
    initialize = function(basePath) {
      private$basePath <- basePath
      if (!dir.exists(basePath)) {
        dir.create(basePath, recursive = TRUE)
      }
    },

    #' @description Check if an artifact exists.
    #' @param key Character. The artifact key (relative path).
    #' @return Logical.
    exists = function(key) {
      file.exists(file.path(private$basePath, key))
    },

    #' @description Read an RDS artifact.
    #' @param key Character. The artifact key.
    #' @return The deserialized R object.
    readRDS = function(key) {
      base::readRDS(file.path(private$basePath, key))
    },

    #' @description Save an RDS artifact.
    #' @param object The R object to save.
    #' @param key Character. The artifact key.
    saveRDS = function(object, key) {
      base::saveRDS(object, file.path(private$basePath, key))
    },

    #' @description Read an Andromeda (zip) artifact.
    #' @param key Character. The artifact key.
    #' @return An Andromeda object.
    readAndromeda = function(key) {
      loadCohortMethodData(file.path(private$basePath, key))
    },

    #' @description Save an Andromeda (zip) artifact.
    #' @param object The Andromeda object to save.
    #' @param key Character. The artifact key.
    saveAndromeda = function(object, key) {
      saveCohortMethodData(object, file.path(private$basePath, key))
    },

    #' @description List artifacts matching a prefix.
    #' @param prefix Character or NULL. Filter to keys starting with this prefix.
    #' @return Character vector of keys.
    listArtifacts = function(prefix = NULL) {
      files <- list.files(private$basePath, recursive = TRUE)
      if (!is.null(prefix)) {
        files <- files[startsWith(files, prefix)]
      }
      files
    },

    #' @description Delete an artifact.
    #' @param key Character. The artifact key.
    delete = function(key) {
      unlink(file.path(private$basePath, key))
    },

    #' @description Ensure a directory exists for a given key.
    #' @param key Character. The artifact key.
    ensureDir = function(key) {
      dir <- dirname(file.path(private$basePath, key))
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
    },

    #' @description Get the full filesystem path for a key.
    #' @param key Character. The artifact key.
    #' @return Character. The full path.
    getFullPath = function(key) {
      file.path(private$basePath, key)
    }
  ),
  private = list(
    basePath = NULL
  )
)
