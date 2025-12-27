package com.gdn.partners.pcu.external.model;

public interface VideoApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/video-upload";
  String GENERATE_SIGNED_URL = "/generate-signed-url";
  String FETCH_CONFIGURATION = "/fetch-configuration";
}
