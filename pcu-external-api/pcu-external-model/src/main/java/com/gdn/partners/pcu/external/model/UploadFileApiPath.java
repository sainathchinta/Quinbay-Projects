package com.gdn.partners.pcu.external.model;

public interface UploadFileApiPath {
  String BASE_PATH = Constants.CONTEXT_PATH + "/api/upload";
  String FILE = "/file";
  String GENERATE_SIGNED_URL = "/generate-signed-url";
}
