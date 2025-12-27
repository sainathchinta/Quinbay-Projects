package com.gdn.partners.pcu.external.model;

public interface DownloadApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/download";
  String DOWNLOAD_FILE = "/fromPath/{date}/{updateReason}";
}
