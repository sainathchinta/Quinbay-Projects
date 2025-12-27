package com.gdn.partners.pcu.external.model;

public interface QRV2ApiPath {
  String BASE_PATH = Constants.CONTEXT_PATH + "/api/qrGenerate";
  String ACCESSIBLE = "/accessible";
  String DOWNLOAD_QR_CODES = "/download/qr-code";
  String UPLOAD_FOR_QR = "/upload/generate-qr";
}
