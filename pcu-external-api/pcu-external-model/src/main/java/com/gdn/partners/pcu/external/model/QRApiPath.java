package com.gdn.partners.pcu.external.model;

public interface QRApiPath {
  String BASE_PATH = Constants.CONTEXT_PATH + "/api/qrCode";
  String MERCHANT_TEMPLATE_DOWNLOAD = "/merchantTemplateDownload";
  String GENERATE = "/generate";
  String DELETE = "/delete";
  String DOWNLOAD = "/download";
}
