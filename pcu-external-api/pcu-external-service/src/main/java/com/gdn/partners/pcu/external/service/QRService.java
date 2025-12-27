package com.gdn.partners.pcu.external.service;

import java.io.IOException;

import com.gdn.partners.pcu.external.web.model.request.QRDownloadWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QRGenerateRequest;
import com.google.zxing.WriterException;

public interface QRService {

  /**
   * To generate the QR image with or without template and get the path of file
   *
   * @param request
   * @return
   */
  String getPathOfImage(QRGenerateRequest request) throws IOException, WriterException;

  /**
   * To get the path of Merchant template file in PDF format
   * @param request
   * @return
   */
  String merchantTemplateDownload(QRGenerateRequest request) throws IOException, WriterException;

  /**
   * To delete the QR codes older than the given days
   *
   * @param days
   */
  void deleteQRCodes(int days) throws Exception;

  /**
   * To download the QR codes for the list of products based on the parameters.
   *
   * @param request
   */
  void downloadQRCodesForProducts(QRDownloadWebRequest request) throws Exception;
}
