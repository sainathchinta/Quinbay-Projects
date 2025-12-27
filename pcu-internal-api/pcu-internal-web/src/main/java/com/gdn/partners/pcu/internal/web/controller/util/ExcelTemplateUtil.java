package com.gdn.partners.pcu.internal.web.controller.util;

import java.io.IOException;

import org.springframework.web.multipart.MultipartFile;

import com.gdn.merchant.service.utils.FileValidator;

public class ExcelTemplateUtil {
  /**
   * Return is multipartFile is xls or xlsx
   *
   * @param multipartFile
   * @return
   * @throws IOException
   */
  public static boolean isInExcelMimeType(MultipartFile multipartFile) throws IOException {
    return !(!FileValidator.checkFileByType(multipartFile.getBytes(), FileValidator.XLS)
        && !FileValidator.checkFileByType(multipartFile.getBytes(), FileValidator.XLSX));
  }
}
