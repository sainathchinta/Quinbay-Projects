package com.gdn.partners.pcu.external.web.controller;

import java.io.File;
import java.io.FileInputStream;

import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.DownloadApiPath;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name ="Download API")
@RestController
@RequestMapping(value = DownloadApiPath.BASE_PATH)
@Validated
public class DownloadController {

  private static final String FILE_NAME_SEPARATOR = "_";
  private static final String ROOT = "/";
  private static final String EXCEL_FILE_TYPE = ".xlsx";
  private static final String EXCEL_FILE_CONTENT_TYPE_HEADER =
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private SystemParameterProperties systemParameterProperties;


  @Operation(summary ="Download file from NFS")
  @GetMapping(value = DownloadApiPath.DOWNLOAD_FILE, produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
  public void getFileFromPath(HttpServletResponse response, @PathVariable("updateReason") String updateReason,
      @PathVariable("date") String date) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String fileName = businessPartnerCode + FILE_NAME_SEPARATOR + updateReason + FILE_NAME_SEPARATOR + date;
    log.info("Downloading file from path : {}", fileName);
    try {
      String file =
          systemParameterProperties.getDirectoryStockAlertsExcel() + updateReason + ROOT + businessPartnerCode + ROOT
              + date + ROOT + fileName + EXCEL_FILE_TYPE;
      try (FileInputStream fileInputStream = new FileInputStream(file)) {
        byte[] bytesArray = IOUtils.toByteArray(fileInputStream);
        response.setContentType(EXCEL_FILE_CONTENT_TYPE_HEADER);
        response.setHeader("Content-Disposition", "attachment; filename=" + fileName + EXCEL_FILE_TYPE);
        response.setContentLength(bytesArray.length);
        response.getOutputStream().write(bytesArray);
      }
    } catch (Exception e) {
      log.error("Error downloading excel file for stock updates : {}", e.getMessage());
    }
  }

}
