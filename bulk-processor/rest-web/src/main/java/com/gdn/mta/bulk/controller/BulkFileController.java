package com.gdn.mta.bulk.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.download.BulkFailedProductFileService;
import com.gdn.mta.product.util.GdnRestSimpleResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@RestController
@RequestMapping(value = BulkFileController.BASE_PATH)
@Tag(name = "BulkFileController", description = "Bulk File Service API")
public class BulkFileController {

  public static final String BASE_PATH = "/api/bulk-file";

  private static final Logger LOG = LoggerFactory.getLogger(BulkFileController.class);
  private static final String DELETE_FILE_LAST_WEEK = "/delete-file";
  private static final String DELETE_FILE_FAILED_PRODUCT = DELETE_FILE_LAST_WEEK
      + "/failed-product";

  @Autowired
  private BulkFailedProductFileService bulkFailedProductFileService;

  @RequestMapping(value = DELETE_FILE_FAILED_PRODUCT, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Bulk-File delete failed product files with modified date last week")
  public GdnRestSimpleResponse<Integer> deleteFailedProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId)
          throws ApplicationException {
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "deleteFailedProduct", null, 
        null, requestId, storeId, channelId, clientId, null, null);
    try {
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      
      int result = bulkFailedProductFileService.deleteFileWithModifiedDateLastWeek();
      return new GdnRestSimpleResponse<>(requestId, result);
    } catch (Exception e) {
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, e.getMessage(), e);
    }
  }

}
