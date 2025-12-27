package com.gdn.partners.pcu.external.web.controller.util;

import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.BrandAuthApipath;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.BrandAuthService;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandDataRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "BrandAuth Controller")
@RestController
@RequestMapping(value = BrandAuthApipath.BASE_PATH)
public class BrandAuthController {

  @Autowired
  BrandAuthService brandAuthService;

  @Autowired
  MandatoryParameterHelper mandatoryParameterHelper;

  @Operation(summary = "Api to download selected brand auth")
  @PostMapping(value = BrandAuthApipath.SELECTED_BRAND_BULK_DOWNLOAD)
  public void selectedBrandBulkDownload(HttpServletResponse servletResponse,
      @RequestBody BulkBrandDataRequest bulkBrandDataRequest) throws Exception {
    String userName = mandatoryParameterHelper.getUsername();
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(bulkBrandDataRequest.getDownloadRequest()),
        ErrorMessages.BULK_BRAND_DATA_CANNOT_BE_EMPTY);
    log.info("Processing Selected brand auth requested by {} ", userName);
    brandAuthService.selectedBrandBulkDownload(servletResponse, bulkBrandDataRequest);
  }
}
