package com.gdn.partners.pcu.internal.web.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.BulkProcessApiPath;
import com.gdn.partners.pcu.internal.service.BulkProcessService;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 30/01/2019 AD.
 */

@Slf4j
@Tag(name = "Bulk Process")
@RestController
@RequestMapping(value = BulkProcessApiPath.BASE_PATH)
public class BulkProcessController {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Operation(summary = "Bulk Download screening products offline")
  @PostMapping(value = BulkProcessApiPath.DOWNLOAD_PRODUCTS_SCREENING, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse bulkDownloadScreeningProducts(@RequestParam(defaultValue = "en") String language,
      @RequestBody ReviewProductsFilterRequest reviewProductsFilterRequest) {
    log.info("Invoking api to bulk download products in screening by filter :{}", reviewProductsFilterRequest);
    String username = clientParameterHelper.getUsername();
    this.bulkProcessService.bulkDownloadScreeningProducts(username,
        reviewProductsFilterRequest, language);
    return new GdnBaseRestResponse(true);
  }
}
