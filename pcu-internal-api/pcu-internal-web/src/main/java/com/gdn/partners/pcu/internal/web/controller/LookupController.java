package com.gdn.partners.pcu.internal.web.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.LookupControllerPath;
import com.gdn.partners.pcu.internal.service.LookupService;
import com.gdn.partners.pcu.internal.web.model.response.LookupWebResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Lookup API")
@RestController
@RequestMapping(value = LookupControllerPath.BASE_PATH)
@Validated
public class LookupController {

  @Autowired
  private LookupService lookupService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "fetches the dangerous level goods data")
  @GetMapping(value = LookupControllerPath.DANGEROUS_GOODS_LEVEL, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<LookupWebResponse> getDangerousGoodsLevel() {
    log.info("Fetching dangerous goods level");
    List<LookupWebResponse> responseList = lookupService.getDangerousGoodsLevel();
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        responseList, new Metadata());
  }
}
