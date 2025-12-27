package com.gdn.x.productcategorybase.controller;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.controller.util.ConverterUtil;
import com.gdn.x.productcategorybase.dto.response.LookupResponse;
import com.gdn.x.productcategorybase.entity.Lookup;
import com.gdn.x.productcategorybase.service.LookupService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(value = LookupController.BASE_PATH)
@Tag(name = "look up Api")
public class LookupController {

  public static final String BASE_PATH = "/api/lookup";
  public static final String LOOKUP_GROUP = "/lookupGroup";

  @Autowired
  private LookupService lookupService;

  @Operation(summary = "API to fetch dangerous goods level")
  @RequestMapping(value = LOOKUP_GROUP, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<LookupResponse> getLookupByLookupGroup(@RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String storeId,
      @RequestParam String username, @RequestParam String lookupGroup) {
    log.info("Fetching details for lookup group : {}", lookupGroup);
    List<Lookup> response = new ArrayList<>();
    try {
      response = lookupService.getLookupByLookupGroup(lookupGroup);
    } catch (Exception e) {
      log.error("error while fetching details for lookup group : {} ", lookupGroup, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
    return new GdnRestListResponse<LookupResponse>(null, null, true, ConverterUtil.toLookupResponseList(response),
        new PageMetaData(response.size(), 0, response.size()), requestId);
  }
}
