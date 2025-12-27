package com.gdn.mta.product.controller;

import com.gda.mta.product.dto.response.SequenceResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.SequenceService;
import com.gdn.mta.product.web.model.SequenceControllerPath;
import com.gdn.partners.pbp.commons.constants.Constants;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = SequenceControllerPath.BASE_PATH)
@Tag(name = "SequenceController")
@Slf4j
public class SequenceController {

  @Autowired
  private SequenceService sequenceService;

  @GetMapping(value = SequenceControllerPath.GET_SEQUENCE_BY_KEY, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get counter of sequence by key", description = "Get counter of sequence by key")
  public GdnRestSingleResponse<SequenceResponse> findCounterByKey(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String key) {
    log.info("Fetching counter for key : {} ", key);
    try {
      GdnPreconditions.checkArgument(!Constants.PRODUCT_CODE_PREFIX.equalsIgnoreCase(key),
          ErrorMessages.PRODUCT_CODE_COUNTER_DISABLED);
      return new GdnRestSingleResponse<>(null, null, true,
          SequenceResponse.builder().key(key).counter(sequenceService.findCounterByKey(key))
              .build(), requestId);
    } catch (Exception e) {
      log.error("Error fetching counter for key : {}, error - ", key, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          null, requestId);
    }
  }
}
