package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.PickupPointApiPath;
import com.gdn.partners.pcu.external.service.PickupPointService;
import com.gdn.partners.pcu.external.web.model.request.PickupPointDetailWebRequest;
import com.gdn.partners.pcu.external.web.model.response.PickupPointDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointStockAndInBoundStatusWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@Slf4j
@Tag(name ="PickupPoint API")
@RestController
@RequestMapping(value = PickupPointApiPath.BASE_PATH)
@Validated
public class PickupPointController {

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Operation(summary ="API to fetch pickupPoint details by codes")
  @PostMapping(value = PickupPointApiPath.FETCH_PICKUP_POINTS_BY_CODES,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<PickupPointDetailWebResponse> getPickupPointDetailsByRequest(
    @RequestBody PickupPointDetailWebRequest pickupPointDetailWebRequest) {
    log.info("Fetching pickupPoint details for {} ",
      pickupPointDetailWebRequest.getPickupPointCodes());
    String requestId = mandatoryParameterHelper.getRequestId();
    List<PickupPointDetailWebResponse> response =
      this.pickupPointService.fetchPickupPointDetailsByRequest(pickupPointDetailWebRequest);
    return new ListBaseResponse<>(null, null, true, requestId, response, null);
  }

  @Operation(summary = "API to fetch accessible pickupPointCodes")
  @PostMapping(produces = MediaType.APPLICATION_JSON_VALUE, consumes =
      MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<PickupPointDetailWebResponse> fetchAccessiblePickupPoints(
      @RequestParam(required = false, defaultValue = "0") Integer page,
      @RequestParam(required = false, defaultValue = "10") Integer size,
      @RequestBody PickupPointDetailWebRequest pickupPointDetailWebRequest) {
    log.info("Fetching accessible pickupPoints for request: {}", pickupPointDetailWebRequest);
    Page<PickupPointDetailWebResponse> responses =
        this.pickupPointService.fetchAccessiblePickupPoints(page, size, pickupPointDetailWebRequest,
            mandatoryParameterHelper.getBusinessPartnerCode());
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        responses.getContent(), new Metadata(page, size, responses.getTotalElements()));
  }

  @Operation(summary = "API to fetch fetch Stock And InBound Status By ProductSku And PPCode")
  @PostMapping(value = PickupPointApiPath.VALIDATE_DELETION_BY_PRODUCT_SKU_AND_PICK_UP_POINT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<PickupPointStockAndInBoundStatusWebResponse> validateDeletionByProductSkuAndPPCode(
    @PathVariable("productSku") String productSku,
    @RequestParam(required = false, defaultValue = "0") Integer page,
    @RequestParam(required = false, defaultValue = "10") Integer size,
    @RequestBody PickupPointDetailWebRequest pickupPointDetailWebRequest) {
    log.info("call #validateDeletionByProductSkuAndPPCode for request : {}, user {} ",
      pickupPointDetailWebRequest, mandatoryParameterHelper.getUsername());
    Page<PickupPointStockAndInBoundStatusWebResponse> stockAndInBoundStatusWebResponses =
      pickupPointService.fetchStockAndInBoundStatusByProductSkuAndPPCode(
        mandatoryParameterHelper.getBusinessPartnerCode(), mandatoryParameterHelper.getStoreId(),
        pickupPointDetailWebRequest, productSku, page, size);
    return new ListBaseResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
      stockAndInBoundStatusWebResponses.getContent(),
      new Metadata(page, size, stockAndInBoundStatusWebResponses.getTotalElements()));
  }
}
