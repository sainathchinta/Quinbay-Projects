package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.DistributionInfoApiPath;
import com.gdn.partners.pcu.external.service.DistributionInfoService;
import com.gdn.partners.pcu.external.web.controller.util.ConverterUtil;
import com.gdn.partners.pcu.external.web.model.request.DistributionInfoWebRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = DistributionInfoApiPath.BASE_PATH)
@RequiredArgsConstructor
@Tag(name = "DistributionInfoController", description = "Distribution Info API")
public class DistributionInfoController {

  private final DistributionInfoService distributionInfoService;

  private final MandatoryParameterHelper mandatoryParameterHelper;

  @Operation(summary = "Get distribution info for product items")
  @GetMapping(value = DistributionInfoApiPath.GET_DISTRIBUTION_INFO, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<DistributionInfoPerSkuResponse> getDistributionInfo(
      @PathVariable String productCode,
      @RequestParam(defaultValue = "false") boolean needDistributionInfoResponse,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size) {
    String requestId = mandatoryParameterHelper.getRequestId();
    Page<DistributionInfoPerSkuResponse> pageData =
        distributionInfoService.getDistributionInfo(productCode, needDistributionInfoResponse, page,
            size);
    return new ListBaseResponse<>(null, null, true, requestId, pageData.getContent(),
        new Metadata(page, size, pageData.getTotalElements()));
  }

  @Operation(summary = "Update distribution info")
  @PostMapping(value = DistributionInfoApiPath.UPDATE_DISTRIBUTION_INFO, consumes =
      MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateDistributionInfo(@PathVariable String productCode,
      @RequestBody DistributionInfoWebRequest distributionInfoWebRequest) {
    log.info("Update distribution-info for the request:{}", distributionInfoWebRequest);
    distributionInfoService.updateDistributionInfo(productCode,
        ConverterUtil.convertToDistributionInfoUpdateRequest(distributionInfoWebRequest,
            mandatoryParameterHelper.getBusinessPartnerCode()));
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }
}
