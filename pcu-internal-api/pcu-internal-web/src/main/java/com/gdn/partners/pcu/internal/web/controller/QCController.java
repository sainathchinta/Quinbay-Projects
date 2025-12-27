package com.gdn.partners.pcu.internal.web.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.QCApiPath;
import com.gdn.partners.pcu.internal.service.QCTaskService;
import com.gdn.partners.pcu.internal.web.model.request.RejectProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by parvej on 10/07/2020 AD.
 */

@Slf4j
@Tag(name = "FinalQC API")
@RestController
@RequestMapping(value = QCApiPath.BASE_PATH)
@Validated
public class QCController {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private QCTaskService qcTaskService;

  @PostMapping(value = QCApiPath.FILTER_QC_READY_PRODUCT, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<DistributionProductWebResponse> filterProduct(
      @RequestParam(defaultValue = "0", required = false) Integer page,
      @RequestParam(defaultValue = "25", required = false) Integer size,
      @RequestBody SummaryFilterWebRequest summaryFilterWebRequest) throws Exception {
    log.info("Api to fetch product list ready for QC review : request {}", summaryFilterWebRequest);
    Page<DistributionProductWebResponse> response =
        qcTaskService.filterQCProductList(summaryFilterWebRequest, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getTotalElements()));
  }

  @PostMapping(value = QCApiPath.RETRY_PRODUCT_ACTIVATION, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse retryProductActivation(@RequestParam String productCode) throws Exception {
    log.info("Api for retry of product activation for productCode: {}", productCode);
    qcTaskService.retryApproveQc(productCode);
    return new GdnBaseRestResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @RequestMapping(value = QCApiPath.REJECT_PRODUCT, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse rejectProduct(@RequestBody RejectProductWebRequest rejectProductWebRequest)
      throws Exception {
    log.info("Api to reject product for QC request : request {}", rejectProductWebRequest);
    qcTaskService.rejectProduct(rejectProductWebRequest);
    return new GdnBaseRestResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @RequestMapping(value = QCApiPath.APPROVE_PRODUCT, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse approveProduct(@RequestParam String productCode, @RequestParam String productId)
      throws Exception {
    qcTaskService.approveProduct(productCode, productId);
    return new GdnBaseRestResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "business partner API")
  @GetMapping(value = QCApiPath.BUSINESS_PARTNER_LIST, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<BusinessPartnerWebResponse> getBusinessPartnerList(
      @RequestParam(defaultValue = "0", required = false) Integer page,
      @RequestParam(defaultValue = "25", required = false) Integer size, @RequestParam(required = false) String keyword,
      @RequestParam String state) {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Invoking business partner list for state : {} ", state);
    Page<BusinessPartnerWebResponse> response = qcTaskService.getBusinessPartnerList(state, keyword, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }
}
