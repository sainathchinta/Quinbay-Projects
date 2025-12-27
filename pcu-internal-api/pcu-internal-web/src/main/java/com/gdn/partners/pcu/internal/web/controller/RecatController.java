package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.RecatApiPath;
import com.gdn.partners.pcu.internal.service.RecatService;
import com.gdn.partners.pcu.internal.web.model.request.RecatProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RecatProductSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.RecatProcessSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductSummaryWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

@Slf4j
@RestController
@Tag(name = "Recat Automation Controller")
@RequestMapping(value = RecatApiPath.BASE_PATH)
public class RecatController {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private RecatService recatService;

  @Operation(summary = "get failed products mail")
  @GetMapping(value = RecatApiPath.GET_FAILED_PRODUCTS_MAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse getFailedProductsMail(@PathVariable("recat-request-code") String recatRequestCode){
    recatService.getFailedProductsMail(recatRequestCode);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Upload excel file for recategorization")
  @PostMapping(value = RecatApiPath.UPLOAD_EXCEL, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse uploadRecatExcel(@RequestParam MultipartFile recatExcel,
      @RequestParam(required = false) String scheduledTime) throws Exception {
    log.info("Entry for recategorization, file name : {}, scheduledTime {}", recatExcel.getOriginalFilename(), scheduledTime);
    String requestId = this.clientParameterHelper.getRequestId();
    this.recatService.uploadRecatRequest(recatExcel, scheduledTime);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Get review products API")
  @PostMapping(value = RecatApiPath.REQUEST_SUMMARY_FILTER,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<RecatProcessSummaryWebResponse> getRecatSummaryFilter(
      @RequestBody RecatProcessSummaryWebRequest recatProcessSummaryWebRequest,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "25") int size)
      throws Exception {
    log.info("Fetching recat process summary for request : {}, page : {}, size : {}",
        recatProcessSummaryWebRequest, page, size);
    Page<RecatProcessSummaryWebResponse> responsePage =
        recatService.getRecatSummaryByFilter(recatProcessSummaryWebRequest, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        responsePage.getContent(), new Metadata(page, size, responsePage.getTotalElements()));
  }

  @Operation(summary = "API for cancel recat request")
  @PutMapping(value = RecatApiPath.CANCEL_REQUEST)
  public BaseResponse productCenterAction(@PathVariable("recatRequestCode") String recatRequestCode) {
    log.info("invoking controller method cancel recat request : {}", recatRequestCode);
    recatService.cancelRecatRequest(recatRequestCode);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "To get trecat product summary")
  @PostMapping(value = RecatApiPath.GET_RECAT_PRODUCT_SUMMARY, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<RecatProductSummaryWebResponse> getRecatProductSummary(
      @PathVariable("recat-request-code") String recatRequestCode, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "25") int size,
      @RequestBody RecatProductSummaryWebRequest recatProductSummaryWebRequest) {
    log.info(
        "API get product summary for recatRequestCode : {}, page : {}, size : {}, recatProductSummaryWebRequest : {}",
        recatRequestCode, page, size, recatProductSummaryWebRequest);
    Page<RecatProductSummaryWebResponse> recatProductSummaryWebResponsePage =
        recatService.getRecatProductSummary(recatRequestCode, page, size, recatProductSummaryWebRequest);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        recatProductSummaryWebResponsePage.getContent(),
        new Metadata(page, size, recatProductSummaryWebResponsePage.getTotalElements()));
  }

  @Operation(summary = "To get the count of product status for recat request")
  @GetMapping(value = RecatApiPath.GET_REACT_PRODUCT_STATUS_COUNTS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<RecatProductCountWebResponse> getRecatProductStatusCount(
      @PathVariable("recat-request-code") String recatRequestCode) {
    log.info("API get the count of product status for recatRequestCode : {}", recatRequestCode);
    RecatProductCountWebResponse recatProductCountWebResponse =
        recatService.getRecatProductStatusCounts(recatRequestCode);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        recatProductCountWebResponse);
  }

}
