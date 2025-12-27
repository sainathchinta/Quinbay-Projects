package com.gdn.partners.pcu.internal.web.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.BulkInternalProcessControllerApiPath;
import com.gdn.partners.pcu.internal.service.BulkInternalProcessService;
import com.gdn.partners.pcu.internal.web.model.request.BulkInternalProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BulkInternalProcessSummaryWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@Tag(name = "Bulk Internal Process Controller")
@RequestMapping(value = BulkInternalProcessControllerApiPath.BASE_PATH)
public class BulkInternalProcessController {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private BulkInternalProcessService bulkInternalProcessService;

  @Operation(summary = "Store Copy Listing Api")
  @PostMapping(value = BulkInternalProcessControllerApiPath.SUMMARY, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<BulkInternalProcessSummaryWebResponse> bulkInternalProcessSummary(
      @RequestBody BulkInternalProcessSummaryWebRequest bulkInternalProcessSummaryWebRequest,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) throws Exception {
    log.info("Fetching internal process summary for request : {}, page : {}, size : {}", bulkInternalProcessSummaryWebRequest,
        page, size);
    Page<BulkInternalProcessSummaryWebResponse> responsePage =
        bulkInternalProcessService.bulkInternalProcessSummary(bulkInternalProcessSummaryWebRequest, page, size);
    return new ListBaseResponse<>(null, null, true,  clientParameterHelper.getRequestId(),
        responsePage.getContent(), new Metadata(page, size, responsePage.getTotalElements()));
  }

  @Operation(summary = "Upload excel file for internal process")
  @PostMapping(value = BulkInternalProcessControllerApiPath.UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse uploadExcelForInternalProcess(@RequestParam MultipartFile internalProcess,
      @RequestParam(required = false, defaultValue = "STORE_COPY") String processType,
      @RequestParam(required = false) String sellerCode, @RequestParam(required = false) String sellerName)
      throws Exception {
    log.info("Entry for bulk internal process, file name : {} ", internalProcess.getOriginalFilename());
    String requestId =  clientParameterHelper.getRequestId();
    this.bulkInternalProcessService.uploadInternalProcess(internalProcess, sellerCode, sellerName, processType);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Cancel new bulk internal process request")
  @PostMapping(value = BulkInternalProcessControllerApiPath.CANCEL, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse cancelInternalBulkProcessRequest(@RequestParam String internalProcessRequestCode) {
    log.info("Cancel new request for internal bulk process : {} ", internalProcessRequestCode);
    String requestId =  clientParameterHelper.getRequestId();
    this.bulkInternalProcessService.cancelInternalBulkProcessRequest(internalProcessRequestCode);
    return new BaseResponse(null, null, true, requestId);
  }
}
