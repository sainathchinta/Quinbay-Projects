package com.gdn.partners.pcu.internal.web.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.StoreCopyApiPath;
import com.gdn.partners.pcu.internal.service.StoreCopyService;
import com.gdn.partners.pcu.internal.web.model.response.PendingDownloadProcessWebResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@Tag(name = "Store Copy Controller")
@RequestMapping(value = StoreCopyApiPath.BASE_PATH)
public class StoreCopyController {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private StoreCopyService storeCopyService;

  @Operation(summary = "Bulk Download seller products")
  @GetMapping(value = StoreCopyApiPath.DOWNLOAD_ALL_PRODUCTS, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkDownloadSellerProducts(@PathVariable(value = "sellerCode") String sellerCode) {
    log.debug("Invoking api to bulk download products to copy store, source seller : {}", sellerCode);
    storeCopyService.downloadAllProductsBySellerCode(clientParameterHelper.getUsername(),
        sellerCode);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Download upload template")
  @GetMapping(value = StoreCopyApiPath.DOWNLOAD_UPLOAD_TEMPLATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<String> downloadUploadTemplate(@PathVariable(value = "sellerCode") String sellerCode) {
    log.debug("Invoking api to download template to copy store, source seller : {}", sellerCode);
    return new SingleBaseResponse(null, null, true, clientParameterHelper.getRequestId(),
        storeCopyService.downloadUploadTemplate(sellerCode));
  }

  @Operation(summary = "Fetching pending download process")
  @GetMapping(value = StoreCopyApiPath.GET_PENDING_PROCESSES, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<PendingDownloadProcessWebResponse> getPendingProcesses(
      @RequestParam String sellerCode, @RequestParam String userName,
      @RequestParam(required = false, defaultValue = "STORE_COPY_PRODUCTS") String processType) {
    log.debug("Invoking api to get pending download process, sellerCode : {}, userName : {}, processType : {}",
        sellerCode, userName, processType);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        storeCopyService.getPendingProcesses(sellerCode, userName, processType));
  }
}
