package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.model.FileUploadApiPath;
import com.gdn.partners.pcu.internal.service.FileHelper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;

@Slf4j
@Tag(name = "FILE UPLOAD API")
@RestController
@RequestMapping(value = FileUploadApiPath.BASE_PATH)
@RequiredArgsConstructor
@Validated
public class FileUploadController {

  private final ClientParameterHelper clientParameterHelper;

  private final FileHelper fileHelper;

  @Operation(summary = "Bulk Upload File")
  @PostMapping(value = FileUploadApiPath.BULK_UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkUpload(@RequestParam("processType")
  @Valid @NotBlank(message = ErrorMessages.PROCESS_TYPE_CANNOT_BE_EMPTY) String processType,
      @RequestParam MultipartFile file) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Bulk upload file for process type {} and requestId {}", processType, requestId);
    fileHelper.uploadBulkFile(file, processType, requestId, clientParameterHelper.getStoreId(),
        clientParameterHelper.getUsername());
    return new BaseResponse(null, null, true, requestId);
  }
}