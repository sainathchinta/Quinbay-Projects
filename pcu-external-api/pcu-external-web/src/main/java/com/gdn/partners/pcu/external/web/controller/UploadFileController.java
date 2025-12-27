package com.gdn.partners.pcu.external.web.controller;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.UploadFileApiPath;
import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.web.model.request.SignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.response.SignedUrlResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;

@Slf4j
@Tag(name = "Upload File API")
@RestController
@RequestMapping(value = UploadFileApiPath.BASE_PATH)
@Validated
public class UploadFileController {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private FileStorageService fileStorageService;

  @Operation(summary = "API to upload the file")
  @PostMapping(value = UploadFileApiPath.FILE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse fileUpload(@RequestParam MultipartFile request,
      @RequestParam String keyword, @RequestParam String processFileType) throws Exception {
    GdnPreconditions.checkArgument(!request.isEmpty(), ErrorMessages.FILE_INVALID);
    log.info("Uploading the file at controller. Request : {}", request);
    return new GdnRestSingleResponse<>(
        fileStorageService.processFileUpload(request, keyword, processFileType,
            mandatoryParameterHelper.getBusinessPartnerCode()),
        mandatoryParameterHelper.getRequestId());
  }

  @PostMapping(UploadFileApiPath.GENERATE_SIGNED_URL)
  @Operation(summary = "Generate GCS signed URLs for upload", description = "Generates pre-signed"
    + " URLs to upload files to Google Cloud Storage (GCS).")
  public GdnRestSingleResponse<SignedUrlResponse> generateSignedUrl(
    @Valid @RequestBody SignedUrlRequest signedUrlRequest) {
    signedUrlRequest.setUsername(mandatoryParameterHelper.getUsername());
    log.info("Received request to generate signed URL for file: {}",
      signedUrlRequest.getFileName());
    return new GdnRestSingleResponse<>(fileStorageService.generateSignedUrl(signedUrlRequest),
      mandatoryParameterHelper.getRequestId());
  }
}
