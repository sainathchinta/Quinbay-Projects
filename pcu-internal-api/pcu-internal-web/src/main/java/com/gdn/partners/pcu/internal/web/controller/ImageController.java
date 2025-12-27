package com.gdn.partners.pcu.internal.web.controller;

import java.io.IOException;

import com.gdn.partners.pcu.internal.client.model.response.AttributeImageUploadResponse;
import com.gdn.partners.pcu.internal.properties.ProductImageProperties;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.internal.service.impl.helper.ImageValidator;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.model.request.UploadImageRequest;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.model.ImageApiPath;
import com.gdn.partners.pcu.internal.service.ImageService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Image API")
@RestController
@RequestMapping(value = ImageApiPath.BASE_PATH)
@Validated
public class ImageController {

  @Autowired
  ImageService imageService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Value("${image.upload.size.threshold.mb}")
  private long imageUploadSizeThresholdInMB;

  @Value("${replace.undefined.with.product.code.switch}")
  private boolean replaceUndefinedWithProductCode;

  @Autowired
  private ProductImageProperties productImageProperties;

  @Operation(summary = "Check image size")
  @GetMapping(value = ImageApiPath.CHECK_SIZE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<String> checkImageSize(@RequestParam String imageFileName, @RequestParam boolean active) {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Method : Check image file size for file : {}", imageFileName);
    boolean response;
    try {
      response = imageService.checkImageSize(imageFileName, active);
    } catch (IOException e) {
      return new SingleBaseResponse<>(e.getMessage(), null, false, requestId, imageFileName);
    }
    return new SingleBaseResponse<>(null, null, response, requestId, imageFileName);
  }

  @Operation(summary = "Upload image API")
  @PostMapping(value = ImageApiPath.UPLOAD_DOCUMENT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse uploadImage(@RequestParam String documentFileName, @RequestParam MultipartFile document,
      @RequestParam String sellerCode, @RequestParam String brandCode) throws IOException {
    String requestId = this.clientParameterHelper.getRequestId();
    if (!document.isEmpty()) {
      log.info("Upload Document , Brand code: {}, Seller code : {}, DocumentFileName: {}, Size: {}", sellerCode,
          brandCode, documentFileName, document.getSize());
      try {
        this.imageService.uploadDocument(documentFileName, document.getBytes(), sellerCode, brandCode);
        return new BaseResponse(null, null, true, requestId);
      } catch (Exception e) {
        return new BaseResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
      }
    } else {
      log.info("File is empty , requestId : {}", requestId);
      return new BaseResponse(ErrorMessages.FILE_EMPTY_ERROR_MESSAGE, ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @Operation(summary = "Upload attribute image API")
  @PostMapping(value = ImageApiPath.UPLOAD_ATTRIBUTE_IMAGE, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<AttributeImageUploadResponse> uploadAttributeImage(
      @RequestParam("imageFileName") String imageFilename, @RequestParam MultipartFile image)
      throws Exception {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("ImageFileName: {}, ImageSize: {} - {}", imageFilename, image.getSize(), requestId);
    AttributeImageUploadResponse attributeImageUploadResponse =
        AttributeImageUploadResponse.builder()
            .finalImagePath(this.imageService.uploadAttributeImage(image, imageFilename)).build();
    return new SingleBaseResponse<>(null, null, true, requestId, attributeImageUploadResponse);
  }

  @Operation(summary = "Upload image API")
  @PostMapping(value = ImageApiPath.UPLOAD_IMAGE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse uploadProductImage(@RequestParam("imageFileName") String imageFilename,
      @RequestParam MultipartFile image,
      @RequestParam(required = false, defaultValue = "false") String active,
      @RequestParam(required = false) String productCode,
      @RequestParam(required = false) boolean retryRequest) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    if (!image.isEmpty()) {
      log.info("ProductCode: {}, ImageFileName: {}, ImageSize: {}, productActive: {} - {}",
          productCode, imageFilename, image.getSize(), active, requestId);
      if (StringUtils.isNotBlank(image.getOriginalFilename())) {
        ImageValidator.validateImageExtensionAndSize(image.getOriginalFilename().toLowerCase(),
            image.getBytes(), image.getSize(), imageUploadSizeThresholdInMB,
            productImageProperties.getValidExtensionsToMagicNumbers());
      } else {
        throw new ImageValidationException(ErrorCategory.VALIDATION.getMessage());
      }

      imageFilename =
          ImageValidator.replaceUndefinedInImageFileWithProductCode(replaceUndefinedWithProductCode,
              imageFilename, productCode);
      UploadImageRequest request =
          ConverterUtil.toUploadImageRequest(imageFilename, productCode, image.getBytes(),
              Boolean.valueOf(active), retryRequest, image.getOriginalFilename().toLowerCase());
      boolean success = this.imageService.uploadImage(request);
      return new BaseResponse(null, null, success, requestId);
    } else {
      log.info("File is empty , requestId : {}", requestId);
      throw new ImageValidationException(ErrorMessages.FILE_EMPTY_ERROR_MESSAGE);
    }
  }
}
