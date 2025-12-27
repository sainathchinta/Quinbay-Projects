package com.gdn.partners.pcu.external.web.controller;


import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.external.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.external.service.impl.helper.ImageValidator;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.web.model.response.AttributeImageUploadResponse;
import org.hibernate.validator.constraints.NotBlank;
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

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.ImageApiPath;
import com.gdn.partners.pcu.external.service.ImageService;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.partners.pcu.external.web.controller.util.ConverterUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.Optional;

@Slf4j
@Tag(name ="Images API")
@RestController
@RequestMapping(value = ImageApiPath.BASE_PATH)
@Validated
public class ImageController {

  private static final String ESCAPE_SLASH = "\\.";
  private static final String SLASH = "/";
  private static final String FILE_NAME_NOT_BLANK = "File name cannot be blank !!";

  @Autowired
  private ImageService imageService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Value("${image.upload.size.threshold.mb}")
  private long imageUploadSizeThresholdInMB;

  @Value("${replace.undefined.with.product.code.switch}")
  private boolean replaceUndefinedWithProductCode;

  @Value("${image.formats.supported}")
  private List<String> imageFormatsSupported;

  @Operation(summary ="Get Image Detail")
  @GetMapping(value = ImageApiPath.IMAGE_DETAIL)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public void getImageDetails(HttpServletResponse httpServletResponse,
      @RequestParam("fileName") @Valid @NotBlank(message = FILE_NAME_NOT_BLANK) String fileName,
      @RequestParam(defaultValue = "false") boolean active)
      throws Exception {
    ServletOutputStream servletOutputStream = httpServletResponse.getOutputStream();
      String[] splitImageFilenameByDash = fileName.split(SLASH);
      String[] splitImageFilenameByDot = splitImageFilenameByDash[splitImageFilenameByDash.length - 1].split(ESCAPE_SLASH);
      String imageFiletype = splitImageFilenameByDot[splitImageFilenameByDot.length - 1];
      byte[] bytes = imageService.getImageDetail(fileName, active);
      httpServletResponse.setHeader("Content-Type", "image/" + imageFiletype);
      httpServletResponse.setHeader("Content-Disposition", "inline; filename="
          + splitImageFilenameByDash[splitImageFilenameByDash.length - 1]);
      httpServletResponse.setContentLength(bytes.length-1);
      servletOutputStream.write(bytes);
  }

  @Operation(summary ="Upload image API")
  @PostMapping(value = ImageApiPath.UPLOAD_IMAGE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse uploadImage(@RequestParam("imageFileName") String imageFilename,
      @RequestParam MultipartFile image, @RequestParam(required = false, defaultValue = "false") String active,
      @RequestParam(required = false) String productCode, @RequestParam(required = false) boolean retryRequest)
      throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    if (!image.isEmpty()) {
      log.info("ProductCode: {}, ImageFileName: {}, ImageSize: {}, productActive: {} - {}", productCode, imageFilename,
          image.getSize(), active, requestId);
      ImageValidator.validateImageExtensionAndSize(
        Optional.ofNullable(image.getOriginalFilename()).map(String::toLowerCase)
          .orElseThrow(() -> new ImageValidationException(ErrorCategory.VALIDATION.getMessage())),
        image.getBytes(), image.getSize(), imageUploadSizeThresholdInMB, imageFormatsSupported);
      imageFilename =
          ImageValidator.replaceUndefinedInImageFileWithProductCode(replaceUndefinedWithProductCode, imageFilename,
              productCode);
      UploadImageRequest request = ConverterUtil.toUploadImageRequest(imageFilename, productCode, image.getBytes(),
          Boolean.valueOf(active), retryRequest, image.getOriginalFilename().toLowerCase());
      boolean success = this.imageService.uploadImage(request);
      return new BaseResponse(null, null, success, requestId);
    } else {
      log.info("File is empty , requestId : {}", requestId);
      throw new ValidationException(ErrorMessages.FILE_EMPTY_ERROR_MESSAGE);
    }
  }

  @Operation(summary = "Upload attribute image API")
  @PostMapping(value = ImageApiPath.UPLOAD_ATTRIBUTE_IMAGE, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<AttributeImageUploadResponse> uploadAttributeImage(
      @RequestParam("imageFileName") String imageFilename, @RequestParam MultipartFile image)
      throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("ImageFileName: {}, ImageSize: {} - {}", imageFilename, image.getSize(), requestId);
    AttributeImageUploadResponse attributeImageUploadResponse =
        AttributeImageUploadResponse.builder()
            .finalImagePath(this.imageService.uploadAttributeImage(image, imageFilename)).build();
    return new SingleBaseResponse<>(null, null, true, requestId, attributeImageUploadResponse);
  }
}
