package com.gdn.mta.product.service;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import jakarta.annotation.Resource;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;
import com.gdn.mta.product.repository.ImageProcessorRepository;
import com.gdn.mta.product.service.util.ImageCheckService;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.valueobject.CommonImagePathDto;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ImageProcessorServiceBean implements ImageProcessorService {

  @Autowired
  private ImageProcessorRepository imageProcessorRepository;

  @Autowired
  @Lazy
  private ProductService productService;

  @Value("${image.source.directory}")
  private String imageSourceDirectory;

  @Value("${check.resize.already.done}")
  private boolean checkResizeAlreadyDone;

  @Autowired
  @Resource(name = "customGraphicsSettingsForImageResizing")
  private CustomGraphicsSettings customGraphicsSettings;

  @Autowired
  private ImageCheckService imageCheckService;

  @Autowired
  private FileStorageService fileStorageService;

  @Override
  public void scaleImage(BulkImagesProcessRequest bulkImagesProcessRequest) throws Exception {
    imageProcessorRepository.scaleImage(bulkImagesProcessRequest);
  }

  @Override
  public void scaleEditedImages(ScaleEditedImageRequest scaleEditedImageRequest) throws Exception {
    imageProcessorRepository.scaleEditedImages(scaleEditedImageRequest);
  }

  @Override
  public boolean resizeImage(String storeId, String productCode, int prioritySeller) throws Exception {
    log.info("Resizing images for product with product code : {}", productCode);
    ProductDetailResponse productDetailResponse = productService.findProductDetailByProductCode(productCode, false);
    if (checkResizeAlreadyDone) {
      boolean containsNonOriginalImages =
          productDetailResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
              .anyMatch(image1 -> !image1.getOriginalImage());
      if (containsNonOriginalImages) {
        return true;
      }
    }
    Map<String, CommonImagePathDto> uniqueImages = new HashMap<>();
    if (Objects.nonNull(productDetailResponse)) {
      String unavailableImageName =
        imageCheckService
            .getUniqueImages(productDetailResponse, uniqueImages, imageSourceDirectory);
      if (StringUtils.isNotEmpty(unavailableImageName)) {
        log.error("Image not found with imageName :{}, for product code :{}", unavailableImageName, productCode);
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, ErrorCategory.DATA_NOT_FOUND.getMessage());
      }
    }
    BulkResizeImageRequest bulkResizeImageRequest = ConverterUtil
        . toBulkResizeImageRequest(productCode, uniqueImages, customGraphicsSettings,
            imageSourceDirectory);
    bulkResizeImageRequest.setPrioritySeller(prioritySeller);
    imageProcessorRepository.resizeImage(bulkResizeImageRequest);
    return false;
  }

  @Override
  public void resizeEditedImage(BulkResizeImageRequest bulkResizeImageRequest) throws Exception {
    log.info("Resizing edited images for product with product code : {}", bulkResizeImageRequest.getGroupCode());
    bulkResizeImageRequest.setCustomGraphicsSettings(customGraphicsSettings);
    String unavailableImageName = fileStorageService.checkImageAvailability(bulkResizeImageRequest.getImageRequests(),
      true);
    if (StringUtils.isNotEmpty(unavailableImageName)) {
      log.error("Image not found with imageName :{}, for product code :{}", unavailableImageName, bulkResizeImageRequest.getGroupCode());
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, ErrorCategory.DATA_NOT_FOUND.getMessage());
    }
    imageProcessorRepository.resizeEditedImage(bulkResizeImageRequest);
  }

  @Override
  public void resizeRevisedImage(BulkResizeImageRequest bulkResizeImageRequest) throws Exception {
    log.info("Resizing revised images for product with product code : {}", bulkResizeImageRequest.getGroupCode());
    bulkResizeImageRequest.setCustomGraphicsSettings(customGraphicsSettings);
    String unavailableImageName = fileStorageService.checkImageAvailability(bulkResizeImageRequest.getImageRequests(),
      false);
    if (StringUtils.isNotEmpty(unavailableImageName)) {
      log.error("Image not found with imageName :{}, for product code :{}", unavailableImageName,
          bulkResizeImageRequest.getGroupCode());
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, ErrorCategory.DATA_NOT_FOUND.getMessage());
    }
    imageProcessorRepository.resizeRevisedImage(bulkResizeImageRequest);
  }

  @Override
  public void resizeEditedImageInFileStoreOrGcs(BulkResizeImageRequest bulkResizeImageRequest)
    throws Exception {
    log.info("Resizing edited images for product with product code : {}",
      bulkResizeImageRequest.getGroupCode());
    bulkResizeImageRequest.setCustomGraphicsSettings(customGraphicsSettings);
    String unavailableImageName = fileStorageService
      .checkImageAvailabilityInFileStoreOrGcs(bulkResizeImageRequest.getImageRequests());
    if (StringUtils.isNotEmpty(unavailableImageName)) {
      log.error("Image not found with imageName :{}, for product code :{}", unavailableImageName,
        bulkResizeImageRequest.getGroupCode());
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
        ErrorCategory.DATA_NOT_FOUND.getMessage());
    }
    imageProcessorRepository.resizeEditedImage(bulkResizeImageRequest);
  }
}
