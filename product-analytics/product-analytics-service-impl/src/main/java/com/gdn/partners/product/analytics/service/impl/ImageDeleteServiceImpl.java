package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.entity.ImageDeletion;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants;
import com.gdn.partners.product.analytics.model.enums.ImageDeleteResults;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.ImageDeletionRepository;
import com.gdn.partners.product.analytics.service.ImageDeleteService;
import com.gdn.partners.product.analytics.service.helper.FileHelper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Slf4j
@Service
@RequiredArgsConstructor
public class ImageDeleteServiceImpl implements ImageDeleteService {

  @Value("${image.delete.minutes.difference}")
  private int imageDeleteMinutesDifference;

  @Value("#{'${image.delete.status.to.fetch}'.split(',')}")
  private Set<String> imageDeleteStatusToFetch;

  private static final BigInteger FOLDER_SIZE = BigInteger.valueOf(200_000);

  private final FileHelper fileHelper;

  private final GCPProperties gcpProperties;

  private final ImageDeletionRepository imageDeletionRepository;


  @Override
  public void updateImageCollectionForProductDelete(String productCode) {
    ImageDeletion imageDeletion =
      ImageDeletion.builder().productCode(productCode).result(ImageDeleteResults.PENDING.name()).retryCount(0)
        .build();
    imageDeletion.setStoreId(Constants.STORE_ID_VALUE);
    imageDeletionRepository.save(imageDeletion);
  }

  @Override
  @Async
  public void deleteImagesOfProduct(String storeId, String productCode) {
    List<ImageDeletion> filteredRecords;
    if (StringUtils.isNotEmpty(productCode)) {
      filteredRecords = Collections.singletonList(
        ImageDeletion.builder().productCode(productCode).result(ImageDeleteResults.PENDING.name())
          .build());
    } else {
      List<ImageDeletion> imageDeletionList =
        imageDeletionRepository.fetchProductCodeForImageDeletion(storeId, imageDeleteStatusToFetch);
      filteredRecords = filterPublishedRecords(imageDeletionList);
    }
    if (CollectionUtils.isNotEmpty(filteredRecords)) {
      Map<String, String> productGroupCodeMap = generateProductGroupCode(filteredRecords);
      for (ImageDeletion imageDeletion : filteredRecords) {
        try {
          long startTime = System.currentTimeMillis();
          productCode = imageDeletion.getProductCode();
          //Saving entity as published to avoid any re-hit
          setRetryCount(imageDeletion);
          imageDeletion.setResult(ImageDeleteResults.PUBLISHED.name());
          imageDeletionRepository.save(imageDeletion);

          int totalCountOfImagesDeleted =
              deleteFinalImages(productGroupCodeMap, productCode, Constants.FULL_IMAGE_PREFIX)
                  + deleteFinalImages(productGroupCodeMap, productCode,
                  Constants.MEDIUM_IMAGE_PREFIX) + deleteFinalImages(productGroupCodeMap,
                  productCode, Constants.THUMBNAIL_IMAGE_PREFIX) + deleteSourceImages(productCode);

          log.info(
              "Successfully deleted the images for the productCode: {}, time taken for deletion "
                  + "is {} seconds", imageDeletion.getProductCode(),
              (System.currentTimeMillis() - startTime) / Constants.THOUSAND);

          //Saving entity as success
          imageDeletion.setResult(ImageDeleteResults.SUCCESS.name());
          imageDeletion.setImageDeletedCount(totalCountOfImagesDeleted);
          imageDeletionRepository.save(imageDeletion);
        } catch (Exception ex) {
          log.error("Exception while deleting image for product code {} ", productCode, ex);
          imageDeletion.setResult(ImageDeleteResults.FAILED.name());
          imageDeletionRepository.save(imageDeletion);
        }
      }
    }
  }

  private List<ImageDeletion> filterPublishedRecords(List<ImageDeletion> imageDeletionList) {
    List<ImageDeletion> filteredList = new ArrayList<>();
    for (ImageDeletion imageDeletion : imageDeletionList) {
      if (!TerminatedSellerDeletionConstants.PUBLISHED.equals(imageDeletion.getResult())
        || !checkUpdatedDate(imageDeletion.getUpdatedDate())) {
        filteredList.add(imageDeletion);
      }
    }
    return filteredList;
  }

  private void setRetryCount(ImageDeletion imageDeletion) {
    if (!ImageDeleteResults.PENDING.name().equals(imageDeletion.getResult())) {
      imageDeletion.setRetryCount(imageDeletion.getRetryCount() + 1);
    }
  }

  private String generateGroupCode(String productCode) {
    BigInteger value = BigInteger.ONE;
    for (int i = 0; i < productCode.length(); i++) {
      value = value.add(new BigInteger(
        StringUtils.EMPTY + (Character.getNumericValue(productCode.charAt(i)) > 0 ?
          Character.getNumericValue(productCode.charAt(i)) :
          1)));
    }
    return String.valueOf(value.mod(FOLDER_SIZE));
  }


  private boolean checkUpdatedDate(Date updatedDate) {
    long timeDifference = new Date().getTime() - updatedDate.getTime();
    long minutesDifference = timeDifference / (60 * 1000);
    return minutesDifference < imageDeleteMinutesDifference;
  }

  private Map<String, String> generateProductGroupCode(List<ImageDeletion> imageDeletionList) {
    Map<String, String> productGroupCodeMap = new HashMap<>();
    imageDeletionList.forEach(
      imageDeletion -> productGroupCodeMap.put(imageDeletion.getProductCode(),
        generateGroupCode(imageDeletion.getProductCode())));
    return productGroupCodeMap;
  }

  private int deleteFinalImages(Map<String, String> productGroupCodeMap,
    String productCode, String imagePathPrefix) {
    String finalImagePrefixPath = gcpProperties.getFinalImagePrefix().concat(imagePathPrefix);
    String catalogImagePathPrefix = gcpProperties.getCatalogImagePathPrefix();
    String finalImagePrefix =
      finalImagePrefixPath.concat(catalogImagePathPrefix).concat(Constants.SLASH_SEPARATOR);
    String finalImageBucketName = gcpProperties.getFinalImageBucketName();
    String groupCode = productGroupCodeMap.getOrDefault(productCode, StringUtils.EMPTY);

    return fileHelper.deleteDirectoryFromGcs(finalImageBucketName,
        finalImagePrefixPath.concat(productCode)) +

        fileHelper.deleteDirectoryFromGcs(finalImageBucketName,
            finalImagePrefixPath.concat(groupCode).concat(Constants.SLASH_SEPARATOR)
                .concat(productCode)) +

        fileHelper.deleteDirectoryFromGcs(finalImageBucketName,
            finalImagePrefix.concat(productCode)) +

        fileHelper.deleteDirectoryFromGcs(finalImageBucketName,
            finalImagePrefix.concat(groupCode).concat(Constants.SLASH_SEPARATOR)
                .concat(productCode));
  }

  private int deleteSourceImages(String productCode) {
    String sourceImagePrefix = gcpProperties.getSourceImagePrefix();
    String catalogImagePathPrefix = gcpProperties.getCatalogImagePathPrefix();
    String sourceImageBucketName = gcpProperties.getSourceImageBucketName();

    return fileHelper.deleteDirectoryFromGcs(sourceImageBucketName,
        sourceImagePrefix.concat(Constants.RESIZE).concat(catalogImagePathPrefix)
            .concat(Constants.SLASH_SEPARATOR).concat(productCode)) +

        fileHelper.deleteDirectoryFromGcs(sourceImageBucketName,
            sourceImagePrefix.concat(Constants.SLASH_SEPARATOR).concat(catalogImagePathPrefix)
                .concat(Constants.SLASH_SEPARATOR).concat(productCode));

  }
}