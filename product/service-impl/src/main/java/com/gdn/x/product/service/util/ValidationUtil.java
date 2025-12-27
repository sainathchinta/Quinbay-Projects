package com.gdn.x.product.service.util;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.annotation.Nullable;

import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.exception.ApiErrorCodes;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.exceptions.ValidationException;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.VideoListResponse;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by parvej on 04/12/2020 AD.
 */

@Slf4j
public class ValidationUtil {

  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String YOUTUBE_REGEX = "^(http(s)?:\\/\\/)?((w){3}.)?youtu(be|.be)?(\\.com)?\\/.+";
  private static final Pattern YOUTUBE_PATTERN = Pattern.compile(YOUTUBE_REGEX);
  private static final String VIDEO_REGEX =
      "(?<=watch\\?v=|/videos/|embed\\/|youtu.be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F|embed%\u200C\u200B2F|youtu.be%2F|%2Fv%2F)[^#\\&\\?\\n]*";
  private static final Pattern VIDEO_PATTERN = Pattern.compile(VIDEO_REGEX);

  public static boolean validateYouTubeUrl(String youTubeUrl, String apiKey, YouTube youTube,
      boolean youTubeUrlValidationSwitch) throws Exception {
    if (StringUtils.isBlank(youTubeUrl)) {
      return true;
    }
    try {
      String videoId = validateUrlAndGetVideoId(VIDEO_PATTERN, youTubeUrl);
      if (youTubeUrlValidationSwitch && Objects.nonNull(videoId) && Objects
          .nonNull(validateUrlAndGetVideoId(YOUTUBE_PATTERN, youTubeUrl))) {
        log.info("Validating youtube videoId: {}", videoId);
        return isYouTubeUrlActive(videoId, apiKey, youTube);
      } else if (Objects.nonNull(videoId) && Objects.nonNull(validateUrlAndGetVideoId(YOUTUBE_PATTERN, youTubeUrl))) {
        return true;
      }
    } catch (Exception e) {
      log.error("getting exception while calling youtube API for url: {} ", youTubeUrl, e);
    }
    return false;
  }

  private static boolean isYouTubeUrlActive(String videoID, String apiKey, YouTube youTube) throws IOException {
    YouTube.Videos.List listVideosRequest = youTube.videos().list(List.of(VIDEO_LIST));
    listVideosRequest.setId(List.of(videoID));
    listVideosRequest.setKey(apiKey);
    VideoListResponse listResponse = listVideosRequest.execute();
    log.info("getting youtube api response: {}", listResponse);

    return listResponse.getPageInfo().getTotalResults() >= 1;
  }

  private static String validateUrlAndGetVideoId(Pattern pattern, String youtubeUrl) {
    Matcher matcher = pattern.matcher(youtubeUrl);
    if (matcher.find()) {
      return matcher.group();
    }
    return null;
  }

  public static void checkParameter(boolean expression, @Nullable Object errorMessage) {
    if (!expression) {
      throw new ValidationException(String.valueOf(errorMessage));
    }
  }

  public static void validateProduct(Product product) {
    if (product.isSuspended()) {
      throw new ApiIncorrectInputDataException(
          ApiErrorCodes.PRODUCT_SUSPENDED.getErrorMessage() + product.getProductSku(),
          ApiErrorCodes.PRODUCT_SUSPENDED.getErrorCode());
    } else if (product.isTakenDown()) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.TAKEN_DOWN.getErrorMessage(),
          ApiErrorCodes.TAKEN_DOWN.getErrorCode());
    } else if (product.isMarkForDelete()) {
      throw new ApiIncorrectInputDataException(
          ApiErrorCodes.PRODUCT_PERMANENTLY_DELETED.getErrorMessage() + product.getProductSku(),
          ApiErrorCodes.PRODUCT_PERMANENTLY_DELETED.getErrorCode());
    } else if (product.isArchived()) {
      throw new ApiIncorrectInputDataException(
          ApiErrorCodes.PRODUCT_IS_ARCHIVED.getErrorMessage() + product.getProductSku(),
          ApiErrorCodes.PRODUCT_IS_ARCHIVED.getErrorCode());
    }
  }

  public static void validateProductBasicInfoBatchSize(List<String> productSkus, int productBasicInfoFetchBatchSize) {
    if (productSkus.size() > productBasicInfoFetchBatchSize) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCodes.PRODUCT_BATCH_SIZE_EXCEEDED.getErrorMessage() + productBasicInfoFetchBatchSize);
    }
  }

  public static boolean checkIfBlacklistedSellerForSpecificEvent(
    Map<String, Set<String>> eventToBlackListedSellersMap, String businessPartnerCode,
    String topicName) {
    return MapUtils.isNotEmpty(eventToBlackListedSellersMap) && CollectionUtils.isNotEmpty(
      eventToBlackListedSellersMap.get(topicName)) && eventToBlackListedSellersMap.get(topicName)
      .contains(businessPartnerCode);
  }
}
