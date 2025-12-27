package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.product.BulkBasicInfoVideoDownloadResponseModel;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessVideo;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.models.ImageDownloadResult;
import com.gdn.mta.bulk.models.L3InfoUpdateChangeType;
import com.gdn.mta.bulk.models.ProductBasicDetail;
import com.gdn.mta.bulk.models.ProductL3Response;
import com.gdn.mta.bulk.models.ProductMasterDataEditRequest;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductImageAndVideoResponse;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.request.BulkBasicInfoUpdateRequest;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ResponseHelper;
import com.gdn.mta.bulk.util.ValidateUrlUtil;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCategoryDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.google.api.services.youtube.YouTube;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.gdn.mta.bulk.BulkProcessValidationErrorMessages.HEADER_VALIDATION_ERROR;

@Service
@Slf4j
public class BulkBasicInfoUpdateServiceBean implements BulkBasicInfoUpdateService {

  private static final int BULK_BASIC_INFO_FIRST_ROW_INDEX = 4;
  public static final String FILE_BLANK_ERROR = "File tidak boleh kosong";

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private BulkProcessRepository bulkProcessRepository;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private BulkProcessImageService bulkProcessImageService;

  @Autowired
  private BulkProcessVideoService bulkProcessVideoService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  @Qualifier("youTube")
  private YouTube youTube;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Value(value = "${youtube.data.api.key}")
  private String youTubeDataApiKey;

  @Value("${bulk.l3.update.max.number.of.rows}")
  private int bulkL3UpdateMaxRows;

  @Value(value = "${validate.youtube.url.switch.en}")
  private boolean validateYoutubeUrlSwitchEn;

  @Value("${bulk.max.number.of.rows}")
  private int bulkMaxNumberOfRows;

  @Value("${validate.bulk.max.number.of.rows}")
  private boolean validateBulkMaxNumberOfRows;

  @Autowired
  private ProductLevel3BulkUpdateServiceBean productLevel3BulkUpdateServiceBean;

  @Autowired
  private FileStorageServiceBean fileStorageServiceBean;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Value("${update.product.basic.info.download.link}")
  private String updateProductBasicInfoDownloadLink;

  @Value("${http.connection.timeout}")
  private int httpConnectionTimeout;

  @Value("${http.connection.readTimeout}")
  private int httpConnectionReadTimeout;

  @Value("${video.static.base.url.prefix}")
  private String videoStaticBaseUrlPrefix;

  @Value("${image.static.base.url.prefix}")
  private String imageStaticBaseUrlPrefix;

  @Value("${gcs.image.path.prefix}")
  private String pathPrefix;

  @Value("${youtube.regex}")
  private String youtubeRegex;

  @Autowired
  private SystemParameterConfigHistoryServiceBean systemParameterConfigHistoryServiceBean;

  @Override
  public void preProcessBulkBasicInfoUpdate(String storeId, String requestId,
      BulkBasicInfoRequest bulkBasicInfoRequest) {
    final String bulkProcessCode = bulkBasicInfoRequest.getBulkProcessCode();
    final String businessPartnerCode = bulkBasicInfoRequest.getBusinessPartnerCode();
    bulkBasicInfoRequest.setStoreId(storeId);
    log.info("Starting bulk basic info preprocessing. storeId: {}, bulkProcessCode: {}, businessPartnerCode: {}",
        storeId, bulkProcessCode, businessPartnerCode);
    try {
      createAndSaveBulkProcess(storeId, requestId, bulkProcessCode, bulkBasicInfoRequest);
      publishBasicInfoUploadEvent(bulkBasicInfoRequest);
      log.info("Bulk basic info preprocessing completed. storeId: {}, bulkProcessCode: {}, businessPartnerCode: {}",
          storeId, bulkProcessCode, businessPartnerCode);
    } catch (Exception e) {
      log.error("Failed to process bulk basic info update. storeId: {}, bulkProcessCode: {}, businessPartnerCode: {}",
          storeId, bulkProcessCode, businessPartnerCode, e);
      throw new RuntimeException("Failed to process bulk basic info update", e);
    }
  }

  @Override
  public void processBulkUpdate(BulkBasicInfoRequest bulkBasicInfoRequest) {
    final String storeId = bulkBasicInfoRequest.getStoreId();
    final String bulkProcessCode = bulkBasicInfoRequest.getBulkProcessCode();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    log.info("invoking postProcessing for bulk update. bulkBasicInfoRequest: {}", bulkBasicInfoRequest);
    BulkProcess bulkProcess =
        bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(storeId, bulkProcessCode,
            BulkProcess.STATUS_PENDING);
    if (Objects.isNull(bulkProcess)) {
      log.warn("Bulk basic info update for bulkProcessCode : {} is already processed or being processed",
          bulkBasicInfoRequest.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          "Bulk file with bulk process code : " + bulkBasicInfoRequest.getBulkProcessCode()
              + " is already processed or being processed");
    }
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkProcess = bulkProcessRepository.save(bulkProcess);
    try {
      Sheet excelSheetData = this.validateBulkUpdateRequest(bulkBasicInfoRequest, bulkProcess);
      int maxRowProcessingSize = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_BASIC_INFO_MAXIMUM_SIZE).getValue());
      POIUtil.validateNumberOfRows(excelSheetData, bulkProcess.getBulkProcessCode(),
          bulkL3UpdateMaxRows, validateBulkMaxNumberOfRows);
      ProfileResponse profileResponse = ProfileResponse.builder().company(new CompanyDTO()).build();
      List<Map<String, String>> productDataFromExcel =
          POIUtil.readFromExcelForBulkUpdateHavingSellerSku(excelSheetData, BULK_BASIC_INFO_FIRST_ROW_INDEX, 0, 0,
              false, profileResponse, true);
      if (productDataFromExcel.isEmpty()) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, BulkUpdateServiceUtil.FILE_BLANK_ERROR);
      }
      if (productDataFromExcel.size() > maxRowProcessingSize) {
        log.error("Number of rows {} is greater than {} threshold for bulk l3 update process",
            productDataFromExcel.size(), maxRowProcessingSize);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN + maxRowProcessingSize);
      }
      productDataFromExcel = productDataFromExcel.stream().map(row -> row.entrySet().stream()
              .collect(Collectors.toMap(entry -> entry.getKey().trim(), Map.Entry::getValue, (v1, v2) -> v1)))
          .collect(Collectors.toList());
      bulkProcess.setTotalCount(productDataFromExcel.size());
      this.generateBulkProcessData(bulkProcess, productDataFromExcel, bulkBasicInfoRequest);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while postProcessing for bulk update. bulkUpdateQueue: {}", bulkBasicInfoRequest, e);
      String errorMessage = Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY);
      String downloadLink = StringUtils.EMPTY;
      downloadLink = setErrorMessageAndGetDownloadLink(bulkProcess, errorMessage, downloadLink);
      notificationService.sendBulkUploadedNotification(bulkProcess, NotificationType.BULK_UPDATED.getValue(),
          downloadLink);
      BulkUpdateServiceUtil.updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, e.getMessage(), counter);
      bulkProcessService.saveOperation(bulkProcess);
    } catch (Exception e) {
      log.error("Error while postProcessing for bulk update. bulkUpdateQueue: {}", bulkBasicInfoRequest, e);
      bulkProcess.setDescription(Constant.SYSTEM_ERROR);
      BulkUpdateServiceUtil.updateBulkStatusAborted(bulkProcess, storeId, bulkProcessCode, e.getMessage(), counter);
      bulkProcessService.saveOperation(bulkProcess);
    }
  }

  @Override
  public void processBulkProcessVideoUpdate(String storeId,
      BulkBasicInfoVideoDownloadResponseModel bulkBasicInfoVideoDownloadResponseModel) {
    Map<String, String> additionalFields = bulkBasicInfoVideoDownloadResponseModel.getAdditionalFields();
    if (additionalFields.containsKey(Constant.BULK_PROCESS_CODE) && StringUtils.isNotBlank(
        additionalFields.get(Constant.BULK_PROCESS_CODE))) {
      BulkProcessVideo bulkProcessVideo = bulkProcessVideoService.findByStoreIdAndBulkProcessCodeAndUploadedURL(storeId,
          bulkBasicInfoVideoDownloadResponseModel.getAdditionalFields().get(Constant.BULK_PROCESS_CODE),
          bulkBasicInfoVideoDownloadResponseModel.getOriginalUrl());
      if (Objects.nonNull(bulkProcessVideo)) {
        CommonUtils.setBulkProcessVideoFinalStatus(bulkBasicInfoVideoDownloadResponseModel, bulkProcessVideo);
        bulkProcessVideoService.saveBulkProcessVideo(bulkProcessVideo);
      }
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void downloadImages(String bulkProcessCode, List<String> imageDownloadList) {
    BulkProcess bulkProcess =
        bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID, bulkProcessCode);
    List<BulkProcessImage> bulkProcessImageList =
        bulkProcessImageService.findByBulkProcessCodeAndImageUrl(bulkProcess, imageDownloadList);
    for (BulkProcessImage bulkProcessImage : bulkProcessImageList) {
      ImageDownloadResult imageDownloadResult = new ImageDownloadResult();
      Map<String, String> imageUrlAndNameMap = new HashMap<>();
      imageUrlAndNameMap.put(bulkProcessImage.getImageURL(),
          String.valueOf(bulkProcessImage.getSequence()));
      StringBuilder validationErrorMessage = new StringBuilder();
      try {
        List<ProductBasicDetail> productBasicDetails =
            objectMapper.readValue(bulkProcessImage.getNotes(), new TypeReference<List<ProductBasicDetail>>() {
            });
        log.info("Downloading image for bulkProcessCode : {} image : {} ", bulkProcessCode,
            bulkProcessImage.getImageURL());
        imageDownloadResult =
            fileStorageServiceBean.downloadImageFileValidateAndUploadToGCS(bulkProcessCode, imageUrlAndNameMap,
                Collections.singletonList(bulkProcessImage.getImageURL()), validationErrorMessage, productBasicDetails);
      } catch (Exception e) {
        log.error("Exception when downloading image for bulkProcessCode : {} image : {} ", bulkProcessCode,
            bulkProcessImage.getImageURL(), e);
        imageDownloadResult.setDownloadSuccess(false);
      }
      CommonUtils.setImageDownloadStatus(bulkProcessImage, imageDownloadResult,
          validationErrorMessage);
    }
    bulkProcessImageService.saveBulkProcessImage(bulkProcessImageList);
  }

  public String setErrorMessageAndGetDownloadLink(BulkProcess bulkProcess, String errorMessage, String downloadLink) {
    if (errorMessage.contains(BulkUpdateServiceUtil.HEADER_MISMATCH)) {
      bulkProcess.setDescription(HEADER_VALIDATION_ERROR);
      bulkProcess.setNotes(errorMessage);
      downloadLink = fileStorageServiceBean.getDownloadLinkHtml(updateProductBasicInfoDownloadLink);
    } else if (errorMessage.contains(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN) || errorMessage.contains(
        BulkUpdateServiceUtil.FILE_BLANK_ERROR)) {
      bulkProcess.setNotes(errorMessage);
      bulkProcess.setDescription(errorMessage.replace(ErrorCategory.VALIDATION.getMessage(), StringUtils.EMPTY));
    }
    return downloadLink;
  }

  public void generateBulkProcessData(BulkProcess bulkProcess, List<Map<String, String>> productDataFromExcel,
      BulkBasicInfoRequest bulkBasicInfoRequest) throws IOException {
    String storeId = bulkProcess.getStoreId();
    int trustedSellerMaxRowSize = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE).getValue());
    int regularSellerMaxRowSize = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE).getValue());
    int regularSellerMinRowSize = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE).getValue());
    log.info("Inserting row data : {} {} ", bulkProcess.getBulkProcessCode(), productDataFromExcel);
    int rowNumber = 5;
    List<BulkProcessData> requestData = new ArrayList<>();
    Map<String, Long> productSkuAndCountMap =
        productDataFromExcel.stream().map(row -> row.get(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER))
            .filter(StringUtils::isNotBlank).map(String::trim)
            .collect(Collectors.groupingBy(sku -> sku, Collectors.counting()));
    Set<String> duplicateProductSkus =
        productSkuAndCountMap.entrySet().stream().filter(entry -> entry.getValue() > 1).map(Map.Entry::getKey)
            .collect(Collectors.toSet());
    Map<String, List<String>> imageUrlToProductCodesMap = new HashMap<>();
    Map<String, List<String>> videoUrlToProductCodesMap = new HashMap<>();
    Map<String, ProductImageAndVideoResponse> productCodeAndProductResponseMap = new HashMap<>();

    for (Map<String, String> userData : productDataFromExcel) {
      String productSku = userData.getOrDefault(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, StringUtils.EMPTY);
      BulkProcessData bulkProcessData = CommonUtils.getBulkProcessData(bulkProcess, rowNumber, productSku);
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(userData));
      if (duplicateProductSkus.contains(productSku)) {
        CommonUtils.addErrorNote(bulkProcess, bulkProcessData, userData, "Duplicate product SKU found");
        requestData.add(bulkProcessData);
        rowNumber++;
        continue;
      }
      ProductBasicResponse productInfo = null;
      ProductImageAndVideoResponse productResponse = null;
      Boolean b2cActivated = null;
      String productCode = StringUtils.EMPTY;
      try {
        productInfo = xProductOutboundService.getProductBasicInfo(storeId, productSku, true);
        productCode = productInfo.getProductCode();
        productResponse =
            pcbOutboundService.getBasicInfoFromPCB(Collections.singletonList(productCode)).get(0);
        b2cActivated = productInfo.isB2cActivated();
        if (CommonUtils.runProductStateValidationChecks(productInfo, bulkProcess, bulkProcessData,
            userData,
            requestData)) {
          rowNumber++;
          continue;
        }
      } catch (Exception e) {
        CommonUtils.addErrorNote(bulkProcess, bulkProcessData, userData, "Failed to fetch product info");
        requestData.add(bulkProcessData);
        rowNumber++;
        continue;
      }
      productCodeAndProductResponseMap.put(productCode, productResponse);
      boolean isValid;
      String errorMessage =
          validateVideoUrlForYoutubeAndVideo(bulkBasicInfoRequest, userData, productResponse);
      isValid = CommonUtils.validaExcelRowData(bulkProcess, requestData, userData, bulkProcessData, b2cActivated,
          errorMessage);
      if (!isValid) {
        rowNumber++;
        continue;
      }
      CommonUtils.mapNewImageUrlsToProductCodes(imageUrlToProductCodesMap, userData, productInfo,
          productResponse, imageStaticBaseUrlPrefix);
      log.info("Image URL to Product Codes Map: {} for bulkProcessCode : {} for fileName : {} ",
          imageUrlToProductCodesMap, bulkProcess.getBulkProcessCode(), bulkBasicInfoRequest.getFileName());
      mapNewVideoUrlsToProductCodes(videoUrlToProductCodesMap, userData, productSku,
          productResponse);
      log.info("Video URL to Product Codes Map: {} for bulkProcessCode : {} for fileName : {} ",
          videoUrlToProductCodesMap, bulkProcess.getBulkProcessCode(), bulkBasicInfoRequest.getFileName());
      rowNumber++;
    }
    saveBulkProcessAndProcessImageAndVideoData(bulkProcess, productDataFromExcel,
        bulkBasicInfoRequest.isTrustedSeller(), trustedSellerMaxRowSize, regularSellerMaxRowSize,
        regularSellerMinRowSize, requestData, imageUrlToProductCodesMap, videoUrlToProductCodesMap, productCodeAndProductResponseMap);
  }

  public String validateVideoUrlForYoutubeAndVideo(BulkBasicInfoRequest bulkBasicInfoRequest,
      Map<String, String> userData, ProductImageAndVideoResponse productImageAndVideoResponse)
      throws IOException {
    final String uploadedUrl = StringUtils.trimToEmpty(userData.get(BulkParameters.VIDEO_URL));
    if (StringUtils.isBlank(uploadedUrl)) {
      return StringUtils.EMPTY;
    }
    boolean isYoutubeUrl = ValidateUrlUtil.validateYouTubeRegex(uploadedUrl, youtubeRegex);
    if (isYoutubeUrl) {
      return validateNewYoutubeUrl(productImageAndVideoResponse, uploadedUrl);
    } else {
      if (!bulkBasicInfoRequest.isProductVideoActivated()) {
        return BulkProcessValidationErrorMessages.VIDEO_URL_MUST_BE_YOUTUBE;
      }
    }
    return StringUtils.EMPTY;
  }

  private String validateNewYoutubeUrl(ProductImageAndVideoResponse productImageAndVideoResponse,
      String uploadedUrl) throws IOException {
    if (StringUtils.equals(uploadedUrl, productImageAndVideoResponse.getUrl())) {
      return StringUtils.EMPTY;
    }
    String videoId =
        ValidateUrlUtil.validateUrlAndGetVideoId(Pattern.compile(youtubeRegex), uploadedUrl);
    boolean isYoutubeActive =
        ValidateUrlUtil.isYouTubeUrlActive(videoId, getYouTubeApiKey(), youTube,
            validateYoutubeUrlSwitchEn);
    if (!isYoutubeActive) {
      return BulkProcessValidationErrorMessages.INVALID_YOUTUBE_URL;
    }
    return StringUtils.EMPTY;
  }

  private String getYouTubeApiKey() {
    String[] apiKeys = youTubeDataApiKey.split(Constant.COMMA);
    int apiKeyIndex = (int) (Math.random() * apiKeys.length);
    return apiKeys[apiKeyIndex];
  }

  private void saveBulkProcessAndProcessImageAndVideoData(BulkProcess bulkProcess,
      List<Map<String, String>> productDataFromExcel, boolean trustedSeller, int trustedSellerMaxRowSize,
      int regularSellerMaxRowSize, int regularSellerMinRowSize, List<BulkProcessData> requestData,
      Map<String, List<String>> imageUrlToProductCodesMap, Map<String, List<String>> videoUrlToProductCodesMap,
      Map<String, ProductImageAndVideoResponse> productCodeAndProductResponseMap) throws JsonProcessingException {
    log.info("Saving row data : {}", requestData);
    this.bulkProcessDataService.saveBulkProcessData(requestData);
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcess.setBulkProcessType(
        CommonUtils.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(bulkProcess.getBulkProcessType(),
            productDataFromExcel.size(), trustedSellerMaxRowSize, regularSellerMinRowSize, regularSellerMaxRowSize,
            trustedSeller));
    List<BulkProcessImage> bulkProcessImageList = new ArrayList<>();
    CommonUtils.generateBulkProcessImageList(bulkProcess, imageUrlToProductCodesMap, bulkProcessImageList,
        productCodeAndProductResponseMap);
    saveAndPublishBulkBasicInfoImageDownloads(bulkProcess, imageUrlToProductCodesMap, bulkProcessImageList);
    List<BulkProcessVideo> bulkProcessVideoList = new ArrayList<>();
    CommonUtils.generateBulkProcessVideoList(bulkProcess, videoUrlToProductCodesMap, bulkProcessVideoList);
    saveAndPublishBulkBasicInfoVideoDownloads(bulkProcess, videoUrlToProductCodesMap, bulkProcessVideoList);
    setBulkProcessStatus(bulkProcess, bulkProcessImageList, bulkProcessVideoList);
    bulkProcess.setTotalCount(productDataFromExcel.size());
    CommonUtils.handleAllFailedScenario(bulkProcess, requestData);
    bulkProcessService.saveOperation(bulkProcess);
  }

  public void setBulkProcessStatus(BulkProcess bulkProcess, List<BulkProcessImage> bulkProcessImageList,
      List<BulkProcessVideo> bulkProcessVideoList) {
    if (CollectionUtils.isNotEmpty(bulkProcessImageList) || CollectionUtils.isNotEmpty(bulkProcessVideoList)) {
      bulkProcess.setStatus(
          BulkCreationCommonUtil.getBasicInfoImageDownloadStatusByPriority(bulkProcess.getBulkProcessType()));
    }
  }

  public void saveAndPublishBulkBasicInfoVideoDownloads(BulkProcess bulkProcess,
      Map<String, List<String>> videoUrlToProductCodesMap, List<BulkProcessVideo> bulkProcessVideoList) {
    if (CollectionUtils.isNotEmpty(bulkProcessVideoList)) {
      bulkProcessVideoService.saveAllBulkProcessVideo(bulkProcessVideoList);
      List<String> videoURLsList = new ArrayList<>(videoUrlToProductCodesMap.keySet());
      for (String videoUrl : videoURLsList) {
        bulkProcessService.publishBulkBasicInfoVideoDownloadEventModel(bulkProcess.getBulkProcessCode(),
            bulkProcess.getBulkProcessType(), videoUrl, bulkProcess.getBusinessPartnerCode());
      }
    }
  }

  public void saveAndPublishBulkBasicInfoImageDownloads(BulkProcess bulkProcess,
      Map<String, List<String>> imageUrlToProductCodesMap, List<BulkProcessImage> bulkProcessImageList) {
    if (CollectionUtils.isNotEmpty(bulkProcessImageList)) {
      bulkProcessImageService.saveBulkProcessImage(bulkProcessImageList);
      List<String> imageURLsList = new ArrayList<>(imageUrlToProductCodesMap.keySet());
      int imageDownloadBatchSize = Integer.parseInt(
          systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
              Constant.IMAGE_DOWNLOAD_BATCH_SIZE).getValue());
      List<List<String>> imageURLsSublist = Lists.partition(imageURLsList, imageDownloadBatchSize);
      for (List<String> subList : imageURLsSublist) {
        bulkProcessService.publishBulkBasicInfoImageDownloadEventModel(bulkProcess.getBulkProcessCode(),
            bulkProcess.getBulkProcessType(), subList);
      }
    }
  }

  public void mapNewVideoUrlsToProductCodes(Map<String, List<String>> videoUrlToProductCodesMap,
      Map<String, String> userData, String productSku, ProductImageAndVideoResponse productImageAndVideoResponse) {
    String uploadedVideoUrl = userData.get(BulkParameters.VIDEO_URL);
    String existingVideoUrl = productImageAndVideoResponse.getVideo();
    String trimmedUploadedVideoUrl = CommonUtils.trimVideoUrlPrefix(uploadedVideoUrl, videoStaticBaseUrlPrefix);
    if (StringUtils.isNotBlank(trimmedUploadedVideoUrl) && !ValidateUrlUtil.validateYouTubeRegex(
        trimmedUploadedVideoUrl, youtubeRegex) && !CommonUtils.isVideoUrlExisting(trimmedUploadedVideoUrl, existingVideoUrl)) {
      videoUrlToProductCodesMap.computeIfAbsent(uploadedVideoUrl, k -> new ArrayList<>()).add(productSku);
    }
  }

  public Sheet validateBulkUpdateRequest(BulkBasicInfoRequest bulkBasicInfoRequest, BulkProcess bulkProcess)
      throws IOException {
    GdnPreconditions.checkArgument(bulkProcess != null, BulkUpdateServiceUtil.BULK_PROCESS_NULL_ERROR);
    // Add file path in the function for new process
    Sheet excelSheetData = fileStorageServiceBean.getFileDataForBasicInfo(bulkBasicInfoRequest, bulkProcess);
    //header validation
    final String storeId = bulkBasicInfoRequest.getStoreId();
    final String bulkProcessCode = bulkBasicInfoRequest.getBulkProcessCode();
    final String businessPartnerCode = bulkBasicInfoRequest.getBusinessPartnerCode();
    Map<Integer, String> headers = POIUtil.readHeadersFromExcel(excelSheetData, 0);
    CommonUtils.validateBasicInfoHeaders(bulkBasicInfoRequest, storeId, bulkProcessCode, businessPartnerCode, headers);
    return excelSheetData;
  }

  @Override
  public void processBulkBasicInfoUpdate(BulkUpdateEventModel bulkUpdateEventModel)
      throws Exception {
    BulkProcess bulkProcess =
        bulkProcessService.findByBulkProcessCode(bulkUpdateEventModel.getStoreId(),
            bulkUpdateEventModel.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList =
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
            bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
            bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    log.info("Fetched {} pending BulkProcessData records for storeId: {}, bulkProcessCode: {} found : {} ",
        bulkProcessDataList.size(), bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkProcessDataList.size());

    ProfileResponse profileResponse =
        productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository()
            .filterByBusinessPartnerCodeV2(bulkUpdateEventModel.getStoreId(),
                bulkUpdateEventModel.getBusinessPartnerCode());

    if (CollectionUtils.isEmpty(bulkProcessDataList)) {
      log.error(
          "No pending bulk process data found for storeId: {}, bulkProcessCode: {}, rowNumbers: {}",
          bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
          bulkUpdateEventModel.getRowNumbers());
      return;
    }

    List<BulkProcessImage> bulkProcessImages =
        bulkProcessImageService.findByStoreIdAndBulkProcess(bulkProcess.getStoreId(), bulkProcess);
    List<BulkProcessVideo> bulkProcessVideo =
        bulkProcessVideoService.findByStoreIdAndBulkProcess(bulkProcess.getStoreId(), bulkProcess);
    Map<String, BulkProcessImage> imageUrlAndBulkProcessImageMap =
        ResponseHelper.buildImageUrlAndBulkProcessImageMap(bulkProcessImages);
    Map<String, BulkProcessVideo> videoUrlAndBulkProcessVideoMap =
        ResponseHelper.buildVideoUrlAndBulkProcessVideoMap(bulkProcessVideo);

    bulkProcessDataList = setInProgressStatus(bulkUpdateEventModel, bulkProcess, bulkProcessDataList);
    if (CollectionUtils.isEmpty(bulkProcessDataList)) {
      return;
    }

    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      try {
        log.info("Processing bulk basic info update for bulkProcessCode : {}, bulkProcessData : {} ",
            bulkProcess.getBulkProcessCode(), bulkProcessData.getRowNumber());
        BulkBasicInfoUpdateRequest request =
            setInitialDataInBulkProcessDataAndGetBasicUpdateRequest(bulkProcessData);
        validateImages(request, imageUrlAndBulkProcessImageMap);
        validateVideoUrl(request, videoUrlAndBulkProcessVideoMap);
        ProductL3Response productL3Response =
            xProductOutboundService.getProductL3DetailsByProductSku(request.getProductSku());
        ProductMasterDataEditRequest productMasterDataEditRequest =
            new ProductMasterDataEditRequest();
        List<ProductLevel3SummaryDetailsImageRequest> productL3CommonImageRequestList =
            ResponseHelper.generateCommonImageRequestForEdit(productL3Response, request,
                imageUrlAndBulkProcessImageMap, imageStaticBaseUrlPrefix, pathPrefix);
        Set<L3InfoUpdateChangeType> changeTypes =
            ResponseHelper.getChangeTypes(request, productL3Response, videoStaticBaseUrlPrefix);
        if (CollectionUtils.isNotEmpty(changeTypes)) {
          log.info("Processing bulk basic info update for productSku: {}, changeTypes: {} ",
              request.getProductSku(), changeTypes);
          ResponseHelper.buildEditRequest(request, videoUrlAndBulkProcessVideoMap,
              productMasterDataEditRequest, productL3CommonImageRequestList, profileResponse,
              changeTypes, productL3Response);
          productMasterDataEditRequest.setProductCode(productL3Response.getProductCode());
          productMasterDataEditRequest.setBusinessPartnerCode(productL3Response.getMerchantCode());
          Optional.ofNullable(productL3Response).map(ProductL3Response::getMasterCatalog)
              .map(MasterCatalogDTO::getCategory).map(CategoryDTO::getCategoryCode)
              .ifPresent(productMasterDataEditRequest::setCategoryCode);
          productMasterDataEditRequest.setUpdatedBy(bulkProcess.getCreatedBy());
          setCategoryName(productL3Response, productMasterDataEditRequest);
          pbpOutboundService.updatedProductMaterData(request.getProductSku(), productMasterDataEditRequest,
              bulkProcess.getCreatedBy());
        }
        bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
        bulkProcessData.setEndDate(new Date());
      } catch (ApplicationRuntimeException ex) {
        String errorMessage =
            ex.getMessage().replace(ErrorCategory.VALIDATION.getMessage(), StringUtils.EMPTY)
                .replace(ErrorCategory.UNSPECIFIED.getMessage(), StringUtils.EMPTY);
        bulkUpdateServiceUtil.setFinalStatusForInputFailure(bulkProcessData, bulkProcess,
            errorMessage, 1, 0);
        log.error("Validation failed for bulkRequestData for bulkProcess code: {}",
            bulkProcessData.getBulkProcessCode(), ex);
      } catch (Exception ex) {
        bulkUpdateServiceUtil.setFinalStatusForInputFailure(bulkProcessData, bulkProcess,
            BulkParameters.SYSTEM_ERROR, 0, 1);
        log.error(
            "System failure while processing BulkProcessData - rowNumber: {}, productSku: {}, bulkProcessCode: {} ",
            bulkProcessData.getRowNumber(), bulkProcessData.getParentProduct(), bulkProcessData.getBulkProcessCode(),
            ex);
      }
    }
    bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  private static void setCategoryName(ProductL3Response productL3Response,
      ProductMasterDataEditRequest productMasterDataEditRequest) {
    Optional.ofNullable(productL3Response).map(ProductL3Response::getItemCatalogs).flatMap(
            itemCatalogs -> itemCatalogs.stream()
                .filter(catalog -> Constants.MASTER_CATALOG.equals(catalog.getCatalogId())).findFirst())
        .map(ItemCatalogDTO::getItemCategories).filter(CollectionUtils::isNotEmpty)
        .map(List::getFirst).map(ItemCategoryDTO::getCategory)
        .ifPresent(productMasterDataEditRequest::setCategoryName);
  }

  private BulkBasicInfoUpdateRequest setInitialDataInBulkProcessDataAndGetBasicUpdateRequest(
      BulkProcessData bulkProcessData) throws JsonProcessingException {
    Map<String, String> requestMap = objectMapper.readValue(bulkProcessData.getBulkRequestData(),
        new TypeReference<Map<String, String>>() {});
    bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
    bulkProcessData.setStartDate(new Date());
    return ResponseHelper.getBulkBasicInfoUpdateRequest(requestMap, youtubeRegex);
  }


  private void validateVideoUrl(BulkBasicInfoUpdateRequest request,
      Map<String, BulkProcessVideo> videoUrlAndBulkProcessVideoMap) {
    if (!request.isYoutubeUrl()) {
      BulkProcessVideo video = videoUrlAndBulkProcessVideoMap.get(request.getVideoUrl());
      if (Objects.nonNull(video) && ResponseHelper.checkVideoForErrors(video)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ResponseHelper.getErrorMessageForBulkProcessVideo(video));
      }
    }
  }

  private static void validateImages(BulkBasicInfoUpdateRequest request,
      Map<String, BulkProcessImage> imageUrlAndBulkProcessImageMap) {
    if (CollectionUtils.isNotEmpty(request.getCommonImages())) {
      for (String imageUrl : request.getCommonImages()) {
        BulkProcessImage bulkProcessImage = imageUrlAndBulkProcessImageMap.get(imageUrl);
        if (Objects.nonNull(bulkProcessImage) && ResponseHelper.checkImageForErrors(
            bulkProcessImage)) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ResponseHelper.getErrorMessageForBulkProcessImage(bulkProcessImage));
        }
      }
    }
  }

  private void createAndSaveBulkProcess(String storeId, String requestId, String bulkProcessCode,
      BulkBasicInfoRequest request) {
    BulkProcess bulkProcess =
        CommonUtils.getBulkProcess(storeId, requestId, bulkProcessCode, request, 0, 0, request.isTrustedSeller());
    bulkProcess.setDescription(request.getFileName() + BulkUpdateServiceUtil.END_SYMBOL
        + ProductLevel3ProcessorServiceBean.DESCRIPTION_PENDING);
    bulkProcess.setUploadedFile(request.getFileName());
    bulkProcessRepository.save(bulkProcess);
  }

  private void publishBasicInfoUploadEvent(BulkBasicInfoRequest request) {
    String topic = request.isTrustedSeller() ?
        kafkaTopicProperties.getBulkBasicInfoUploadPriority1Event() :
        kafkaTopicProperties.getBulkBasicInfoUploadPriority2Event();
    kafkaProducer.send(topic, request);
    log.info("Published event to process basic info for bulkProcessCode: {} , topic : {} , file : {} ",
        request.getBulkProcessCode(), topic, request.getFileName());
  }

  private List<BulkProcessData> setInProgressStatus(BulkUpdateEventModel bulkUpdateEventModel, BulkProcess bulkProcess,
      List<BulkProcessData> bulkProcessDataList) throws Exception {
    try {
      bulkProcessDataList.forEach(bulkProcessData ->
          bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS));
      bulkProcessDataList = bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
    } catch (Exception ex) {
      log.error("Failed to update status to IN_PROGRESS for bulkProcessCode: {}, storeId: {} ",
          bulkUpdateEventModel.getBulkProcessCode(), bulkUpdateEventModel.getStoreId(), ex);
      bulkUpdateServiceUtil.setFinalStatusForSystemFailure(bulkProcessDataList, bulkProcess);
      return null;
    }
    return bulkProcessDataList;
  }
}
