package com.gdn.partners.pcu.external.service.impl.helper;

import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.B2bFieldsRequest;
import com.gda.mta.product.dto.B2BFields;
import com.gda.mta.product.dto.BaseRequest;
import com.gda.mta.product.dto.BuyableScheduleRequest;
import com.gda.mta.product.dto.CopyImageEditRequest;
import com.gda.mta.product.dto.DeletedProductItems;
import com.gda.mta.product.dto.DiscoverableScheduleRequest;
import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.ItemImageEditRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.NeedRevisionSubmitRequest;
import com.gda.mta.product.dto.OmniChannelExistsRequest;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.PickupPointRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.pcu.external.client.model.OmniChannelSkuWebRequest;
import com.gdn.partners.pcu.external.client.model.BulkProcessExternalUploadRequest;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerLogisticsRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemLevel3Request;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductLevel3ImageRequest;
import com.gda.mta.product.dto.ProductLevel3LogisticsRequest;
import com.gda.mta.product.dto.ProductLevel3OrderResponse;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.dto.BulkActivityStatus;
import com.gdn.mta.bulk.dto.BulkProcessDeleteOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessUpsertOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessV2Request;
import com.gdn.mta.bulk.dto.QrExcelUploadRequest;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.partners.pcu.external.client.model.VideoAddEditRequest;
import com.gdn.mta.product.enums.PreOrderType;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pcu.external.client.model.ProductL3UpdateRequest;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerAttributeServiceRequest;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.model.request.ProductItemBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.service.impl.exception.ApplicationException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.service.model.request.UploadAttributeImageRequest;
import com.gdn.partners.pcu.external.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.external.streaming.model.bulk.DownloadType;
import com.gdn.partners.pcu.external.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.external.streaming.model.bulk.ProductBasicInfoDownloadRequest;
import com.gdn.partners.pcu.external.web.BulkActivityStatusWeb;
import com.gdn.partners.pcu.external.web.model.enums.AllowedNumberOfQRPerPage;
import com.gdn.partners.pcu.external.web.model.enums.MerchantType;
import com.gdn.partners.pcu.external.web.model.request.ActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.AppealProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.DefaultConfigurationAndPickupPointRequest;
import com.gdn.partners.pcu.external.web.model.request.DeletedProductItemsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.HistorySummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InProcessProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemImageEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemLevel4WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointDeleteWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ManualQRCodeRequest;
import com.gdn.partners.pcu.external.web.model.request.NeedRevisionSubmitWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointUpdateItemsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBundleRecipeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBundleWebRecipe;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductImageEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemLevel3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemUomInfoDTO;
import com.gdn.partners.pcu.external.web.model.request.ProductItemUomInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemWholesalePriceWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductL3ListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3AttributeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3CommonImageWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3ImageWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3LogisticsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3PriceWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3SummaryDetailsImageWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3UpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3VariantsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3ViewConfigWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductPriceAndStockUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductPriceStockAndImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSizeChartUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantPriceStockAndImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.SuspensionWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpdateItemsPriceStockImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.WholeSaleDetailListWebRequest;
import com.gdn.partners.pcu.external.web.model.response.ApiErrorCode;
import com.gdn.partners.pcu.external.web.model.response.BulkProcessStatusListingWebResponse;
import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.x.businesspartner.dto.ProductSettingDTO;
import com.gdn.x.businesspartner.dto.ProfileRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.MarkPickupPointAsDefaultRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSizeChartUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.Jsoup;
import org.jsoup.safety.Whitelist;
import org.springframework.http.HttpStatus;
import org.springframework.web.multipart.MultipartFile;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static com.gdn.partners.pcu.external.model.Accessibilty.TRANSFER_PRODUCT_LISTING;
import static java.util.stream.Collectors.toList;

/**
 * @author Pradeep Reddy
 */
@Slf4j
public class RequestHelper {

  private static final String PRIVILEGED_EDIT_O2O = "isPrivilegedToEditO2O";
  private static final String BULK_PROCESS_TYPE = "ProductLevel3";
  public static final String DEFAULT_CLIENT_HOST = "MTA_WEB";
  private static final String SUBMITTED_DATE = "submittedDate";
  private static final String IFRAME_REGEX = "(?:<iframe[^>]*)(?:(?:\\/>)|(?:>.*?<\\/iframe>))";
  private static final String SRC_REGEX = "(?<=src=\").*?(?=[\\?\"])";
  private static final String IFRAME = "iframe";
  private static final String YOUTUBE_URL_LINK = "www.youtube.com";
  private static final String YOUTUBE_MOBILE_LINK = "youtu.be";
  private static final int DAYS_IN_YEAR = 365;
  private static final int DAYS_IN_MONTH = 30;
  private static final int HOURS_IN_DAY = 24;
  private static final long SIZE_IN_BYTES = 1024 * 1024;
  public static final String ASSEMBLY_REQUEST = "ASSEMBLY_REQUEST";
  public static final String DISASSEMBLY_REQUEST = "DISASSEMBLY_REQUEST";
  public static final String TRANSFER_REQUEST = "TRANSFER_REQUEST";
  private static final String LANGUAGE = "en";
  private static final String ACTIVE = "ACTIVE";

  public static ProductBusinessPartnerRequest toProductBusinessPartnerRequest(
      ProductBusinessPartnerServiceRequest productBusinessPartnerServiceRequest) {
    ProductBusinessPartnerRequest productBusinessPartnerRequest =
        new ProductBusinessPartnerRequest();
    BeanUtils.copyProperties(productBusinessPartnerServiceRequest, productBusinessPartnerRequest);
    productBusinessPartnerRequest.setBusinessPartnerId(productBusinessPartnerServiceRequest.getBusinessPartnerCode());
    productBusinessPartnerRequest.setProductItemBusinessPartners(
        toProductItemBusinessPartnerListRequest(
            productBusinessPartnerServiceRequest.getStoreId(), productBusinessPartnerServiceRequest.getProductItemBusinessPartners()));
    productBusinessPartnerRequest.setProductBusinessPartnerAttributes(
        toProductBusinessPartnerAttributeListRequest(
            productBusinessPartnerServiceRequest.getStoreId(), productBusinessPartnerServiceRequest.getProductBusinessPartnerAttributes()));
    productBusinessPartnerRequest.setCreatedDate(productBusinessPartnerServiceRequest.getCreatedDate());
    productBusinessPartnerRequest.setCreatedBy(productBusinessPartnerServiceRequest.getUsername());
    productBusinessPartnerRequest.setUpdatedDate(productBusinessPartnerServiceRequest.getCreatedDate());
    productBusinessPartnerRequest.setUpdatedBy(productBusinessPartnerServiceRequest.getUsername());
    return productBusinessPartnerRequest;
  }

  public static ActiveProductRequest toActiveProductRequest(ActiveProductWebRequest activeProductWebRequest,
      List<String> categoryCodes) {
    ActiveProductRequest activeProductRequest = new ActiveProductRequest();
    BeanUtils.copyProperties(activeProductWebRequest, activeProductRequest);
    activeProductRequest.setCategoryCodes(categoryCodes);
    return activeProductRequest;
  }

  public static ActiveProductRequest toActiveProductRequest(ProductL3ListingWebRequest webRequest) {
    ActiveProductRequest activeProductRequest = new ActiveProductRequest();
    BeanUtils.copyProperties(webRequest, activeProductRequest);
    activeProductRequest.setBundleProduct(webRequest.getBundlingProduct());
    return activeProductRequest;
  }


  public static SummaryFilterRequest toSummaryFilterRequest(SuspensionWebRequest suspensionWebRequest) {
    SummaryFilterRequest summaryFilterRequest =
        SummaryFilterRequest.builder().businessPartnerCode(suspensionWebRequest.getBusinessPartnerCode())
            .categoryCode(suspensionWebRequest.getCategoryCode()).nameKey(suspensionWebRequest.getNameKey())
            .searchKeyword(suspensionWebRequest.getSkuKey()).sortType(suspensionWebRequest.getSortType()).build();
    summaryFilterRequest.setSearchKeyword(suspensionWebRequest.getSkuKey());
    return summaryFilterRequest;
  }

  private static List<ProductItemBusinessPartnerRequest> toProductItemBusinessPartnerListRequest(
      String storeId, List<ProductItemBusinessPartnerServiceRequest> productItemBusinessPartnerServiceRequestList) {
    return productItemBusinessPartnerServiceRequestList.stream()
        .map(productItemBusinessPartnerServiceRequest -> toProductItemBusinessPartnerRequest(storeId, productItemBusinessPartnerServiceRequest))
        .collect(Collectors.toList());
  }

  private static ProductItemBusinessPartnerRequest toProductItemBusinessPartnerRequest(
      String storeId, ProductItemBusinessPartnerServiceRequest productItemBusinessPartnerServiceRequest) {
    ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest =
        new ProductItemBusinessPartnerRequest();
    BeanUtils.copyProperties(productItemBusinessPartnerServiceRequest, productItemBusinessPartnerRequest);
    if (CollectionUtils.isNotEmpty(productItemBusinessPartnerServiceRequest.getProductItemWholesalePriceRequests())) {
      productItemBusinessPartnerRequest.setProductItemWholesalePriceRequests(
          productItemBusinessPartnerServiceRequest.getProductItemWholesalePriceRequests().stream().map(
              productItemWholesalePriceWebRequest -> new ProductItemWholesalePriceRequest(
                  productItemWholesalePriceWebRequest.getQuantity(),
                  productItemWholesalePriceWebRequest.getWholesaleDiscount())).collect(toList()));
    }
    if (CollectionUtils.isNotEmpty(
        productItemBusinessPartnerServiceRequest.getProductItemLogisticsWebRequests())) {
      productItemBusinessPartnerRequest.setProductItemBusinessPartnerLogisticsRequests(
          productItemBusinessPartnerServiceRequest.getProductItemLogisticsWebRequests().stream()
              .map(logistics -> new ProductItemBusinessPartnerLogisticsRequest(
                  logistics.getLogisticProductCode(), logistics.isSelected()))
              .collect(Collectors.toList()));
    }
    productItemBusinessPartnerRequest.setStoreId(storeId);
    return productItemBusinessPartnerRequest;
  }

  private static ProductBusinessPartnerAttributeRequest toProductBusinessPartnerAttributeRequest(
      String storeId, ProductBusinessPartnerAttributeServiceRequest productBusinessPartnerAttributeServiceRequest) {
    ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest =
        new ProductBusinessPartnerAttributeRequest();
    BeanUtils.copyProperties(productBusinessPartnerAttributeServiceRequest,
        productBusinessPartnerAttributeRequest);
    productBusinessPartnerAttributeRequest.setStoreId(storeId);
    return productBusinessPartnerAttributeRequest;
  }

  private static List<ProductBusinessPartnerAttributeRequest>
  toProductBusinessPartnerAttributeListRequest(
      String storeId, List<ProductBusinessPartnerAttributeServiceRequest>
          productBusinessPartnerAttributeServiceRequestList) {
    return productBusinessPartnerAttributeServiceRequestList.stream().map(
        toProductBusinessPartnerAttributeRequest -> toProductBusinessPartnerAttributeRequest(
            storeId, toProductBusinessPartnerAttributeRequest))
        .collect(Collectors.toList());
  }


  public static String generatePath(String brandName, MultipartFile file) throws Exception {
    String fileType =
      file.getOriginalFilename().substring(file.getOriginalFilename().lastIndexOf(Constants.DOT_SEPARATOR) + 1);
    String lowercaseFilename = brandName.replaceAll(Constants.SPECIAL_CHARS_REGEX, Constants.DASH_SEPARATOR).toLowerCase();
    return lowercaseFilename.substring(0, Math.min(lowercaseFilename.length(), Constants.FILENAME_MAX_LENGTH))
      + Constants.DOT_SEPARATOR + fileType;
  }

  public static void createFile(MultipartFile newFile, String locationPath) throws Exception {
    File file = new File(locationPath);
    File directory = file.getParentFile();
    if (!directory.exists()) {
      directory.mkdirs();
    }
    byte[] bytes = newFile.getBytes();
    try(BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(new FileOutputStream(file))) {
      bufferedOutputStream.write(bytes);
    }
  }

  public static ProductLevel3SummaryRequest toProductLevel3SummaryRequestFromInActiveProductWebRequestTest (
      InActiveProductWebRequest inActiveProductWebRequest) {
    ProductLevel3SummaryRequest summaryRequest = new ProductLevel3SummaryRequest();
    BeanUtils.copyProperties(inActiveProductWebRequest, summaryRequest);
    summaryRequest.setArchived(true);
    if(!CollectionUtils.isEmpty(inActiveProductWebRequest.getCategoryCodes())){
      summaryRequest.setCategoryCodes(inActiveProductWebRequest.getCategoryCodes().stream()
          .filter(StringUtils::isNotBlank).collect(toList()));
    }
    if(!CollectionUtils.isEmpty(inActiveProductWebRequest.getPickupPointCodes())){
      summaryRequest.setPickupPointCodes(inActiveProductWebRequest.getPickupPointCodes().stream()
          .filter(StringUtils::isNotBlank).collect(toList()));
    }
    return summaryRequest;
  }

  public static ProductLevel3SummaryRequest toProductLevel3SummaryRequest (
      ActiveProductWebRequest activeProductWebRequest) {
    ProductLevel3SummaryRequest summaryRequest = new ProductLevel3SummaryRequest();
    BeanUtils.copyProperties(activeProductWebRequest, summaryRequest);
    if (!CollectionUtils.isEmpty(activeProductWebRequest.getCategoryCodes())) {
      summaryRequest.setCategoryCodes(
          activeProductWebRequest.getCategoryCodes().stream().filter(StringUtils::isNotBlank).collect(toList()));
    }
    if (!CollectionUtils.isEmpty(activeProductWebRequest.getPickupPointCodes())) {
      summaryRequest.setPickupPointCodes(
          activeProductWebRequest.getPickupPointCodes().stream().filter(StringUtils::isNotBlank).collect(toList()));
    }
    if (ProductLevel3InventoryCriteria.AVAILABLE.toString()
        .equalsIgnoreCase(activeProductWebRequest.getInventoryFilter())) {
      summaryRequest.setInventoryFilter(ProductLevel3InventoryCriteria.AVAILABLE);
    } else if (ProductLevel3InventoryCriteria.STOCK_ALERT.toString()
        .equalsIgnoreCase(activeProductWebRequest.getInventoryFilter())) {
      summaryRequest.setInventoryFilter(ProductLevel3InventoryCriteria.STOCK_ALERT);
    } else if (ProductLevel3InventoryCriteria.OOS.toString()
        .equalsIgnoreCase(activeProductWebRequest.getInventoryFilter())) {
      summaryRequest.setInventoryFilter(ProductLevel3InventoryCriteria.OOS);
    }

    return summaryRequest;
  }

  public static ProductLevel3SummaryRequest toProductLevel3SummaryRequest(String productSku) {
    ProductLevel3SummaryRequest summaryRequest = new ProductLevel3SummaryRequest();
    summaryRequest.setProductSkuList(Arrays.asList(productSku));
    summaryRequest.setIgnoreArchive(true);
    return summaryRequest;
  }

  public static SummaryFilterRequest toSummaryFilterRequestFromInActiveProductWebRequest(
      InActiveProductWebRequest inActiveProductWebRequest) {
    SummaryFilterRequest summaryFilterRequest =
        SummaryFilterRequest.builder().businessPartnerCode(inActiveProductWebRequest.getBusinessPartnerCode())
            .categoryCodes(inActiveProductWebRequest.getCategoryCodes())
            .pickupPointCodes(inActiveProductWebRequest.getPickupPointCodes())
            .searchKeyword(inActiveProductWebRequest.getSearchKey())
            .sortType(inActiveProductWebRequest.getOrderBy())
            .sortOrder(inActiveProductWebRequest.getSortBy()).build();
    return summaryFilterRequest;
  }

  public static ProductLevel3WipSummaryRequest toProductLevel3WipSummaryRequest(
      InProcessProductWebRequest inProcessProductWebRequest) {
    ProductLevel3WipSummaryRequest productLevel3WipSummaryRequest = new ProductLevel3WipSummaryRequest();
    BeanUtils.copyProperties(inProcessProductWebRequest, productLevel3WipSummaryRequest);
    if (StringUtils.isEmpty(inProcessProductWebRequest.getOrderBy()) || Objects.isNull(inProcessProductWebRequest.getOrderBy())) {
      productLevel3WipSummaryRequest.setOrderBy(SUBMITTED_DATE);
    }
    if (Objects.nonNull(inProcessProductWebRequest.getSearchKey())) {
      productLevel3WipSummaryRequest.setProductName(inProcessProductWebRequest.getSearchKey());
    }
    if (ProductLevel3WipSummaryCriteria.IN_PROGRESS.toString().equals(inProcessProductWebRequest.getCriteria())) {
      productLevel3WipSummaryRequest.setCriteria(ProductLevel3WipSummaryCriteria.IN_PROGRESS);
    } else if (ProductLevel3WipSummaryCriteria.NEED_CORRECTION.toString()
        .equals(inProcessProductWebRequest.getCriteria())) {
      productLevel3WipSummaryRequest.setCriteria(ProductLevel3WipSummaryCriteria.NEED_CORRECTION);
    } else if (ProductLevel3WipSummaryCriteria.FAILED.toString().equals(inProcessProductWebRequest.getCriteria())) {
      productLevel3WipSummaryRequest.setCriteria(ProductLevel3WipSummaryCriteria.FAILED);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_REQUEST);
    }
    return productLevel3WipSummaryRequest;
  }

  public static void setProductOff2OnFlagNullAndRoundOffSalePrice(ProductLevel3OrderResponse productLevel3OrderResponse,
      ProductLevel3UpdateSummaryRequest request) {
    if (Objects.nonNull(productLevel3OrderResponse) && CollectionUtils
        .isNotEmpty(productLevel3OrderResponse.getItems()) && (
        Objects.isNull(productLevel3OrderResponse.getItems().get(0).getOff2OnActiveFlag())
            || productLevel3OrderResponse.getItems().get(0).getOff2OnActiveFlag()
            .equals(request.getOff2OnActiveFlag()))) {
      log.info("Updating O2O flag value, SaveValue :{}, new Value :{}",
          productLevel3OrderResponse.getItems().get(0).getOff2OnActiveFlag(), request.getOff2OnActiveFlag());
      request.setOff2OnActiveFlag(null);
    }
    request.getPrices().get(0)
        .setSalePrice((double) Math.round(request.getPrices().get(0).getSalePrice()));
  }

  public static BulkProcessUpdateRequest toBulkProcessUpdateRequest(String businessPartnerCode, String username,
      Map<String, Boolean> privilegeMap, MultipartFile request, ProfileResponse businessPartner, String bulkProcessType)
      throws IOException {
    BulkProcessUpdateRequest bulkProcessUpdateRequest = new BulkProcessUpdateRequest();
    bulkProcessUpdateRequest.setBusinessPartnerCode(businessPartnerCode);
    bulkProcessUpdateRequest.setBulkProcessType(bulkProcessType);
    bulkProcessUpdateRequest.setFileName(request.getOriginalFilename());
    bulkProcessUpdateRequest.setFileContent(request.getBytes());
    bulkProcessUpdateRequest.setPrivilegedMap(privilegeMap);
    Boolean privilegeEditO2O = bulkProcessUpdateRequest.getPrivilegedMap().get(PRIVILEGED_EDIT_O2O);
    boolean updatedPrivilegeEditO2O =
        Objects.nonNull(privilegeEditO2O) && privilegeEditO2O && businessPartner.getCompany().isOfflineToOnlineFlag();
    bulkProcessUpdateRequest.getPrivilegedMap().put(PRIVILEGED_EDIT_O2O, updatedPrivilegeEditO2O);
    bulkProcessUpdateRequest.setClientHost(DEFAULT_CLIENT_HOST);
    bulkProcessUpdateRequest.setUpdatedBy(username);
    return bulkProcessUpdateRequest;
  }

  public static BulkProcessV2Request toBulkProcessV2Request(String businessPartnerCode, String username,
    Map<String, Boolean> privilegeMap, MultipartFile request, ProfileResponse businessPartner,
    Set<String> accessiblePickupPointCodes, String bulkProcessType)
    throws IOException {
    BulkProcessV2Request bulkProcessV2Request = new BulkProcessV2Request();
    bulkProcessV2Request.setAccessiblePickupPoints(accessiblePickupPointCodes);
    bulkProcessV2Request.setBusinessPartnerCode(businessPartnerCode);
    bulkProcessV2Request.setBulkProcessType(bulkProcessType);
    bulkProcessV2Request.setFileName(request.getOriginalFilename());
    bulkProcessV2Request.setFileContent(request.getBytes());
    bulkProcessV2Request.setPrivilegedMap(privilegeMap);
    boolean privilegeEditO2O =
      bulkProcessV2Request.getPrivilegedMap().getOrDefault(PRIVILEGED_EDIT_O2O, false)
        && businessPartner.getCompany().isOfflineToOnlineFlag();
    bulkProcessV2Request.getPrivilegedMap().put(PRIVILEGED_EDIT_O2O, privilegeEditO2O);
    bulkProcessV2Request.setClientHost(DEFAULT_CLIENT_HOST);
    bulkProcessV2Request.setUpdatedBy(username);
    return bulkProcessV2Request;
  }

  public static BulkBasicInfoRequest toBulkBasicInfoRequest(String businessPartnerCode, String username,
      BulkBasicInfoWebRequest request, ProfileResponse businessPartner, String bulkProcessType,
      boolean productVideoActivated) throws IOException {
    BulkBasicInfoRequest bulkBasicInfoRequest = new BulkBasicInfoRequest();
    bulkBasicInfoRequest.setBusinessPartnerCode(businessPartnerCode);
    bulkBasicInfoRequest.setBulkProcessType(bulkProcessType);
    bulkBasicInfoRequest.setFileName(request.getFileName());
    bulkBasicInfoRequest.setFilePath(request.getFilePath());
    bulkBasicInfoRequest.setBulkProcessCode(request.getBulkProcessCode());
    bulkBasicInfoRequest.setInstoreSeller(businessPartner.getCompany().isOfflineToOnlineFlag());
    bulkBasicInfoRequest.setTrustedSeller(businessPartner.isTrustedSeller());
    bulkBasicInfoRequest.setUpdatedBy(username);
    bulkBasicInfoRequest.setInternationalMerchant(businessPartner.getCompany().isInternationalFlag());
    bulkBasicInfoRequest.setProductVideoActivated(productVideoActivated);
    return bulkBasicInfoRequest;
  }


  public static BulkProcessUpsertOfflineItemRequest toBulkProcessUpsertOfflineItemRequest(String businessPartnerCode,
      String username, MultipartFile request, Set<String> accessiblePickupPoints) throws IOException {
    BulkProcessUpsertOfflineItemRequest bulkProcessUpsertOfflineItemRequest = new BulkProcessUpsertOfflineItemRequest();
    bulkProcessUpsertOfflineItemRequest.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue());
    bulkProcessUpsertOfflineItemRequest.setBusinessPartnerCode(businessPartnerCode);
    bulkProcessUpsertOfflineItemRequest.setFileContent(request.getBytes());
    bulkProcessUpsertOfflineItemRequest.setFileName(request.getOriginalFilename());
    bulkProcessUpsertOfflineItemRequest.setUpdatedBy(username);
    bulkProcessUpsertOfflineItemRequest.setClientHost(DEFAULT_CLIENT_HOST);
    bulkProcessUpsertOfflineItemRequest.setPrivilegedMap(Collections.emptyMap());
    bulkProcessUpsertOfflineItemRequest.setAccessiblePickupPoints(accessiblePickupPoints);
    return bulkProcessUpsertOfflineItemRequest;
  }


  public static boolean checkPrivilegeEditO2O(ProfileResponse businessPartner, Map<String, Boolean> privilegeMap) {
    Boolean privilegeEditO2O = privilegeMap.get(PRIVILEGED_EDIT_O2O);
    return Objects.nonNull(privilegeEditO2O) && privilegeEditO2O && businessPartner.getCompany()
        .isOfflineToOnlineFlag();
  }

  public static BulkProcessUploadRequest toBulkProcessUploadRequest(String businessPartnerCode,
      Map<String, String> args, Map<String, String> files) throws IOException {
    BulkProcessUploadRequest bulkProcessUploadRequest = new BulkProcessUploadRequest();
    bulkProcessUploadRequest.setBusinessPartnerCode(businessPartnerCode);
    bulkProcessUploadRequest.setBulkProcessType(BULK_PROCESS_TYPE);
    bulkProcessUploadRequest.setArgs(args);
    bulkProcessUploadRequest.setFiles(files);
    return bulkProcessUploadRequest;
  }

  public static BulkProcessExternalUploadRequest toBulkProcessExternalUploadRequest(
      String zipFileName, Map<String, String> files, String bulkProcessCode, String pickuPointCode,
      String onlyExternalUser, String businessPartnerCode) {
    BulkProcessExternalUploadRequest
        bulkProcessExternalUploadProcess = new BulkProcessExternalUploadRequest();
    bulkProcessExternalUploadProcess.setBulkProcessCode(bulkProcessCode);
    bulkProcessExternalUploadProcess.setOnlyExternalUser(onlyExternalUser);
    bulkProcessExternalUploadProcess.setFiles(files);
    bulkProcessExternalUploadProcess.setPickupPointCode(pickuPointCode);
    bulkProcessExternalUploadProcess.setZipFileName(zipFileName);
    bulkProcessExternalUploadProcess.setBusinessPartnerCode(businessPartnerCode);
    return bulkProcessExternalUploadProcess;
  }

  public static Map<String, Boolean> getEditAccessibilities() {
    Map<String, Boolean> privilegeMap = new HashMap<>();
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    log.info("Creating privilege map from the accessibilties : {}", accessibilties);
    if (accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_O2O_CHANGE_DATA)) {
      privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_O2O, true);
    }
    if (accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA)) {
      privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK, true);
    }
    if (accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_DISPLAY_BUYABLE_CHANGE_DATA)) {
      privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, true);
    }
    if (accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_PICKUP_POINT_CHANGE_DATA)) {
      privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_PICKUP_POINT, true);
    }
    if (accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA)) {
      privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_PRICE, true);
    }
    return privilegeMap;
  }

  public static Map<String, Boolean> getReadAccessibilities(ProfileResponse profileResponse, boolean isOnlyExternalUser){
    Map<String, Boolean> privilegeMap = new HashMap<>();
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    if(accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_O2O_VIEW_ONLY)){
      privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_READ_O2O, true);
    }
    if(accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_VIEW_ONLY)){
      privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_READ_AVAILABLE_STOCK, true);
    }
    if(accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_DISPLAY_BUYABLE_VIEW_ONLY)){
      privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_READ_DISPLAY_BUYABLE, true);
    }
    if(accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_PICKUP_POINT_VIEW_ONLY)){
      privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_READ_PICKUP_POINT, true);
    }
    if(accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_VIEW_ONLY)){
      privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_READ_PRICE, true);
    }
    boolean isPrivilegedToReadWarehouseStock =
        profileResponse.getCompany().getInventoryFulfillment().equalsIgnoreCase(Constants.INVENTORY_FULFILLMENT_BLIBLI)
            && (!isOnlyExternalUser || ExcelTemplateUtil.isWareHouseMerchant(profileResponse.getCompany().getMerchantType()));
    privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_READ_WAREHOUSE_STOCK, isPrivilegedToReadWarehouseStock);
    return privilegeMap;
  }

  public static ProductLevel3Request toProductLevel3RequestFromProductLevel3WebRequest
      (ProductLevel3WebRequest productLevel3WebRequest){
    ProductLevel3Request productLevel3Request = new ProductLevel3Request();
    BeanUtils.copyProperties(productLevel3WebRequest,productLevel3Request);
    productLevel3Request.setItems(productLevel3WebRequest.getItems().stream().map(items ->
        toProductItemLevel3RequesFromProductItemLevel3Request(items)).collect(toList()));
    productLevel3Request.setAttributes(productLevel3WebRequest.getAttributes().stream().map(attributes ->
        toProductLevel3AttributeRequestFromProductLevel3AttributeWebRequest(attributes)).collect(toList()));
    productLevel3Request.setImages(productLevel3WebRequest.getImages().stream().map(image ->
        toProductLevel3ImageRequestFromProductLevel3ImageWebRequest(image)).collect(toList()));
    if (CollectionUtils.isNotEmpty(productLevel3WebRequest.getLogistics())) {
      productLevel3Request
          .setProductLevel3LogisticsRequest(
              productLevel3WebRequest.getLogistics().stream()
                  .map(logistics -> new ProductLevel3LogisticsRequest(
                      logistics.getLogisticProductCode(), logistics.isSelected()))
                  .collect(Collectors.toList()));
    }
    return productLevel3Request;
  }

  public static ProductLevel3Request toProductLevel3RequestFromProductEditInfoWebRequest(
      ProductEditInfoWebRequest productEditInfoWebRequest) {
    ProductLevel3Request productLevel3Request = new ProductLevel3Request();
    BeanUtils.copyProperties(productEditInfoWebRequest, productLevel3Request);
    productLevel3Request.setAttributes(productEditInfoWebRequest.getAttributes().stream()
        .map(attributes -> toProductLevel3AttributeRequestFromProductLevel3AttributeWebRequest(attributes))
        .collect(toList()));
    productLevel3Request.setImages(productEditInfoWebRequest.getImages().stream()
        .map(image -> toProductLevel3ImageRequestFromProductLevel3ImageWebRequest(image)).collect(toList()));
    return productLevel3Request;
  }

  public static ProductL3UpdateRequest toProductL3RequestFromProductEditInfoWebRequest(
    ProductEditInfoV2WebRequest editInfoV2WebRequest, boolean ranchIntegrationEnabled) {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    BeanUtils.copyProperties(editInfoV2WebRequest, productL3UpdateRequest);
    productL3UpdateRequest.setAttributes(editInfoV2WebRequest.getAttributes().stream()
      .map(RequestHelper::toProductLevel3AttributeRequestFromProductLevel3AttributeWebRequest)
      .collect(toList()));
    productL3UpdateRequest.setCommonImages(editInfoV2WebRequest.getCommonImages().stream()
      .map(RequestHelper::toProductLevel3CommonImageRequestFromProductL3CommonImageWebRequest)
      .collect(toList()));
    productL3UpdateRequest
      .setAddPickupPoints(toItemPickupPointRequest(editInfoV2WebRequest.getAddPickupPoints()));
    productL3UpdateRequest.setSizeChartCode(editInfoV2WebRequest.getSizeChartCode());
    productL3UpdateRequest.setSizeChartChanged(editInfoV2WebRequest.isSizeChartChanged());
    productL3UpdateRequest.setDeletePickupPoints(
      toItemPickupPointDeleteRequest(editInfoV2WebRequest.getDeletePickupPoints()));
    productL3UpdateRequest.setDeletedProductItems(toDeletedProductItems(editInfoV2WebRequest.getDeletedProductItems()));
    productL3UpdateRequest.setProductItems(
      toProductVariantPriceStockAndImagesRequest(editInfoV2WebRequest.getProductItems()));
    if (CollectionUtils.isNotEmpty(editInfoV2WebRequest.getLogistics())) {
      productL3UpdateRequest.setProductLevel3LogisticsRequest(
        editInfoV2WebRequest.getLogistics().stream()
          .map(logisticsWebRequest -> toProductLevel3Logistics(logisticsWebRequest))
          .collect(toList()));
    }
    if (Objects.nonNull(editInfoV2WebRequest.getPreOrder())) {
      PreOrderRequest preOrderRequest = new PreOrderRequest();
      BeanUtils.copyProperties(editInfoV2WebRequest.getPreOrder(), preOrderRequest);
      productL3UpdateRequest.setPreOrder(preOrderRequest);
    }
    if (CollectionUtils.isNotEmpty(editInfoV2WebRequest.getProductBundleRecipe())) {
      List<ProductBundleRecipeRequest> productBundleRecipeRequestList = new ArrayList<>();
      for (ProductBundleRecipeWebRequest productBundleRecipeWebRequest : editInfoV2WebRequest.getProductBundleRecipe()) {
        ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
        productBundleRecipeRequest.setItemSku(productBundleRecipeWebRequest.getItemSku());
        productBundleRecipeRequest.setBundleRecipe(toBundleRecipe(productBundleRecipeWebRequest));
        productBundleRecipeRequestList.add(productBundleRecipeRequest);
      }
      productL3UpdateRequest.setProductBundleRecipe(productBundleRecipeRequestList);
    }
    productL3UpdateRequest.setVideoUpdated(editInfoV2WebRequest.getVideoUpdated());
    productL3UpdateRequest.setVideoAddEditRequest(
        toVideoAddEditRequest(editInfoV2WebRequest.getVideoAddEditRequest()));
    if (ranchIntegrationEnabled) {
      productL3UpdateRequest.setDistributionInfoRequest(editInfoV2WebRequest.getDistributionInfoRequest());
      productL3UpdateRequest.setDistributionAndUOMRequest(
          Optional.ofNullable(editInfoV2WebRequest.getDistributionAndUOMRequest()).orElse(new ArrayList<>()).stream()
              .map(RequestHelper::convertToProductItemUomInfoDTO).collect(toList()));
    }
    return productL3UpdateRequest;
  }

  public static ProductItemUomInfoDTO convertToProductItemUomInfoDTO(
      ProductItemUomInfoWebRequest productItemUomInfoWebRequest) {
    ProductItemUomInfoDTO productItemUomInfoDTO = new ProductItemUomInfoDTO();
    productItemUomInfoDTO.setSkuCode(productItemUomInfoWebRequest.getSkuCode());
    productItemUomInfoDTO.setDistributionItemInfoRequest(
        productItemUomInfoWebRequest.getDistributionItemInfoRequest());
    productItemUomInfoDTO.setDimensionsAndUOMRequest(productItemUomInfoWebRequest.getDimensionsAndUOMRequest());
    return productItemUomInfoDTO;
  }

  public static VideoAddEditRequest toVideoAddEditRequest(
      com.gdn.partners.pcu.external.web.model.request.VideoAddEditRequest videoAddEditWebRequest) {
    if(Objects.nonNull(videoAddEditWebRequest)) {
      VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
      BeanUtils.copyProperties(videoAddEditWebRequest, videoAddEditRequest);
      return videoAddEditRequest;
    }
    return null;
  }

  private static Set<ProductBundleRecipe> toBundleRecipe(ProductBundleRecipeWebRequest productBundleRecipeWebRequest) {
    Set<ProductBundleRecipe> productBundleRecipeSet = new HashSet<>();
    for (ProductBundleWebRecipe productBundleWebRecipe : productBundleRecipeWebRequest.getBundleRecipe()) {
      ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
      productBundleRecipe.setItemSku(productBundleWebRecipe.getItemSku());
      productBundleRecipe.setQuantity(productBundleWebRecipe.getQuantity());
      productBundleRecipeSet.add(productBundleRecipe);
    }
    return productBundleRecipeSet;
  }

  public static List<ItemPickupPointRequest> toItemPickupPointRequest(
    List<ItemPickupPointWebRequest> itemPickupPointWebRequests) {
    return Optional.ofNullable(itemPickupPointWebRequests).orElse(new ArrayList<>()).stream()
      .map(RequestHelper::toItemPickupPointRequest).collect(toList());
  }

  public static List<PickupPointDeleteRequest> toItemPickupPointDeleteRequest(
    List<ItemPickupPointDeleteWebRequest> deleteWebRequests) {
    return Optional.ofNullable(deleteWebRequests).orElse(new ArrayList<>()).stream()
      .map(RequestHelper::toPickupPointDeleteRequest).collect(toList());
  }

  public static List<DeletedProductItems> toDeletedProductItems(
      List<DeletedProductItemsWebRequest> deletedProductItems) {
    return Optional.ofNullable(deletedProductItems).orElse(new ArrayList<>()).stream()
        .map(RequestHelper::toDeletedProductItem).collect(toList());
  }

  public static DeletedProductItems toDeletedProductItem(DeletedProductItemsWebRequest deletedProductItemsWebRequest) {
    DeletedProductItems deletedProductItems = new DeletedProductItems();
    BeanUtils.copyProperties(deletedProductItemsWebRequest, deletedProductItems);
    return deletedProductItems;
  }

  public static List<ProductLevel3SummaryDetailsImageRequest> toCopyToAllVariantImagesRequest(
    List<ProductLevel3SummaryDetailsImageWebRequest> productLevel3SummaryDetailsImageWebRequests) {
    return Optional.ofNullable(productLevel3SummaryDetailsImageWebRequests)
      .orElse(new ArrayList<>()).stream()
      .map(RequestHelper::toProductLevel3SummaryDetailsImageRequest).collect(toList());
  }

  public static List<ProductVariantPriceStockAndImagesRequest> toProductVariantPriceStockAndImagesRequest(
    List<ProductVariantPriceStockAndImagesWebRequest> webRequest) {
    List<ProductVariantPriceStockAndImagesRequest> requests = new ArrayList<>();
    for (ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest : webRequest) {
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
      BeanUtils.copyProperties(productVariantPriceStockAndImagesWebRequest,
        productVariantPriceStockAndImagesRequest, "images", "modifiedItemPickupPoints");
      productVariantPriceStockAndImagesRequest.setImages(
        Optional.ofNullable(productVariantPriceStockAndImagesWebRequest.getImages())
          .orElse(new ArrayList<>()).stream()
          .map(RequestHelper::toProductLevel3SummaryDetailsImageRequest).collect(toList()));
      productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Optional.ofNullable(productVariantPriceStockAndImagesWebRequest.getModifiedItemPickupPoints())
          .orElse(new ArrayList<>()).stream().map(RequestHelper::toItemPickupPointRequest)
          .collect(toList()));
      productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints().forEach(
        itemPickupPointWebRequest -> itemPickupPointWebRequest
          .setSellerSku(productVariantPriceStockAndImagesWebRequest.getMerchantSku()));
      requests.add(productVariantPriceStockAndImagesRequest);
    }
    return requests;
  }

  public static ProductItemLevel3Request toProductItemLevel3RequesFromProductItemLevel3Request(
      ProductItemLevel3WebRequest productItemLevel3WebRequest) {
    ProductItemLevel3Request productItemLevel3Request = new ProductItemLevel3Request();
    BeanUtils.copyProperties(productItemLevel3WebRequest, productItemLevel3Request);
    if (CollectionUtils.isNotEmpty(productItemLevel3WebRequest.getPrices())) {
      productItemLevel3Request.setPrices(productItemLevel3WebRequest.
          getPrices().stream().map(
          productLevel3PriceWebRequest -> toProductLevel3PriceRequestFromProductLevel3PriceWebRequest(
              productLevel3PriceWebRequest)).collect(toList()));
    }
    productItemLevel3Request.setViewConfigs(productItemLevel3WebRequest.
        getViewConfigs().stream().map(
        viewConfigWebRequest -> toProductLevel3ViewConfigRequestFromProductLevel3ViewConfigWebRequest(
            viewConfigWebRequest)).collect(toList()));
    productItemLevel3Request.setImages(productItemLevel3WebRequest.
        getImages().stream()
        .map(imageWebRequest -> toProductLevel3ImageRequestFromProductLevel3ImageWebRequest(imageWebRequest))
        .collect(toList()));
    return productItemLevel3Request;
  }

  private static ProductLevel3AttributeRequest toProductLevel3AttributeRequestFromProductLevel3AttributeWebRequest(
      ProductLevel3AttributeWebRequest productLevel3AttributeWebRequest) {
    ProductLevel3AttributeRequest productLevel3AttributeRequest = new ProductLevel3AttributeRequest();
    BeanUtils.copyProperties(productLevel3AttributeWebRequest, productLevel3AttributeRequest);
    return productLevel3AttributeRequest;
  }

  private static ProductLevel3ImageRequest toProductLevel3ImageRequestFromProductLevel3ImageWebRequest(
      ProductLevel3ImageWebRequest productLevel3ImageWebRequest) {
    ProductLevel3ImageRequest productLevel3ImageRequest = new ProductLevel3ImageRequest();
    BeanUtils.copyProperties(productLevel3ImageWebRequest, productLevel3ImageRequest);
    return productLevel3ImageRequest;
  }

  private static ProductLevel3SummaryDetailsImageRequest toProductLevel3CommonImageRequestFromProductL3CommonImageWebRequest(
    ProductLevel3CommonImageWebRequest productLevel3CommonImageWebRequest) {
    ProductLevel3SummaryDetailsImageRequest commonImageRequest = new ProductLevel3SummaryDetailsImageRequest();
    BeanUtils.copyProperties(productLevel3CommonImageWebRequest, commonImageRequest,
      "markForDelete","mainImages");
    commonImageRequest.setMarkForDelete(productLevel3CommonImageWebRequest.getMarkForDelete());
    commonImageRequest.setMainImage(productLevel3CommonImageWebRequest.getMainImages());
    return commonImageRequest;
  }

  private static ProductLevel3ViewConfigRequest toProductLevel3ViewConfigRequestFromProductLevel3ViewConfigWebRequest(
      ProductLevel3ViewConfigWebRequest productLevel3ViewConfigWebRequest) {
    ProductLevel3ViewConfigRequest productLevel3ViewConfigRequest = new ProductLevel3ViewConfigRequest();
    BeanUtils.copyProperties(productLevel3ViewConfigWebRequest, productLevel3ViewConfigRequest);
    return productLevel3ViewConfigRequest;
  }

  private static ProductLevel3PriceRequest toProductLevel3PriceRequestFromProductLevel3PriceWebRequest(
      ProductLevel3PriceWebRequest productLevel3PriceWebRequest) {
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    BeanUtils.copyProperties(productLevel3PriceWebRequest, productLevel3PriceRequest);
    return productLevel3PriceRequest;
  }

  public static void validateRequest(ProductLevel3Request request) {
    validateBaseRequest(request);
    request.setDescription(validateDescription(request.getDescription()));
    request.setUniqueSellingPoint(validateData(request.getUniqueSellingPoint()));
    request.setSpecificationDetail(validateData(request.getSpecificationDetail()));
    request.setProductName(validateData(request.getProductName()));
    request.setProductSku(validateData(request.getProductSku()));
    request.setProductStory(validateData(request.getProductStory()));
    request.setProductCode(validateData(request.getProductCode()));
    request.setUrl(validateData(request.getUrl()));
    request.setAccessChannel(validateData(request.getAccessChannel()));
    request.setBusinessPartnerCode(validateData(request.getBusinessPartnerCode()));
    request.setCategoryCode(validateData(request.getCategoryCode()));
    request.setCategoryHierarchy(validateData(request.getCategoryHierarchy()));
    request.setCategoryName(validateData(request.getCategoryName()));

    if (Objects.nonNull(request.getItems())) {
      for (ProductItemLevel3Request productItemLevel3Request : request.getItems()) {
        validateBaseRequest(productItemLevel3Request);
        productItemLevel3Request.setItemName(validateData(productItemLevel3Request.getItemName()));
        productItemLevel3Request.setItemSku(validateData(productItemLevel3Request.getItemSku()));
        productItemLevel3Request.setUpcCode(validateData(productItemLevel3Request.getUpcCode()));
        productItemLevel3Request.setMerchantSku(validateData(productItemLevel3Request.getMerchantSku()));
        productItemLevel3Request.setPickupPointCode(validateData(productItemLevel3Request.getPickupPointCode()));
        productItemLevel3Request.setPickupPointName(validateData(productItemLevel3Request.getPickupPointName()));
      }
    }

    if (Objects.nonNull(request.getAttributes())) {
      for (ProductLevel3AttributeRequest productLevel3AttributeRequest : request.getAttributes()) {
        validateBaseRequest(productLevel3AttributeRequest);
        productLevel3AttributeRequest.setAttributeName(validateData(productLevel3AttributeRequest.getAttributeName()));
        productLevel3AttributeRequest.setAttributeCode(validateData(productLevel3AttributeRequest.getAttributeCode()));
        productLevel3AttributeRequest.setAttributeType(validateData(productLevel3AttributeRequest.getAttributeType()));
        productLevel3AttributeRequest.setId(validateData(productLevel3AttributeRequest.getId()));
        productLevel3AttributeRequest.setItemSku(validateData(productLevel3AttributeRequest.getItemSku()));
      }
    }

    if (Objects.nonNull(request.getImages())) {
      for (ProductLevel3ImageRequest productLevel3ImageRequest : request.getImages()) {
        validateBaseRequest(productLevel3ImageRequest);
        productLevel3ImageRequest.setLocationPath(validateData(productLevel3ImageRequest.getLocationPath()));
      }
    }
  }

  public static void validateRequest(ProductLevel3UpdateSummaryRequest request) {
    request.setAccessChannel(validateData(request.getAccessChannel()));
    request.setMerchantSku(validateData(request.getMerchantSku()));
    request.setPickupPointCode(validateData(request.getPickupPointCode()));
    request.setProductName(validateData(request.getProductName()));

    if (Objects.nonNull(request.getPrices())) {
      for (ProductLevel3PriceRequest productLevel3PriceRequest : request.getPrices()) {
        validateBaseRequest(productLevel3PriceRequest);
        productLevel3PriceRequest.setChannelId(validateData(productLevel3PriceRequest.getChannelId()));
        productLevel3PriceRequest.setPromotionName(validateData(productLevel3PriceRequest.getPromotionName()));
        productLevel3PriceRequest.setId(validateData(productLevel3PriceRequest.getId()));
      }
    }

    if (Objects.nonNull(request.getViewConfigs())) {
      for (ProductLevel3ViewConfigRequest productLevel3ViewConfigRequest : request.getViewConfigs()) {
        validateBaseRequest(productLevel3ViewConfigRequest);
        productLevel3ViewConfigRequest.setChannelId(validateData(productLevel3ViewConfigRequest.getChannelId()));
      }
    }
  }

  public static void validateGenerateShippingWeightRequest(double length, double width, double height, double weight) {
    if (Double.isNaN(length) || Double.isNaN(width) || Double.isNaN(height) || Double.isNaN(weight)) {
      throw new ValidationException(ErrorMessages.NAN_NOT_ALLOWED);
    }
  }

  public static void validateRequest(ProductCreationRequest request, int preOrderMaximumDays, int preOrderMaximumWeek,
      boolean isMPPFlow, boolean warnaFamilyColorValidationSwitch) throws Exception {
    if (Objects.nonNull(request.getDescription())) {
      String description = validateDescription(new String(request.getDescription(), "UTF-8"));
      request.setDescription(description.getBytes());
    }
    if (Objects.nonNull(request.getLongDescription())) {
      String description = validateDescription(new String(request.getLongDescription(), "UTF-8"));
      request.setLongDescription(description.getBytes());
    }
    request.setUniqueSellingPoint(validateData(request.getUniqueSellingPoint()));
    request.setSpecificationDetail(validateData(request.getSpecificationDetail()));
    request.setForceReviewNotes(validateData(request.getForceReviewNotes()));
    request.setNotes(validateData(request.getNotes()));
    request.setUom(validateData(request.getUom()));
    request.setName(validateData(request.getName()));
    request.setProductStory(validateData(request.getProductStory()));
    request.setProductCode(validateData(request.getProductCode()));
    request.setUrl(validateData(request.getUrl()));
    request.setBrandApprovalStatus(validateData(request.getBrandApprovalStatus()));
    request.setBrandCode(validateData(request.getBrandCode()));
    request.setBusinessPartnerCode(validateData(request.getBusinessPartnerCode()));
    request.setBusinessPartnerId(validateData(request.getBusinessPartnerId()));
    request.setBusinessPartnerName(validateData(request.getBusinessPartnerName()));
    request.setGdnProductSku(validateData(request.getGdnProductSku()));
    request.setOldProductCode(validateData(request.getOldProductCode()));
    request.setOldProductRejectionNote(validateData(request.getOldProductRejectionNote()));
    request.setCategoryName(validateData(request.getCategoryName()));
    request.setCreatedBy(validateData(request.getCreatedBy()));
    request.setUpdatedBy(validateData(request.getUpdatedBy()));
    request.setStoreId(validateData(request.getStoreId()));

    if (Objects.nonNull(request.getProductItemRequests())) {
      for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
        productItemCreationRequest.setUpcCode(validateData(productItemCreationRequest.getUpcCode()));
        productItemCreationRequest.setMerchantSku(validateData(productItemCreationRequest.getMerchantSku()));
        productItemCreationRequest
            .setGdnProductItemSku(validateData(productItemCreationRequest.getGdnProductItemSku()));
        productItemCreationRequest.setPickupPointId(validateData(productItemCreationRequest.getPickupPointId()));
        productItemCreationRequest.setProductItemId(validateData(productItemCreationRequest.getProductItemId()));
        productItemCreationRequest
            .setProductItemHashCode(validateData(productItemCreationRequest.getProductItemHashCode()));
        if (isMPPFlow) {
          productItemCreationRequest.getPickupPoints().forEach(pickupPointCreateRequest -> pickupPointCreateRequest
              .setPickupPointId(validateData(pickupPointCreateRequest.getPickupPointId())));
        }
        if (Objects.nonNull(productItemCreationRequest.getImages())) {
          for (Image image : productItemCreationRequest.getImages()) {
            image.setLocationPath(validateData(image.getLocationPath()));
            image.setId(validateData(image.getId()));
            image.setHashCode(validateData(image.getHashCode()));
            image.setCreatedBy(validateData(image.getCreatedBy()));
            image.setUpdatedBy(validateData(image.getUpdatedBy()));
            image.setStoreId(validateData(image.getStoreId()));
          }
        }
      }
    }

    if (warnaFamilyColorValidationSwitch && !checkWarnaAndFamilyColorBothShouldBePresent(request)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.WARNA_AND_FAMILY_COLOR_ERROR.getDesc());
    }

    if (Objects.nonNull(request.getProductBusinessPartnerAttributes())) {
      for (ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest : request
          .getProductBusinessPartnerAttributes()) {
        validateBaseRequest(productBusinessPartnerAttributeRequest);
        productBusinessPartnerAttributeRequest.setId(validateData(productBusinessPartnerAttributeRequest.getId()));
        productBusinessPartnerAttributeRequest
            .setAttributeId(validateData(productBusinessPartnerAttributeRequest.getAttributeId()));
        productBusinessPartnerAttributeRequest
            .setValue(validateData(productBusinessPartnerAttributeRequest.getValue()));
      }
    }

    if (Objects.nonNull(request.getProductAttributes())) {
      for (ProductAttributeRequest productAttributeRequest : request.getProductAttributes()) {
        validateBaseDTORequest(productAttributeRequest);
        productAttributeRequest
            .setProductAttributeName(validateData(productAttributeRequest.getProductAttributeName()));
      }
    }

    if (Objects.nonNull(request.getProductCategories())) {
      for (ProductCategoryRequest productCategoryRequest : request.getProductCategories()) {
        validateBaseDTORequest(productCategoryRequest);
        CategoryRequest categoryRequest = productCategoryRequest.getCategory();
        if (Objects.nonNull(categoryRequest)) {
          categoryRequest.setCategoryCode(validateData(categoryRequest.getCategoryCode()));
          categoryRequest.setName(validateData(categoryRequest.getName()));
          categoryRequest.setId(validateData(categoryRequest.getId()));
          CategoryRequest productCategoryRequestCategory = categoryRequest.getParentCategory();
          if (Objects.nonNull(productCategoryRequestCategory)) {
            productCategoryRequestCategory.setId(validateData(categoryRequest.getId()));
          }
          CatalogRequest catalogRequest = categoryRequest.getCatalog();
          if (Objects.nonNull(catalogRequest)) {
            catalogRequest.setCatalogCode(validateData(catalogRequest.getCatalogCode()));
            catalogRequest.setCatalogType(validateData(catalogRequest.getCatalogType()));
            catalogRequest.setName(validateData(catalogRequest.getName()));
            catalogRequest.setId(validateData(catalogRequest.getId()));
          }
        }
      }
    }
    if (Objects.nonNull(request.getPreOrder()) && Boolean.TRUE.equals(request.getPreOrder().getIsPreOrder())) {
      validatePreOrderRequest(request.getPreOrder(), preOrderMaximumDays, preOrderMaximumWeek);
    }
  }

  private static boolean checkWarnaAndFamilyColorBothShouldBePresent(ProductCreationRequest productRequest) {
    boolean isFamilyColorPresent = productRequest.getProductItemRequests().stream().flatMap(
        productItemRequest -> Optional.ofNullable(productItemRequest.getProductItemAttributeValueRequests())
            .orElse(new ArrayList<>()).stream()).anyMatch(
        productItemAttributeValueRequest -> Constants.FAMILY_COLOUR.equals(
            productItemAttributeValueRequest.getAttribute().getName()));
    boolean isWarnaPresent =
        Optional.ofNullable(productRequest.getProductAttributes()).orElse(new ArrayList<>()).stream()
            .anyMatch(RequestHelper::isWarnaAndDefiningAttributeOrVariantCreationTrue);
    if (isFamilyColorPresent) {
      return isWarnaPresent;
    }
    if (isWarnaPresent) {
      return isFamilyColorPresent;
    }
    return true;
  }

  private static boolean isWarnaAndDefiningAttributeOrVariantCreationTrue(
      ProductAttributeRequest productAttributeRequest) {
    return Constants.WARNA.equals(productAttributeRequest.getAttribute().getName()) && (
        AttributeType.DEFINING_ATTRIBUTE.equals(productAttributeRequest.getAttribute().getAttributeType())
            || productAttributeRequest.getAttribute().isVariantCreation());
  }

  public static void validateRequest(ProductBusinessPartnerServiceRequest request) {
    request.setBusinessPartnerCode(validateData(request.getBusinessPartnerCode()));
    request.setGdnProductSku(validateData(request.getGdnProductSku()));
    request.setProductId(validateData(request.getProductId()));
    request.setProductName(validateData(request.getProductName()));
    request.setCategoryName(validateData(request.getCategoryName()));
    request.setUsername(validateData(request.getUsername()));
    request.setRequestId(validateData(request.getRequestId()));
    request.setStoreId(validateData(request.getStoreId()));

    if (Objects.nonNull(request.getProductItemBusinessPartners())) {
      for (ProductItemBusinessPartnerServiceRequest productItemBusinessPartnerServiceRequest : request
          .getProductItemBusinessPartners()) {
        productItemBusinessPartnerServiceRequest
            .setMerchantSku(validateData(productItemBusinessPartnerServiceRequest.getMerchantSku()));
        productItemBusinessPartnerServiceRequest
            .setGdnProductItemSku(validateData(productItemBusinessPartnerServiceRequest.getGdnProductItemSku()));
        productItemBusinessPartnerServiceRequest
            .setPickupPointId(validateData(productItemBusinessPartnerServiceRequest.getPickupPointId()));
        productItemBusinessPartnerServiceRequest
            .setProductItemId(validateData(productItemBusinessPartnerServiceRequest.getProductItemId()));
        productItemBusinessPartnerServiceRequest
            .setProductHashCode(validateData(productItemBusinessPartnerServiceRequest.getProductHashCode()));

      }
    }

    if (Objects.nonNull(request.getProductBusinessPartnerAttributes())) {
      for (ProductBusinessPartnerAttributeServiceRequest productBusinessPartnerAttributeServiceRequest : request
          .getProductBusinessPartnerAttributes()) {
        productBusinessPartnerAttributeServiceRequest
            .setAttributeId(validateData(productBusinessPartnerAttributeServiceRequest.getAttributeId()));
        productBusinessPartnerAttributeServiceRequest
            .setValue(validateData(productBusinessPartnerAttributeServiceRequest.getValue()));
      }
    }
  }

  public static void validateRequest(CreateBrandWipRequest request) {
    request.setBrandDescription(validateData(request.getBrandDescription()));
    request.setBrandLogoPath(validateData(request.getBrandLogoPath()));
    request.setBrandName(validateData(request.getBrandName()));
    request.setBusinessPartnerCode(validateData(request.getBusinessPartnerCode()));
    request.setBusinessPartnerName(validateData(request.getBusinessPartnerName()));
    request.setProfileBannerPath(validateData(request.getProfileBannerPath()));
  }

  private static void validateBaseRequest(BaseRequest request) {
    request.setCreatedBy(validateData(request.getCreatedBy()));
    request.setUpdatedBy(validateData(request.getUpdatedBy()));
    request.setStoreId(validateData(request.getStoreId()));
  }

  private static void validateBaseDTORequest(BaseDTORequest request) {
    request.setCreatedBy(validateData(request.getCreatedBy()));
    request.setUpdatedBy(validateData(request.getUpdatedBy()));
    request.setStoreId(validateData(request.getStoreId()));
    request.setId(validateData(request.getId()));
  }

  private static String validateData(String request) {
    if (StringUtils.isBlank(request)) {
      return request;
    }
    if (request.contains("&lt;") || request.contains("&gt;") || request.contains("&amp;")) {
      request = Jsoup.clean(StringEscapeUtils.unescapeHtml3(request), customWhitelist());
      request = StringEscapeUtils.unescapeHtml3(request);
    } else {
      request = StringEscapeUtils.unescapeHtml3(Jsoup.clean(request, customWhitelist()));
    }
    if(request.contains(IFRAME)) {
      return validateVideoURLs(request);
    }
    return request;
  }

  private static Whitelist customWhitelist() {
    return Whitelist.relaxed().addTags(IFRAME)
        .addAttributes(IFRAME, "align", "alt", "height", "src", "title", "width", "allowfullscreen");
  }

  private static String validateDescription(String request) {
    if (StringUtils.isBlank(request)) {
      return request;
    }
    if (request.contains("&lt;") || request.contains("&gt;") || request.contains("&amp;")) {
      request = Jsoup.clean(StringEscapeUtils.unescapeHtml3(request), Whitelist.relaxed().addTags(IFRAME)
          .addAttributes(IFRAME, "align", "alt", "height", "src", "title", "width", "allowfullscreen")
          .addAttributes("p", "style").addAttributes("div", "style").addAttributes("span", "style"));
      request = StringEscapeUtils.unescapeHtml3(request);
    } else {
      request = StringEscapeUtils.unescapeHtml3(Jsoup.clean(request, Whitelist.relaxed().addTags(IFRAME)
          .addAttributes(IFRAME, "align", "alt", "height", "src", "title", "width", "allowfullscreen")
          .addAttributes("p", "style").addAttributes("div", "style").addAttributes("span", "style")));
    }
    if(request.contains(IFRAME)) {
      return validateVideoURLs(request);
    }
    return request;
  }

  public static String validateVideoURLs(String request) {
    Matcher matcherForIframe = Pattern.compile(IFRAME_REGEX).matcher(request);
    List<String> iFrames = new ArrayList<>();
    List<String> sources = new ArrayList<>();
    while (matcherForIframe.find()) {
      iFrames.add(matcherForIframe.group());
    }
    for (String string : iFrames) {
      Matcher matcherForSrc = Pattern.compile(SRC_REGEX).matcher(string);
      while (matcherForSrc.find()) {
        sources.add(matcherForSrc.group());
      }
    }
    for (String source : sources) {
      if (!isYoutubeURL(source)) {
        for (String iFrame : iFrames) {
          if (iFrame.contains(source)) {
            request = request.replace(iFrame, StringUtils.EMPTY);
          }
        }
      }
    }
    return request;
  }

  private static boolean isYoutubeURL(String url) {
    return url.contains(YOUTUBE_URL_LINK) || url.contains(YOUTUBE_MOBILE_LINK);
  }

  public static ProductPriceAndWholesaleRequest toProductPriceAndWholesaleRequest(
      ProductPriceAndStockUpdateWebRequest productPriceAndStockUpdateWebRequest) {
    ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest = new ProductPriceAndWholesaleRequest();
    productPriceAndWholesaleRequest.setProductItemWholesalePriceRequests(
        Optional.ofNullable(productPriceAndStockUpdateWebRequest.getProductItemWholesalePriceRequests())
            .orElse(new ArrayList<>()).stream().map(RequestHelper::toProductItemWholesalePriceRequest)
            .collect(toList()));
    productPriceAndWholesaleRequest.setChannel(productPriceAndStockUpdateWebRequest.getPrices().get(0).getChannelId());
    productPriceAndWholesaleRequest.setListPrice(productPriceAndStockUpdateWebRequest.getPrices().get(0).getPrice());
    productPriceAndWholesaleRequest.setOfferPrice(productPriceAndStockUpdateWebRequest.getPrices().get(0).getSalePrice());
    productPriceAndWholesaleRequest
        .setWholesalePriceActivated(productPriceAndStockUpdateWebRequest.getWholesalePriceActivated());
    return productPriceAndWholesaleRequest;
  }

  private static ProductItemWholesalePriceRequest toProductItemWholesalePriceRequest(
      ProductItemWholesalePriceWebRequest productItemWholesalePriceWebRequest) {
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    BeanUtils.copyProperties(productItemWholesalePriceWebRequest, productItemWholesalePriceRequest);
    return productItemWholesalePriceRequest;
  }

  public static UpdateItemsPriceStockImagesRequest toUpdateItemsPriceStockImagesRequest(
      UpdateItemsPriceStockImagesWebRequest updateItemsPriceStockImagesWebRequest) {
    UpdateItemsPriceStockImagesRequest updateItemsPriceStockImagesRequest = new UpdateItemsPriceStockImagesRequest();
    BeanUtils.copyProperties(updateItemsPriceStockImagesWebRequest, updateItemsPriceStockImagesRequest,
        "copyToAllVariantImages", "productItems");
    updateItemsPriceStockImagesRequest.setCopyToAllVariantImages(
        Optional.ofNullable(updateItemsPriceStockImagesWebRequest.getCopyToAllVariantImages()).orElse(new ArrayList<>())
            .stream().map(RequestHelper::toProductLevel3SummaryDetailsImageRequest).collect(toList()));
    updateItemsPriceStockImagesRequest.setProductItems(updateItemsPriceStockImagesWebRequest.getProductItems().stream()
        .map(productItem -> toProductPriceStockAndImagesWebRequest(productItem)).collect(toList()));
    return updateItemsPriceStockImagesRequest;
  }

  private static ProductPriceStockAndImagesRequest toProductPriceStockAndImagesWebRequest(
      ProductPriceStockAndImagesWebRequest productItem) {
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    BeanUtils.copyProperties(productItem, productPriceStockAndImagesRequest);
    if (CollectionUtils.isNotEmpty(productItem.getPrices())) {
      productPriceStockAndImagesRequest.setPrices(productItem.
          getPrices().stream().map(
          productLevel3PriceWebRequest -> toProductLevel3PriceRequestFromProductLevel3PriceWebRequest(
              productLevel3PriceWebRequest)).collect(toList()));
    }
    productPriceStockAndImagesRequest.setViewConfigs(productItem.
        getViewConfigs().stream().map(
        viewConfigWebRequest -> toProductLevel3ViewConfigRequestFromProductLevel3ViewConfigWebRequest(
            viewConfigWebRequest)).collect(toList()));
    productPriceStockAndImagesRequest.setImages(productItem.
        getImages().stream().map(imageWebRequest -> toProductLevel3SummaryDetailsImageRequest(imageWebRequest))
        .collect(toList()));
    return productPriceStockAndImagesRequest;
  }

  private static ProductLevel3SummaryDetailsImageRequest toProductLevel3SummaryDetailsImageRequest(
      ProductLevel3SummaryDetailsImageWebRequest productLevel3SummaryDetailsImageWebRequest) {
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    BeanUtils.copyProperties(productLevel3SummaryDetailsImageWebRequest, productLevel3SummaryDetailsImageRequest);
    return productLevel3SummaryDetailsImageRequest;
  }

  private static ProductLevel3SummaryDetailsImageRequest toProductLevel3SummaryDetailsImageRequest(
    ProductLevel3CommonImageWebRequest productLevel3SummaryDetailsImageWebRequest) {
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
      new ProductLevel3SummaryDetailsImageRequest();
    BeanUtils.copyProperties(productLevel3SummaryDetailsImageWebRequest,
      productLevel3SummaryDetailsImageRequest, "commonImages");
    productLevel3SummaryDetailsImageRequest.setMainImage(productLevel3SummaryDetailsImageWebRequest.getMainImages());
    return productLevel3SummaryDetailsImageRequest;
  }

  public static HistoryRequest toHistoryRequest(HistorySummaryWebRequest historySummaryWebRequest) {
    return new HistoryRequest(historySummaryWebRequest.getProductSku(), historySummaryWebRequest.getSearchField(),
        historySummaryWebRequest.getKeyword(), historySummaryWebRequest.getStartDate(),
        historySummaryWebRequest.getEndDate(), historySummaryWebRequest.isBeforeThreeMonths());
  }

  public static ProductLevel3UpdateRequest toProductLevel3UpdateRequest(
      ProductLevel3UpdateWebRequest productLevel3UpdateWebRequest) {
    ProductLevel3UpdateRequest productLevel3UpdateRequest = new ProductLevel3UpdateRequest();
    BeanUtils.copyProperties(productLevel3UpdateWebRequest, productLevel3UpdateRequest);
    if (CollectionUtils.isNotEmpty(productLevel3UpdateWebRequest.getLogistics())) {
      productLevel3UpdateRequest.setProductLevel3LogisticsRequest(productLevel3UpdateWebRequest.getLogistics().stream()
          .map(logisticsWebRequest -> toProductLevel3Logistics(logisticsWebRequest)).collect(toList()));
    }
    if (Objects.nonNull(productLevel3UpdateWebRequest.getPreOrder())) {
      PreOrderRequest preOrderRequest = new PreOrderRequest();
      BeanUtils.copyProperties(productLevel3UpdateWebRequest.getPreOrder(), preOrderRequest);
      productLevel3UpdateRequest.setPreOrder(preOrderRequest);
    }
    return productLevel3UpdateRequest;
  }

  public static ProductLevel3Logistics toProductLevel3Logistics(
      ProductLevel3LogisticsWebRequest productLevel3LogisticsWebRequest) {
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    BeanUtils.copyProperties(productLevel3LogisticsWebRequest, productLevel3Logistics);
    return productLevel3Logistics;
  }

  public static PickupPointUpdateRequest toPickupPointUpdateRequest(PickupPointUpdateWebRequest pickupPointUpdateWebRequest,
      String businessPartnerCode) {
    PickupPointUpdateRequest pickupPointUpdateRequest = new PickupPointUpdateRequest();
    BeanUtils.copyProperties(pickupPointUpdateWebRequest, pickupPointUpdateRequest);
    pickupPointUpdateRequest.setBusinessPartnerCode(businessPartnerCode);
    if (CollectionUtils.isNotEmpty(pickupPointUpdateWebRequest.getItemsPickupPoint())) {
      pickupPointUpdateRequest.setItemsPickupPoint(pickupPointUpdateWebRequest.getItemsPickupPoint().stream()
          .map(RequestHelper::toPickupPointRequest)
          .collect(toList()));
    }
    return pickupPointUpdateRequest;
  }

  public static PickupPointRequest toPickupPointRequest(
      PickupPointUpdateItemsWebRequest pickupPointUpdateItemsWebRequest) {
    PickupPointRequest pickupPointRequest = new PickupPointRequest();
    BeanUtils.copyProperties(pickupPointUpdateItemsWebRequest, pickupPointRequest);
    return pickupPointRequest;
  }

  public static ProductLevel3SummaryDetailsRequest toProductLevel3SummaryDetailsRequest(
      ProductLevel3VariantsWebRequest request) {
    ProductLevel3SummaryDetailsRequest productLevel3SummaryDetailsRequest = new ProductLevel3SummaryDetailsRequest();
    BeanUtils.copyProperties(request, productLevel3SummaryDetailsRequest);
    return productLevel3SummaryDetailsRequest;
  }

  public static List<QuickEditRequest> toQuickEditRequestList(List<QuickEditWebRequest> quickEditWebRequests) {
    return quickEditWebRequests.stream().map(RequestHelper::toQuickEditRequest).collect(Collectors.toList());
  }

  private static QuickEditRequest toQuickEditRequest(QuickEditWebRequest quickEditWebRequest) {
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    BeanUtils.copyProperties(quickEditWebRequest, quickEditRequest, "price", "status");
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    BeanUtils.copyProperties(quickEditWebRequest.getPrices().get(0), productLevel3PriceRequest);
    quickEditRequest.setPrice(productLevel3PriceRequest);
    quickEditRequest.setStatus(getProductLevel3Status(quickEditWebRequest.getStatus()));
    quickEditRequest.setUseWarehouseStock(quickEditWebRequest.getSynchronizeStock());
    quickEditRequest.setWholeSaleActivated(quickEditWebRequest.getWholesalePriceActivated());
    quickEditRequest.setPickupPointCode(quickEditWebRequest.getPickupPointCode());
    return quickEditRequest;
  }

  private static ProductLevel3Status getProductLevel3Status(String status) {
    if (ProductLevel3Status.ONLINE.name().equals(status)) {
      return ProductLevel3Status.ONLINE;
    } else if (ProductLevel3Status.OFFLINE.name().equals(status)) {
      return ProductLevel3Status.OFFLINE;
    } else if (ProductLevel3Status.TEASER.name().equals(status)) {
      return ProductLevel3Status.TEASER;
    } else {
      return ProductLevel3Status.B2B;
    }
  }

  public static ProductSummaryRequest toProductSummaryRequest(ProductSummaryWebRequest productSummaryWebRequest) {
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    BeanUtils.copyProperties(productSummaryWebRequest, productSummaryRequest);
    return productSummaryRequest;
  }

  private static void validatePreOrderRequest(PreOrderRequest preOrderRequest, int preOrderMaximumDays,
      int preOrderMaximumWeek) throws Exception {
    if (PreOrderType.DAYS.name().equals(preOrderRequest.getPreOrderType())) {
      if (preOrderRequest.getPreOrderValue() == 0) {
        throw new ApplicationException(HttpStatus.BAD_REQUEST, ApiErrorCode.PREORDER_MINIMUM_DAYS_ERROR.getCode(),
            ApiErrorCode.PREORDER_MINIMUM_DAYS_ERROR.getDesc());

      } else if (preOrderRequest.getPreOrderValue() > preOrderMaximumDays) {
        throw new ApplicationException(HttpStatus.BAD_REQUEST, ApiErrorCode.PREORDER_MAXIMUM_DAYS_ERROR.getCode(),
            String.format(ApiErrorCode.PREORDER_MAXIMUM_DAYS_ERROR.getDesc(), preOrderMaximumDays));
      }
    } else if (PreOrderType.WEEK.name().equals(preOrderRequest.getPreOrderType())) {
      if (preOrderRequest.getPreOrderValue() == 0) {
        throw new ApplicationException(HttpStatus.BAD_REQUEST, ApiErrorCode.PREORDER_MINIMUM_WEEK_ERROR.getCode(),
            ApiErrorCode.PREORDER_MINIMUM_WEEK_ERROR.getDesc());
      } else if (preOrderRequest.getPreOrderValue() > preOrderMaximumWeek) {
        throw new ApplicationException(HttpStatus.BAD_REQUEST, ApiErrorCode.PREORDER_MAXIMUM_WEEK_ERROR.getCode(),
            String.format(ApiErrorCode.PREORDER_MAXIMUM_WEEK_ERROR.getDesc(), preOrderMaximumWeek));
      }
    } else if (PreOrderType.DATE.name().equals(preOrderRequest.getPreOrderType())) {
      Date currentDate = Constants.PREORDER_DATE_FORMAT.parse(Constants.PREORDER_DATE_FORMAT.format(new Date()));
      Date preOrderDate = Constants.PREORDER_DATE_FORMAT
          .parse(Constants.PREORDER_DATE_FORMAT.format(preOrderRequest.getPreOrderDate()));
      if (currentDate.equals(preOrderDate) || currentDate.after(preOrderDate)) {
        throw new ApplicationException(HttpStatus.BAD_REQUEST, ApiErrorCode.PREORDER_DATE_ERROR.getCode(),
            ApiErrorCode.PREORDER_DATE_ERROR.getDesc());
      }
      preOrderRequest.setPreOrderDate(preOrderDate);
      long totalDays = Math.abs(preOrderDate.getTime() - currentDate.getTime()) / Constants.TOTAL_HOURS;
      if (totalDays > preOrderMaximumDays) {
        throw new ApplicationException(HttpStatus.BAD_REQUEST, ApiErrorCode.PREORDER_DATE_EXCEEDED_ERROR.getCode(),
            String.format(ApiErrorCode.PREORDER_DATE_EXCEEDED_ERROR.getDesc(), preOrderMaximumDays));
      }
    } else {
      throw new ApplicationException(HttpStatus.BAD_REQUEST, ApiErrorCode.PREORDER_INVALID_TYPE.getCode(),
          ApiErrorCode.PREORDER_INVALID_TYPE.getDesc());
    }
  }

  public static NeedRevisionSubmitRequest toNeedRevisionSubmitRequest(
      NeedRevisionSubmitWebRequest needRevisionSubmitWebRequest) {
    return NeedRevisionSubmitRequest.builder().productCode(needRevisionSubmitWebRequest.getProductCode())
        .productSku(needRevisionSubmitWebRequest.getProductSku())
        .appealedProduct(needRevisionSubmitWebRequest.isAppealedProduct())
        .appealedProductNotes(needRevisionSubmitWebRequest.getAppealedProductNotes()).build();
  }

  public static AppealProductRequest toAppealProductRequest(
      AppealProductWebRequest appealProductWebRequest, String businessPartnerCode) {
    return AppealProductRequest.builder().notes(appealProductWebRequest.getNotes())
        .productCode(appealProductWebRequest.getProductCode())
        .productSku(appealProductWebRequest.getProductSku())
        .businessPartnerCode(businessPartnerCode)
        .build();
  }

  public static ProductImageEditRequest toProductImageEditRequest(ProductImageEditWebRequest productImageEditWebRequest,
      String productSku) {
    ProductImageEditRequest productImageEditRequest = new ProductImageEditRequest();
    productImageEditRequest.setProductSku(productSku);
    productImageEditRequest.setImagePath(productImageEditWebRequest.getImagePath());
    if (CollectionUtils.isNotEmpty(productImageEditWebRequest.getProductItems())) {
      List<ItemImageEditRequest> imageEditRequests = new ArrayList<>();
      for (ItemImageEditWebRequest itemImageEditWebRequest : productImageEditWebRequest.getProductItems()) {
        ItemImageEditRequest itemImageEditRequest = new ItemImageEditRequest();
        itemImageEditRequest.setCopy(itemImageEditWebRequest.isCopy());
        if (StringUtils.isNotBlank(productImageEditWebRequest.getImageData())) {
          itemImageEditRequest.setCopy(false);
          itemImageEditRequest.setAdd(true);
        }
        itemImageEditRequest.setItemSku(itemImageEditWebRequest.getItemSku());
        itemImageEditRequest.setMainImage(itemImageEditWebRequest.isMainImage());
        itemImageEditRequest.setMarkForDelete(itemImageEditWebRequest.isMarkForDelete());
        imageEditRequests.add(itemImageEditRequest);
      }
      productImageEditRequest.setProductItems(imageEditRequests);
    }
    if (Objects.nonNull(productImageEditWebRequest.getCopyToAllVariantImages())) {
      CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
      copyImageEditRequest.setCopy(productImageEditWebRequest.getCopyToAllVariantImages().isCopy());
      if (StringUtils.isNotBlank(productImageEditWebRequest.getImageData())) {
        copyImageEditRequest.setCopy(false);
        copyImageEditRequest.setAdd(true);
      }
      copyImageEditRequest.setMainImage(productImageEditWebRequest.getCopyToAllVariantImages().isMainImage());
      copyImageEditRequest.setMarkForDelete(productImageEditWebRequest.getCopyToAllVariantImages().isMarkForDelete());
      productImageEditRequest.setCopyToAllVariantImages(copyImageEditRequest);
    }
    return productImageEditRequest;
  }

  public static List<QuickEditV2Request> toQuickEditV2Requests(
    List<QuickEditV2WebRequest> quickEditV2WebRequests) {
    List<QuickEditV2Request> quickEditV2Requests = new ArrayList<>();
    for (QuickEditV2WebRequest quickEditV2WebRequest : quickEditV2WebRequests) {
      QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
      BeanUtils.copyProperties(quickEditV2WebRequest, quickEditV2Request);
      quickEditV2Request.setStatus(ProductLevel3Status.valueOf(quickEditV2WebRequest.getStatus()));
      if (StringUtils.isNotEmpty(quickEditV2WebRequest.getCncStatus())) {
        quickEditV2Request.setCncStatus(
          ProductLevel3Status.valueOf(quickEditV2WebRequest.getCncStatus()));
      }
      quickEditV2Request.setUseWarehouseStock(quickEditV2WebRequest.getSynchronizeStock());
      quickEditV2Request.setPrice(toProductLevel3PriceRequestFromProductLevel3PriceWebRequest(
        quickEditV2WebRequest.getPrices().stream().findFirst().get()));
      quickEditV2Request.setWholeSaleActivated(quickEditV2WebRequest.getWholesalePriceActivated());
      quickEditV2Request.setCncActive(quickEditV2WebRequest.getCncActivated());
      quickEditV2Request.setScheduleUpdate(quickEditV2WebRequest.isScheduleRemoval());
      if (Objects.nonNull(quickEditV2WebRequest.getB2bFields())) {
        B2bFieldsRequest b2bFieldsRequest = new B2bFieldsRequest();
        b2bFieldsRequest.setManaged(quickEditV2WebRequest.getB2bFields().isManaged());
        b2bFieldsRequest.setBasePrice(quickEditV2WebRequest.getB2bFields().getBasePrice());
        b2bFieldsRequest.setStatus(ProductLevel3Status.valueOf(quickEditV2WebRequest.getB2bFields().getStatus()));
        quickEditV2Request.setB2bFieldsRequest(b2bFieldsRequest);
      }
      quickEditV2Requests.add(quickEditV2Request);
    }
    return quickEditV2Requests;
  }

  public static ItemLevel4ListingWebRequest toItemLevel4WebRequest(ItemLevel4WebRequest request) {
    ItemLevel4ListingWebRequest itemLevel4ListingWebRequest = new ItemLevel4ListingWebRequest();
    if (Objects.nonNull(request)) {
      for (String productSku : request.getProductSkus()) {
        GdnPreconditions.checkArgument(Constants.PRODUCT_SKU_PATTERN.matcher(productSku).matches(),
          ErrorMessages.INVALID_PATTERN_SKU);
      }
      if (CollectionUtils.isNotEmpty(request.getProductSkus())) {
        Set<String> productSkus = request.getProductSkus();
        itemLevel4ListingWebRequest.setProductSkus(productSkus);
      }
    } else {
      throw new ApplicationException(HttpStatus.BAD_REQUEST, ErrorCategory.INVALID_STATE.getCode(),
        ErrorMessages.INVALID_STATE);
    }
    return itemLevel4ListingWebRequest;
  }


  public static PickupPointFilterRequest toPickupPointFilterRequest(
      PickupPointSummaryWebRequest pickupPointSummaryWebRequest, Set<String> accessiblePickupPointCodes,
      boolean setWaitingDeletionForDeletePickupPoint) {
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    BeanUtils.copyProperties(pickupPointSummaryWebRequest, pickupPointFilterRequest);
    if(CollectionUtils.isNotEmpty(pickupPointSummaryWebRequest.getPickupPointCodes())) {
      pickupPointFilterRequest.setCodes(pickupPointSummaryWebRequest.getPickupPointCodes());
    } else {
      pickupPointFilterRequest.setCodes(accessiblePickupPointCodes);
    }
    setWaitingDeletionFlagForDeletePickupPoint(setWaitingDeletionForDeletePickupPoint, pickupPointFilterRequest);
    return pickupPointFilterRequest;
  }

  public static void setWaitingDeletionFlagForDeletePickupPoint(boolean setWaitingDeletionForDeletePickupPoint,
      PickupPointFilterRequest pickupPointFilterRequest) {
    if (setWaitingDeletionForDeletePickupPoint) {
      pickupPointFilterRequest.setWaitingDeletion(false);
    }
  }

  public static PickupPointSummaryRequest toPickupPointSummaryRequest(
      PickupPointSummaryWebRequest pickupPointSummaryWebRequest, Set<String> accessiblePickupPoints) {
    PickupPointSummaryRequest pickupPointSummaryRequest = new PickupPointSummaryRequest();
    BeanUtils.copyProperties(pickupPointSummaryWebRequest, pickupPointSummaryRequest);
    pickupPointSummaryRequest.setCodes(accessiblePickupPoints);
    return pickupPointSummaryRequest;
  }

  public static ProductSummaryRequest toProductSummaryRequest(ProductSummaryV2WebRequest request) {
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    BeanUtils.copyProperties(request, productSummaryRequest);
    productSummaryRequest.setSizeAttributeCode(request.getAttributeCode());
    return productSummaryRequest;
  }

  public static String toL5Id(String webItemSku, String pickupPointCode) {
    return new StringBuilder().append(webItemSku).append(Constants.DASH_SEPARATOR)
        .append(pickupPointCode).toString();
  }

  public static String toProductDetailPage(String productSku, String productDetailPageUrlPrefix) {
    String productDetailLink =
      Optional.ofNullable(productSku).map(sku -> sku.replace(Constants.DASH_SEPARATOR, Constants.DOT_SEPARATOR))
        .map(sku -> String.join(Constants.DOT_SEPARATOR, sku, "html"))
        .orElse(org.apache.commons.lang3.StringUtils.EMPTY);
    return String.join("-", productDetailPageUrlPrefix, productDetailLink);
  }

  public static List<ItemInfoDto> toItemDTOLIstFromItemPickupPointRequest(
    List<ItemPickupPointWebRequest> itemPickupPointWebRequestList) {
    return itemPickupPointWebRequestList.stream().map(
      itemPickupPointRequest -> ItemInfoDto.builder().itemSku(itemPickupPointRequest.getItemSku())
        .pickupPointCode(itemPickupPointRequest.getPickupPointId()).build()).collect(toList());
  }

  public static ItemPickupPointListingL3Request toItemPickupPointListingL3Request(String productSku,
      ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest) {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    BeanUtils.copyProperties(itemPickupPointListingL3WebRequest, itemPickupPointListingL3Request);
    itemPickupPointListingL3Request.setProductSku(productSku);
    return itemPickupPointListingL3Request;
  }

  public static BulkProcessDeleteOfflineItemRequest toBulkProcessDeleteOfflineItemRequest(
     String businessPartnerCode, String username, String clientId,
    MultipartFile file, Set<String> accessiblePickupPointCodes) throws IOException {
    BulkProcessDeleteOfflineItemRequest request = new BulkProcessDeleteOfflineItemRequest();
    request.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue());
    request.setBusinessPartnerCode(businessPartnerCode);
    request.setFileContent(file.getBytes());
    request.setFileName(file.getOriginalFilename());
    request.setUpdatedBy(username);
    request.setClientHost(clientId);
    request.setPrivilegedMap(Collections.emptyMap());
    request.setAccessiblePickupPoints(accessiblePickupPointCodes);
    return request;
  }

  public static void validateRequest(ProductL3UpdateRequest request) {
    validateBaseRequest(request);
    request.setDescription(validateDescription(request.getDescription()));
    request.setUniqueSellingPoint(validateData(request.getUniqueSellingPoint()));
    request.setSpecificationDetail(validateData(request.getSpecificationDetail()));
    request.setProductName(validateData(request.getProductName()));
    request.setProductSku(validateData(request.getProductSku()));
    request.setProductStory(validateData(request.getProductStory()));
    request.setProductCode(validateData(request.getProductCode()));
    request.setUrl(validateData(request.getUrl()));
    request.setAccessChannel(validateData(request.getAccessChannel()));
    request.setBusinessPartnerCode(validateData(request.getBusinessPartnerCode()));
    request.setCategoryCode(validateData(request.getCategoryCode()));
    request.setCategoryHierarchy(validateData(request.getCategoryHierarchy()));
    request.setCategoryName(validateData(request.getCategoryName()));

    if (Objects.nonNull(request.getAttributes())) {
      for (ProductLevel3AttributeRequest productLevel3AttributeRequest : request.getAttributes()) {
        validateBaseRequest(productLevel3AttributeRequest);
        productLevel3AttributeRequest
          .setAttributeName(validateData(productLevel3AttributeRequest.getAttributeName()));
        productLevel3AttributeRequest
          .setAttributeCode(validateData(productLevel3AttributeRequest.getAttributeCode()));
        productLevel3AttributeRequest
          .setAttributeType(validateData(productLevel3AttributeRequest.getAttributeType()));
        productLevel3AttributeRequest.setId(validateData(productLevel3AttributeRequest.getId()));
        productLevel3AttributeRequest
          .setItemSku(validateData(productLevel3AttributeRequest.getItemSku()));
      }
    }

    if (Objects.nonNull(request.getCommonImages())) {
      for (ProductLevel3SummaryDetailsImageRequest productLevel3ImageRequest : request.getCommonImages()) {
        productLevel3ImageRequest
          .setLocationPath(validateData(productLevel3ImageRequest.getLocationPath()));
      }
    }
  }

  public static MarkPickupPointAsDefaultRequest toMarkPickupPointAsDefaultRequest(String merchantCode,
      DefaultConfigurationAndPickupPointRequest request) {
    MarkPickupPointAsDefaultRequest markPickupPointAsDefaultRequest = new MarkPickupPointAsDefaultRequest();
    markPickupPointAsDefaultRequest.setStoreCode(merchantCode);
    markPickupPointAsDefaultRequest.setPickupPointCodes(request.getPickupCodes());
    return markPickupPointAsDefaultRequest;
  }

  public static ProfileRequest toProfileRequest(String merchantCode, DefaultConfigurationAndPickupPointRequest request) {
    ProfileRequest profileRequest = new ProfileRequest();
    ProductSettingDTO productSettings = new ProductSettingDTO();
    productSettings.setRegular(request.getProductSettings().isRegular());
    productSettings.setCnc(request.getProductSettings().isCnc());
    productSettings.setFbb(request.getProductSettings().isFbb());
    productSettings.setOfflineToOnline(request.getProductSettings().isOfflineToOnline());
    profileRequest.setProductSettings(productSettings);
    profileRequest.setBusinessPartnerCode(merchantCode);
    return profileRequest;
  }

  public static List<QuickEditWebRequest> toQuickEditWebRequestList(
    List<QuickEditV2WebRequest> quickEditV2WebRequests) {
    List<QuickEditWebRequest> quickEditWebRequestList = new ArrayList<>();
    for (QuickEditV2WebRequest quickEditV2WebRequest : quickEditV2WebRequests) {
      QuickEditWebRequest quickEditWebRequest = new QuickEditWebRequest();
      BeanUtils.copyProperties(quickEditV2WebRequest, quickEditWebRequest);
      quickEditWebRequest.setWholesalePriceActivated(
        quickEditV2WebRequest.getWholesalePriceActivated());
      quickEditWebRequest.setPrices(new ArrayList<>(quickEditV2WebRequest.getPrices()));
      quickEditWebRequest.setSynchronizeStock(quickEditV2WebRequest.getSynchronizeStock());
      quickEditWebRequest.setPickupPointCode(quickEditV2WebRequest.getPickupPointCode());
      quickEditWebRequestList.add(quickEditWebRequest);
    }
    return quickEditWebRequestList;
  }

  public static ProductSummaryWebRequest toProductSummaryWebRequest(
    ProductSummaryV2WebRequest request) {
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    BeanUtils.copyProperties(request, productSummaryWebRequest);
    return productSummaryWebRequest;
  }

  public static void profileResponseAndRequestValidation(ProfileResponse profileResponse,
    ProductL3UpdateRequest request) throws ApplicationRuntimeException {
    if (Objects.isNull(profileResponse) || !Constants.ACTIVE.equals(profileResponse.getMerchantStatus())) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.BUSINESS_PARTNER_NOT_ACTIVE);
    }
    RequestHelper.validateRequest(request);
  }

  public static ProductVariantUpdateRequest toProductVariantUpdateRequest(
      ProductVariantUpdateWebRequest productVariantUpdateWebRequest, String businessPartnerCode) {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setProductCode(productVariantUpdateWebRequest.getProductCode());
    productVariantUpdateRequest.setProductSku(productVariantUpdateWebRequest.getProductSku());
    productVariantUpdateRequest.setSynchronize(productVariantUpdateWebRequest.isSynchronize());
    productVariantUpdateRequest.setNeedCorrection(productVariantUpdateWebRequest.isNeedCorrection());
    productVariantUpdateRequest.setProductEditable(productVariantUpdateWebRequest.isProductEditable());
    productVariantUpdateRequest.setBusinessPartnerCode(businessPartnerCode);
    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    for (ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest : productVariantUpdateWebRequest
        .getProductItems()) {
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
          new ProductVariantPriceStockAndImagesRequest();
      BeanUtils.copyProperties(productVariantPriceStockAndImagesWebRequest, productVariantPriceStockAndImagesRequest,
          "images", "modifiedItemPickupPoints");
      productVariantPriceStockAndImagesRequest.setImages(
          Optional.ofNullable(productVariantPriceStockAndImagesWebRequest.getImages()).orElse(new ArrayList<>())
              .stream().map(RequestHelper::toProductLevel3SummaryDetailsImageRequest).collect(toList()));
      productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
          Optional.ofNullable(productVariantPriceStockAndImagesWebRequest.getModifiedItemPickupPoints())
              .orElse(new ArrayList<>()).stream().map(RequestHelper::toItemPickupPointRequest).collect(toList()));
      productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints().forEach(
          itemPickupPointRequest -> itemPickupPointRequest
              .setSellerSku(productVariantPriceStockAndImagesWebRequest.getMerchantSku()));
      productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    }
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    productVariantUpdateRequest.setAddPickupPoints(
        Optional.ofNullable(productVariantUpdateWebRequest.getAddPickupPoints()).orElse(new ArrayList<>()).stream()
            .map(RequestHelper::toItemPickupPointRequest).collect(toList()));
    productVariantUpdateRequest.setDeletePickupPoints(
        Optional.ofNullable(productVariantUpdateWebRequest.getDeletePickupPoints()).orElse(new ArrayList<>()).stream()
            .map(RequestHelper::toPickupPointDeleteRequest).collect(toList()));
    return productVariantUpdateRequest;
  }

  private static ItemPickupPointRequest toItemPickupPointRequest(ItemPickupPointWebRequest itemPickupPointWebRequest) {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    BeanUtils.copyProperties(itemPickupPointWebRequest, itemPickupPointRequest, "productItemWholesalePriceRequests",
        "b2bFields");
    if (CollectionUtils.isNotEmpty(itemPickupPointWebRequest.getProductItemWholesalePriceRequests())) {
      itemPickupPointRequest.setProductItemWholesalePriceRequests(
          itemPickupPointWebRequest.getProductItemWholesalePriceRequests().stream().map(
              productItemWholesalePriceWebRequest -> new ProductItemWholesalePriceRequest(
                  productItemWholesalePriceWebRequest.getQuantity(),
                  productItemWholesalePriceWebRequest.getWholesaleDiscount())).collect(toList()));
    }
    if (Objects.nonNull(itemPickupPointWebRequest.getB2bFields())) {
      B2BFields b2BFields = new B2BFields();
      BeanUtils.copyProperties(itemPickupPointWebRequest.getB2bFields(), b2BFields);
      itemPickupPointRequest.setB2bFields(b2BFields);
    }
    if (Objects.nonNull(itemPickupPointWebRequest.getBuyableSchedule())) {
      itemPickupPointRequest.setBuyableSchedule(new BuyableScheduleRequest());
      BeanUtils.copyProperties(itemPickupPointWebRequest.getBuyableSchedule(),
        itemPickupPointRequest.getBuyableSchedule());
    }
    if (Objects.nonNull(itemPickupPointWebRequest.getDiscoverableSchedule())) {
      itemPickupPointRequest.setDiscoverableSchedule(new DiscoverableScheduleRequest());
      BeanUtils.copyProperties(itemPickupPointWebRequest.getDiscoverableSchedule(),
        itemPickupPointRequest.getDiscoverableSchedule());
    }
    itemPickupPointRequest.setScheduleRemoval(itemPickupPointWebRequest.isScheduleRemoval());
    return itemPickupPointRequest;
  }

  private static PickupPointDeleteRequest toPickupPointDeleteRequest(
      ItemPickupPointDeleteWebRequest itemPickupPointDeleteWebRequest) {
    PickupPointDeleteRequest pickupPointDeleteRequest = new PickupPointDeleteRequest();
    pickupPointDeleteRequest.setItemSku(itemPickupPointDeleteWebRequest.getItemSku());
    pickupPointDeleteRequest.setPickupPointId(itemPickupPointDeleteWebRequest.getPickupPointId());
    return pickupPointDeleteRequest;
  }

  public static void setTimeFilterForHistoryRequest(HistoryUpdateRequest historyUpdateRequest) {
    Instant currentInstant = Instant.now();
    if (historyUpdateRequest.isBeforeOneMonths()) {
      if (Objects.isNull(historyUpdateRequest.getStartDate())) {
        historyUpdateRequest.setStartDate(
          Date.from(currentInstant.minus(Duration.ofDays(DAYS_IN_YEAR))));
      }
      if (Objects.isNull(historyUpdateRequest.getEndDate())) {
        historyUpdateRequest.setEndDate(Date.from(
          currentInstant.minus(Duration.ofDays(DAYS_IN_MONTH))
            .minus(Duration.ofHours(HOURS_IN_DAY))));
      }
    } else {
      if (Objects.isNull(historyUpdateRequest.getStartDate())) {
        historyUpdateRequest.setStartDate(
          Date.from(currentInstant.minus(Duration.ofDays(DAYS_IN_MONTH))));
      }
      if (Objects.isNull(historyUpdateRequest.getEndDate())) {
        historyUpdateRequest.setEndDate(Date.from(currentInstant));
      }
    }
  }

  public static MarkPickupPointAsDefaultRequest toMarkPickupPointDefaultRequest(String merchantCode,
      String pickupPointCode) {
    MarkPickupPointAsDefaultRequest markPickupPointAsDefaultRequest = new MarkPickupPointAsDefaultRequest();
    markPickupPointAsDefaultRequest.setStoreCode(merchantCode);
    markPickupPointAsDefaultRequest.setPickupPointCodes(Arrays.asList(pickupPointCode));
    return markPickupPointAsDefaultRequest;
  }

  public static List<ItemInfoDto> toItemInfoDtoList(List<WholeSaleDetailListWebRequest> wholeSaleDetailListWebRequests) {
    List<ItemInfoDto> itemInfoDtos = new ArrayList<>();
    for (WholeSaleDetailListWebRequest wholeSaleDetailListWebRequest : wholeSaleDetailListWebRequests) {
      ItemInfoDto itemInfoDto = new ItemInfoDto();
      itemInfoDto.setItemSku(wholeSaleDetailListWebRequest.getItemSku());
      itemInfoDto.setPickupPointCode(wholeSaleDetailListWebRequest.getPickupPointCode());
      itemInfoDto.setItemPickupPointId(itemInfoDto.getItemSku() + Constants.DASH_SEPARATOR + itemInfoDto.getPickupPointCode());
      itemInfoDtos.add(itemInfoDto);
    }
    return itemInfoDtos;
  }

  public static WholesalePriceSkuDetailListRequest getWholesalePriceSkuDetailListRequest(
      ProductPriceAndStockUpdateWebRequest productPriceAndStockUpdateWebRequest) {
    WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest = new WholesalePriceSkuDetailListRequest();
    wholesalePriceSkuDetailListRequest.setItemSkus(
        Collections.singleton(productPriceAndStockUpdateWebRequest.getItemSku()));
    wholesalePriceSkuDetailListRequest.setItemInfo(Collections.singletonList(
        new ItemInfoDto(productPriceAndStockUpdateWebRequest.getItemSku(),
            productPriceAndStockUpdateWebRequest.getPickupPointCode(),
            productPriceAndStockUpdateWebRequest.getItemSku() + Constants.DASH_SEPARATOR
                + productPriceAndStockUpdateWebRequest.getPickupPointCode())));
    return wholesalePriceSkuDetailListRequest;
  }

  public static List<BulkProcessStatusListingWebResponse> toBulkProcessStatusListingWebResponse(
    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse) {
    List<BulkProcessStatusListingWebResponse> bulkProcessStatusListingWebResponses =
      new ArrayList<>();
    if (CollectionUtils.isNotEmpty(listingResponse.getContent())) {
      for (BulkProcessStatusListingResponse bulkProcessStatusListingResponse : listingResponse.getContent()) {
        BulkProcessStatusListingWebResponse webResponse = new BulkProcessStatusListingWebResponse();
        BeanUtils.copyProperties(bulkProcessStatusListingResponse, webResponse, "status",
          "successRowCount", "uploadedFileName", "processCompletionPercentage");
        webResponse.setUploadedFileName(
          Objects.nonNull(bulkProcessStatusListingResponse.getUploadedFileName()) ?
            bulkProcessStatusListingResponse.getUploadedFileName() :
            StringUtils.EMPTY);
        webResponse.setStatus(
          Objects.nonNull(bulkProcessStatusListingResponse.getBulkActivityStatus()) ?
            mapBulkActivityStatus(bulkProcessStatusListingResponse.getBulkActivityStatus()) :
            BulkActivityStatusWeb.NA);
        webResponse.setProcessCompletionPercentage(bulkProcessStatusListingResponse.getProcessCompletionPercentage());
        webResponse.setSuccessRowCount(bulkProcessStatusListingResponse.getSuccessRowCount());
        webResponse.setEstimatedCompletionTime(bulkProcessStatusListingResponse.getEstimatedCompletionTime());
        bulkProcessStatusListingWebResponses.add(webResponse);
      }
    }
    return bulkProcessStatusListingWebResponses;
  }

  private static BulkActivityStatusWeb mapBulkActivityStatus(BulkActivityStatus sourceStatus) {
    switch (sourceStatus) {
      case SUCCESS:
        return BulkActivityStatusWeb.SUCCESS;
      case PARTIAL_SUCCESS:
        return BulkActivityStatusWeb.PARTIAL_SUCCESS;
      case FAILED:
        return BulkActivityStatusWeb.FAILED;
      case IN_PROGRESS:
        return BulkActivityStatusWeb.IN_PROGRESS;
      case PENDING:
        return BulkActivityStatusWeb.PENDING;
      default:
        return BulkActivityStatusWeb.NA;
    }
  }

  public static void validateManualQRCodeRequest(ManualQRCodeRequest request) {
    validateQrTemplateDetails(request);

    if (request.isAllStores() && CollectionUtils.isNotEmpty(
      request.getProductDetailsRequestList())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ErrorMessages.INVALID_REQUEST);
    }
    if (AllowedQRGenerationType.STORE.equals(request.getQrGenerationType())
        && request.isPrintPrice()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.INVALID_REQUEST);
    }

    if (AllowedQRGenerationType.ALL_PRODUCTS.equals(request.getQrGenerationType())
        && request.isPrintPrice()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.INVALID_REQUEST);
    }
  }

  private static void validateQrTemplateDetails(ManualQRCodeRequest request) {
    switch (request.getTemplateSize()) {
      case A5:
        checkQrPerPage(request.getQrPerPage());

        if (AllowedNumberOfQRPerPage.ONE.getNumberValue() != request.getQrPerPage()
            && AllowedNumberOfQRPerPage.TWO.getNumberValue() != request.getQrPerPage()) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ErrorMessages.INVALID_NUMBER_OF_QR_PER_PAGE);
        }
        break;
      case SINGLE_QR:
        if (Objects.nonNull(request.getIsDarkTheme())) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ErrorMessages.INVALID_REQUEST);
        }
        checkQrPerPage(request.getQrPerPage());
        if (AllowedNumberOfQRPerPage.ONE.getNumberValue() != request.getQrPerPage()
            && AllowedNumberOfQRPerPage.SIXTEEN.getNumberValue() != request.getQrPerPage()
            && AllowedNumberOfQRPerPage.THIRTY_TWO.getNumberValue() != request.getQrPerPage()) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ErrorMessages.INVALID_NUMBER_OF_QR_PER_PAGE);
        }
        break;
      case A1:
        if (Objects.nonNull(request.getQrPerPage())) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ErrorMessages.INVALID_NUMBER_OF_QR_PER_PAGE);
        }
        checkDarkTheme(request.getIsDarkTheme());

        break;

      case BY46:
        checkQrPerPage(request.getQrPerPage());
        checkDarkTheme(request.getIsDarkTheme());
        if (AllowedNumberOfQRPerPage.ONE.getNumberValue() != request.getQrPerPage()
            && AllowedNumberOfQRPerPage.EIGHTEEN.getNumberValue() != request.getQrPerPage()) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ErrorMessages.INVALID_NUMBER_OF_QR_PER_PAGE);
        }

        break;
      case BY712:
        checkQrPerPage(request.getQrPerPage());
        checkDarkTheme(request.getIsDarkTheme());
        if (AllowedNumberOfQRPerPage.ONE.getNumberValue() != request.getQrPerPage()
            && AllowedNumberOfQRPerPage.FOUR.getNumberValue() != request.getQrPerPage()) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ErrorMessages.INVALID_NUMBER_OF_QR_PER_PAGE);
        }
        break;
    }
  }

  private static void checkQrPerPage(Integer qrPerPage) {
    if (Objects.isNull(qrPerPage)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.INVALID_REQUEST);
    }
  }

  private static void checkDarkTheme(Boolean isDarkTheme) {
    if (Objects.isNull(isDarkTheme)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.INVALID_REQUEST);
    }
  }

  public static DownloadQRCodeRequest populateDownloadQrCodeRequest(String storeId,
      String requestId, String businessPartnerCode, String merchantName,
      ManualQRCodeRequest request) {
    DownloadQRCodeRequest downloadQRCodeRequest =
        new DownloadQRCodeRequest();
    BeanUtils.copyProperties(request, downloadQRCodeRequest);
    downloadQRCodeRequest.setMerchantCode(businessPartnerCode);
    downloadQRCodeRequest.setStoreId(storeId);
    downloadQRCodeRequest.setRequestId(requestId);
    downloadQRCodeRequest.setTemplateSize(request.getTemplateSize().name());
    downloadQRCodeRequest.setMerchantName(merchantName);
    downloadQRCodeRequest.setQrGenerationType(request.getQrGenerationType().name());

    int qrPerPage = Objects.isNull(request.getQrPerPage()) ? 1 : request.getQrPerPage();
    downloadQRCodeRequest.setQrPerPage(qrPerPage);
    return downloadQRCodeRequest;
  }

  public static QrExcelUploadRequest populateQrExcelUploadRequest(String storeId,
    String requestId, String businessPartnerCode, String merchantName,
    ManualQRCodeRequest request, MultipartFile multipartFile) throws IOException {
    QrExcelUploadRequest qrExcelUploadRequest = new QrExcelUploadRequest();
    qrExcelUploadRequest.setDownloadQRCodeRequest(populateDownloadQrCodeRequest(storeId, requestId, businessPartnerCode,
      merchantName, request));
    qrExcelUploadRequest.setFileContent(multipartFile.getBytes());
    qrExcelUploadRequest.setFileName(multipartFile.getOriginalFilename());
    return qrExcelUploadRequest;
  }

  public static void validateExcelQRCodeRequest(ManualQRCodeRequest manualQRCodeRequest) {
    validateQrTemplateDetails(manualQRCodeRequest);
    if (AllowedQRGenerationType.STORE.equals(manualQRCodeRequest.getQrGenerationType())
        || AllowedQRGenerationType.ALL_PRODUCTS.equals(manualQRCodeRequest.getQrGenerationType())) {
      log.error("Request - {} for excel upload has been requested for STORE qrGenerationType",
        manualQRCodeRequest);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ErrorMessages.INVALID_REQUEST);
    }
  }

  private static List<String> getSalesChannelFromProfileResponse(ProfileResponse profileResponse) {
    List<String> salesChannel = new ArrayList<>();
    if (Objects.nonNull(profileResponse.getCompany()) && CollectionUtils.isNotEmpty(
      profileResponse.getCompany().getSalesChannel())) {
      salesChannel = profileResponse.getCompany().getSalesChannel();
    }
    return salesChannel;
  }

  public static MerchantType getMerchantType(ProfileResponse profileResponse) {
    List<String> salesChannel = getSalesChannelFromProfileResponse(profileResponse);
    if (Objects.nonNull(profileResponse.getCompany()) && profileResponse.getCompany()
      .isCncActivated() && !salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
      return MerchantType.CNC_SELLER;
    } else if (Objects.nonNull(profileResponse.getCompany()) && salesChannel.contains(
      Constants.B2B_SELLER_CHANNEL) && profileResponse.getCompany().isCncActivated()) {
      return MerchantType.BFB_CNC_SELLER;
    } else if (salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
      return MerchantType.BFB_SELLER;
    } else
      return MerchantType.PURE_DELIVERY_SELLER;
  }

  public static void validateAuthorisation(String clientSellerCode, String requestSellerCode) {
    if (!StringUtils.equals(requestSellerCode, clientSellerCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION,
        ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  public static void validateBundlingAllowedBasedOnSellerType(ProfileResponse profileResponse,
      String bundlingAllowedSellerType) {
    boolean sellerAllowed = false;
    try {
      String merchantType = profileResponse.getCompany().getMerchantType();
      sellerAllowed = bundlingAllowedSellerType.contains(merchantType);
    } catch (Exception e) {
      log.error("Exception while fetching the merchantType for profileResponse : {}", profileResponse);
    }
    if (!sellerAllowed) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_SELLER_TYPE_FOR_ASSEMBLY);
    }
  }

  public static void validateSizeForExcelUploads(MultipartFile multipartFile,
    long excelTemplateUploadSizeThresholdInMB) {
    if (Long.compare(multipartFile.getSize(), excelTemplateUploadSizeThresholdInMB * SIZE_IN_BYTES) == 1) {
      log.error("File {} exceeded max size threshold .", multipartFile.getOriginalFilename());
      throw new ApiIncorrectInputDataException(
        String.format(ApiErrorCode.INVALID_EXCEL_SIZE.getDesc(),
          excelTemplateUploadSizeThresholdInMB), ApiErrorCode.INVALID_EXCEL_SIZE.getCode());
    }
  }

  public static UploadAttributeImageRequest toUploadAttributeImageRequest(String imageFileName,
      byte[] bytes, String originalFileName) {
    String lowercaseFilename =
        imageFileName.replaceAll(Constants.SPECIAL_CHARS_REGEX, Constants.DASH_SEPARATOR)
            .toLowerCase();
    return UploadAttributeImageRequest.builder().imageFileName(lowercaseFilename).bytes(bytes)
        .originalFileType(originalFileName.substring(
            originalFileName.lastIndexOf(Constants.DOT) + Constants.INDEX_COUNT)).build();
  }

  public static void validateProductSkuByBusinessPartnerCode(boolean validateBusinessPartnerCodeForSecurityEnabled,
      String businessPartnerCode, Set<String> productSkuList) {
    if (CollectionUtils.isNotEmpty(productSkuList)) {
      productSkuList.stream().map(productSku -> StringUtils.defaultIfEmpty(productSku, StringUtils.EMPTY))
          .forEach(productSku -> {
            if (validateBusinessPartnerCodeForSecurityEnabled && !productSku.startsWith(businessPartnerCode)) {
              throw new ValidationException(ErrorMessages.INVALID_GDN_SKU);
            }
          });
    }
  }

  public static void validateAccessibilityForAssemblyAndDisassembly(String type) {
      List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
    if (!(accessibility.contains(Accessibilty.ASSEMBLY_REQUEST_LISTING) && StringUtils.equals(type, ASSEMBLY_REQUEST)
        || (accessibility.contains(Accessibilty.DISASSEMBLY_REQUEST_LISTING) && StringUtils.equals(type,
        DISASSEMBLY_REQUEST)) || (StringUtils.equals(type, TRANSFER_REQUEST) && accessibility.contains(
        TRANSFER_PRODUCT_LISTING)))) {
        throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  public static void checkProductSkuOrItemSkuStartsWithBPCode(List<String> productOrItemSkus,
      boolean validateBusinessPartnerCodeForSecurityEnabled , String businessPartnerCode) {
    if (validateBusinessPartnerCodeForSecurityEnabled && !productOrItemSkus.stream()
        .allMatch(itemSku -> itemSku.startsWith(businessPartnerCode))) {
      throw new ValidationException(ErrorMessages.INVALID_GDN_SKU);
    }
  }

  public static ProductSizeChartUpdateRequest toProductSizeChartUpdateRequest(
      ProductSizeChartUpdateWebRequest productSizeChartUpdateWebRequest) {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest = new ProductSizeChartUpdateRequest();
    BeanUtils.copyProperties(productSizeChartUpdateWebRequest, productSizeChartUpdateRequest);
    return productSizeChartUpdateRequest;
  }

  public static Set<String> validateModifiedPickupPoints(
      List<ProductVariantPriceStockAndImagesWebRequest> productVariantPriceStockAndImagesWebRequestList) {
   return productVariantPriceStockAndImagesWebRequestList.stream().flatMap(
            productVariantPriceStockAndImagesWebRequest -> productVariantPriceStockAndImagesWebRequest.getModifiedItemPickupPoints()
                .stream()).filter(response -> !Boolean.TRUE.equals(response.getPickupPointNotUpdated()))
        .map(ItemPickupPointWebRequest::getPickupPointId).collect(Collectors.toSet());
  }

  public static boolean isBopisEligible(String merchantType, boolean bopisEligible,
      boolean bopisCategoryRestrictionEnabled, List<String> bopisUnsupportedMerchantTypes, boolean isOnlyExternal) {
    if (bopisCategoryRestrictionEnabled && !bopisEligible && isOnlyExternal) {
      return !bopisUnsupportedMerchantTypes.contains(merchantType);
    }
    return true;
  }

  public static String getMerchantTypeFromProfileResponse(ProfileResponse profileResponse) {
    if (Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany()) && StringUtils.isNotBlank(
        profileResponse.getCompany().getMerchantType())) {
      return profileResponse.getCompany().getMerchantType();
    }
    return StringUtils.EMPTY;
  }

  public static void validateProductEditAccessibility(boolean validationEnabled, Set<String> excludedChannelIds, String channelId) {
    if (validationEnabled && !excludedChannelIds.contains(channelId)) {
      if (!Arrays.asList(Credential.getAccessibilities()).contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT)) {
        throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
      }
    }
  }

  public static boolean isInstoreEligibleSeller(ProfileResponse profileResponse) {
    return Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany())
        && profileResponse.getCompany().isOfflineToOnlineFlag();
  }

  public static boolean isInstoreEligibleSellerWithSwitch(boolean instoreNewFlowEnabled,
      ProfileResponse profileResponse) {
    return instoreNewFlowEnabled && isInstoreEligibleSeller(profileResponse);
  }

  public static boolean checkHideFromSellerAttribute(boolean productSuitabilityFeatureEnabled,
      CategoryAttributeResponse categoryAttribute) {
    return productSuitabilityFeatureEnabled && categoryAttribute.getAttribute().isHideForSeller();
  }

  public static void validateAccessibilityForProductTab(boolean validateProductAccessibility,
      List<String> accessibility, boolean isExternal, String productAccessibilityList, String clientType) {
    if (validateProductAccessibility && isExternal && StringUtils.isNotBlank(productAccessibilityList)
        && !Constants.CLIENT_TYPE_SELLER_API.equals(clientType)) {
      String[] productAccessibility = productAccessibilityList.split(Constants.COMMA_DELIMITER_NO_SPACE);
      Arrays.stream(productAccessibility).filter(access -> !accessibility.contains(access)).forEach(access -> {
        throw new UnauthorizedException(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
      });
    }
  }

  public static ProductBasicInfoDownloadRequest toProductBasicInfoDownloadRequest(String username,
      String businessPartnerCode, Map<String, Boolean> privilegeMap, String randomUUID, String fileName,
      ProductSummaryRequest productSummaryRequest) {
    ProductBasicInfoDownloadRequest productBasicInfoDownloadRequest = new ProductBasicInfoDownloadRequest();
    productBasicInfoDownloadRequest.setPrivilegedMap(privilegeMap);
    productBasicInfoDownloadRequest.setProductSummaryRequest(productSummaryRequest);
    productBasicInfoDownloadRequest.setLanguage(LANGUAGE);
    productBasicInfoDownloadRequest.setRequestId(randomUUID);
    productBasicInfoDownloadRequest.setDownloadType(DownloadType.ALL);
    productBasicInfoDownloadRequest.setFileType(FileType.XLSX);
    productBasicInfoDownloadRequest.setBulkProcessEntity(BulkProcessEntity.PRODUCT_BASIC_INFO);
    productBasicInfoDownloadRequest.setFilename(fileName);
    productBasicInfoDownloadRequest.setMerchantId(businessPartnerCode);
    productBasicInfoDownloadRequest.setUsername(username);
    productBasicInfoDownloadRequest.setDirectDownload(true);
    return productBasicInfoDownloadRequest;
  }

  public static void validateBusinessPartnerCode(String businessPartnerCode, ProductSummaryWebRequest request,
      boolean validateBusinessPartnerCodeForSecurityEnabled) {
    if (validateBusinessPartnerCodeForSecurityEnabled && !StringUtils.equals(businessPartnerCode,
        request.getMerchantCode())) {
      throw new ValidationException(ErrorMessages.INVALID_GDN_SKU);
    }
  }

  public static void validateActiveBusinessPartner(String businessPartnerCode,
      ProfileResponse profileResponse) {
    GdnPreconditions.checkArgument(
        Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
        ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
  }

  public static OmniChannelExistsRequest mapToOmniChannelSkuRequest(
    OmniChannelSkuWebRequest webRequest) {
    OmniChannelExistsRequest request = new OmniChannelExistsRequest();
    request.setSellerCode(webRequest.getSellerCode());
    request.setSellerSkus(webRequest.getOmniChannelSkus());
    return request;
  }

  public static boolean getBusinessPartnerFlagValue(ProfileResponse profileResponse, String flagName) {
    return Optional.ofNullable(profileResponse).map(ProfileResponse::getFlags).filter(MapUtils::isNotEmpty)
        .map(flags -> flags.get(flagName)).map(Object::toString).map(Boolean::parseBoolean)
        .orElse(false);
  }
}