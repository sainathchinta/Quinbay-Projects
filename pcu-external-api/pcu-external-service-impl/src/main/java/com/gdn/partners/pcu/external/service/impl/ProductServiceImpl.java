package com.gdn.partners.pcu.external.service.impl;

import static com.gdn.partners.pcu.external.service.impl.helper.RequestHelper.checkProductSkuOrItemSkuStartsWithBPCode;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.StringJoiner;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.gdn.partners.pcu.external.client.model.BusinessPartnerPickupPointOutboundResponse;
import com.gdn.partners.pcu.external.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.external.service.impl.helper.BeanUtils;
import jakarta.servlet.http.HttpServletResponse;

import com.gdn.partners.pcu.external.streaming.model.bulk.ProductBasicInfoDownloadRequest;
import com.gdn.partners.pcu.external.streaming.model.bulk.ProductDownloadEANRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelProductListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpcStatusWebRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductBasicWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ReelProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcStatusWebResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.UpcStatusRequest;
import com.gdn.x.product.rest.web.model.response.UpcStatusResponse;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.google.common.collect.Lists;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import com.blibli.oss.common.response.Response;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gda.mta.product.dto.BulkDeleteProductWipRequest;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemLevel3Request;
import com.gda.mta.product.dto.ProductLevel3OrderResponse;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditRequest;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3StockInfoWebSiteResponse;
import com.gda.mta.product.dto.ProductLevel3StockRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryCountResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductSkuAndPickupPointCodeRequest;
import com.gda.mta.product.dto.ProductSkuListRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gda.mta.product.dto.UpdateImageRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gda.mta.product.dto.response.ImageResponse;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductItemNameResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.ProductSkuResponseList;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.ProductSystemParameterSwitchResponse;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.fbb.core.web.model.request.CountConsignmentFormsByItemSkusRequest;
import com.gdn.fbb.core.web.model.response.v3.CountConsignmentFormsByItemSkuResponse;
import com.gdn.mta.product.commons.constant.UpdateProductAccessChannel;
import com.gdn.mta.product.entity.PickupPointCodeResponse;
import com.gdn.mta.product.entity.UniquePickupPointCodeResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pbp.dto.productcategory.CategoryHierarchyProductCountResponse;
import com.gdn.partners.pbp.dto.productlevel1.ProductSearchRequest;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.EstimateItemPriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.PostLiveProductCountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pbp.model.SortOrder;
import com.gdn.partners.pcu.external.client.feign.AGPQueryFeign;
import com.gdn.partners.pcu.external.client.feign.FbbFeign;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.client.feign.PDTFeign;
import com.gdn.partners.pcu.external.client.feign.ProductAssemblyFeign;
import com.gdn.partners.pcu.external.client.feign.ProductPricingFeign;
import com.gdn.partners.pcu.external.client.feign.XCampaignFeign;
import com.gdn.partners.pcu.external.client.feign.XInventoryFeign;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.client.helper.AgpSimpleQueryResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.client.model.InventoryDetailStockInfoRequestDTO;
import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.properties.GCSProperties;
import com.gdn.partners.pcu.external.service.BrandService;
import com.gdn.partners.pcu.external.service.BulkProcessService;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.CategoryService;
import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.service.GCSService;
import com.gdn.partners.pcu.external.service.ImageService;
import com.gdn.partners.pcu.external.service.PickupPointService;
import com.gdn.partners.pcu.external.service.ProductAssemblyService;
import com.gdn.partners.pcu.external.service.ProductPricingService;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.service.impl.config.BeanUtilsConfigurer;
import com.gdn.partners.pcu.external.service.impl.config.KafkaTopicProperties;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.service.impl.helper.ExcelTemplateUtil;
import com.gdn.partners.pcu.external.service.impl.helper.ImageValidator;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ValidateUrlUtil;
import com.gdn.partners.pcu.external.client.model.SubmitEvidenceIPRRequest;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.partners.pcu.external.streaming.model.bulk.BulkDownloadRequest;
import com.gdn.partners.pcu.external.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.external.streaming.model.bulk.DownloadType;
import com.gdn.partners.pcu.external.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.external.streaming.model.bulk.ProductDownloadRequest;
import com.gdn.partners.pcu.external.web.model.enums.InActiveProductStatus;
import com.gdn.partners.pcu.external.web.model.enums.MerchantType;
import com.gdn.partners.pcu.external.web.model.request.ActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.AppealProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkRequest;
import com.gdn.partners.pcu.external.web.model.request.HistorySummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.HistoryUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InProcessProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemLevel4WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.NeedRevisionSubmitWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PriceChangeCompatibleRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductImageEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductL3ListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3UpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3VariantsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductPriceAndStockUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.SuspensionWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpdateItemsPriceStockImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.WholeSaleDetailListWebRequest;
import com.gdn.partners.pcu.external.web.model.response.ActiveProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ApiErrorCode;
import com.gdn.partners.pcu.external.web.model.response.AppealProductConfigResponse;
import com.gdn.partners.pcu.external.web.model.response.BrandPredefinedAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerPickupPointWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategorySuggestionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CreateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.DistinctPickUpPoint;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.EstimateItemPriceWebResponse;
import com.gdn.partners.pcu.external.web.model.response.HistorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.HistoryUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InProcessWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventoryWarehouseStockWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemAttributeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemL5ListingResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemLevel4ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointListingL3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemsPriceStockImagesUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.MasterWarehouseListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.OrderPlacedWebResponse;
import com.gdn.partners.pcu.external.web.model.response.OrderStatusWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointCodeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PriceChangeCompatibleResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBundleRecipeEditableResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductCountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemLevel3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemNameWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3MasterWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryDetailsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductScoreRuleWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductSystemParameterSwitchWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ShippingTypeEligibility;
import com.gdn.partners.pcu.external.web.model.response.SimpleCategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SuspensionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.TemplateDownloadFilePathWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UniquePickupPointCodeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcCodeAndImagesWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesalePromoResponse;
import com.gdn.partners.pcu.external.web.model.response.YouTubeAPIWebResponse;
import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.commons.enums.InventoryFulfillment;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.request.CampaignPriceSkuRequest;
import com.gdn.x.campaign.request.ProductCampaignAvailabilityRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignPriceSkuResponse;
import com.gdn.x.campaign.response.ProductCampaignAvailabilityResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WarehouseInventoryStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummaryResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryDetailResponseDTO;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductNameSuggestionResponse;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.BrandPredefinedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.UPCCodeSearchRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.google.api.services.youtube.YouTube;
import com.google.cloud.storage.Blob;
import com.google.common.collect.ImmutableList;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductServiceImpl implements ProductService {

  private static final String VIEWABLE_PRODUCT_COUNT = "VIEWABLE_PRODUCT_COUNT";
  private static final long REDIS_PRODUCT_COUNT_TIMEOUT = 5l;
  private static final String BRAND_APPROVAL_STATUS_DRAFT = "DRAFT";
  private static final String BRAND_APPROVAL_STATUS_APPROVED = "APPROVED";
  private static final String ACTIVE_STATUS = "ACTIVE";
  private static final String INPROCESS_STATUS = "INPROCESS";
  private static final String INACTIVE_STATUS = "INACTIVE";
  private static final String ORDERBY = "createdDate";
  private static final String SORTBY = "desc";
  public static final String DEFAULT_CLIENT_HOST = "MTA_WEB";
  private static final String FILE_BULK_UPDATE_PRODUCT_TEMPLATE = "bulk-update-product-template";
  private static final String IN_REVIEW = StringUtils.SPACE + "(IN_REVIEW)";
  private static final String BRAND = "Brand";
  private static final String INTERNAL = "INTERNAL";
  private static final String LANGUAGE = "en";
  private static final String UNDERSCORE = "_";
  private static final String ACTIVE = "ACTIVE";
  public static final ImmutableList<String> MERCHANT_TYPE_ORDERS_FULLFILL_BY_BLIBLI =
      ImmutableList.of("CM", "RB");
  private static final String YOUTUBE_REGEX = "^(http(s)?:\\/\\/)?((w){3}.)?youtu(be|.be)?(\\.com)?\\/.+";
  private static final Pattern YOUTUBE_PATTERN = Pattern.compile(YOUTUBE_REGEX);
  private static final String IS_ONLY_EXTERNAL_USER = "isOnlyExternalUser";
  private static final String ORDERBY_ARCHIVED = "isArchived";
  private static final String SORTBY_ASC = "asc";
  public static final String JPEG = "jpeg";
  private static final String OTHERS = "OTHERS";
  private static final String XLSX = ".xlsx";
  private static final String PIPE = " || ";
  private static final String BULK_BASIC_INFO_FILE_NAME = "bulk-basic-info-update-template";

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private ProductAssemblyFeign productAssemblyFeign;

  @Autowired
  private ProductAssemblyService productAssemblyService;

  @Autowired
  private AGPQueryFeign agpQueryFeign;

  @Autowired
  private FbbFeign fbbFeign;

  @Autowired
  @Qualifier("stringRedisTemplate")
  private StringRedisTemplate productRedisTemplate;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private BrandService brandService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private XProductFeign xProductFeign;

  @Autowired
  private ProductPricingService productPricingService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private XCampaignFeign xCampaignFeign;

  @Autowired
  private XInventoryFeign xInventoryFeign;

  @Autowired
  private YouTube youTube;

  @Autowired
  private ProductPricingFeign productPricingFeign;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ImageService imageService;

  @Autowired
  private UserPicService userPicService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private PDTFeign pdtFeign;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private GCSProperties gcsProperties;

  @Autowired
  private GCSService gcsService;

  @Value("${sysparam.directory.template}")
  private String templateDirectory;

  @Value("${sysparam.directory.multipickuppoint.file.en}")
  private String multiPickupPointUploadTemplateFileEnglish;

  @Value("${sysparam.directory.multipickuppoint.file}")
  private String multiPickupPointUploadTemplateFile;

  @Value("${sysparam.directory.category.file}")
  private String categoryUploadTemplateFile;

  @Value("${sysparam.directory.category.file.en}")
  private String categoryUploadTemplateFileEnglish;

  @Value(value = "${youtube.data.api.key}")
  private String youTubeDataApiKey;

  @Value("${product.detail.page.url.prefix}")
  private String productDetailPageUrlPrefix;

  @Value("${preOrder.maximum.days}")
  private int preOrderMaximumDays;

  @Value("${preOrder.working.maximum.week}")
  private int preOrderMaximumWeek;

  @Value("${inventory.api.batch.size}")
  private int inventoryApiBatchSize;

  @Value("${pickupPointCode.batch.size:100}")
  private int pickupPointCodeBatchSize;

  @Value("${multipickuppoint.workflow.enabled:false}")
  private boolean multiPickupPointEnabled;

  @Value("${pricing.multipickuppoint.workflow.enabled}")
  private boolean pricingMultiPickupPointEnabled;

  @Value("${cf.count.listing.enabled}")
  private boolean cfCountAtListingEnabled;

  @Value("${pickup.point.name.concat.switch.enabled}")
  private boolean pickupPointNameConcat;

  @Value("${pickup.point.name.delimiter}")
  private String ppNameDelimiter;

  @Value("${bp.bopis.restriction.enabled}")
  private boolean bpBopisRestrictionEnabled;

  @Value("${product.creation.response.validation.switch}")
  private boolean productCreationResponseValidationSwitch;

  @Value("${warna.family.color.validation.switch}")
  private boolean warnaFamilyColorValidationSwitch;

  @Value("${max.allowed.item.skus}")
  private int maxAllowedItemSkus;

  @Value("${product.bundling.enabled}")
  private boolean productBundlingEnabled;

  @Value("${header.Authenticator.fbb}")
  private String headerAuthenticatorFbb;

  @Value("${resize.image.path.removal}")
  private boolean resizeImageRemoval;

  @Value("${resize.image.path.list}")
  private String resizeImagePathList;

  @Value("${bundling.allowed.seller.type}")
  private String productBundlingEligibleMerchantTypes;

  @Value("${override.image.active.flag.l5.listing}")
  private boolean overrideImageActiveFlagForL5Listing;

  @Value("${shared.product.details.for.editable.flag}")
  private boolean sharedProductDetailsForEditableFlag;

  @Value("${get.stock.for.non.distributed.warehouse}")
  private boolean getStockForNonDistributedWareHouse;

  @Value("${validate.business.partner.code.for.security.enabled}")
  private boolean validateBusinessPartnerCodeForSecurityEnabled;

  @Value("${bopis.category.restriction.enabled}")
  private boolean bopisCategoryRestrictionEnabled;

  @Value("${bopis.category.validation.for.merchant.types}")
  private String bopisUnsupportedMerchantTypes;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${validate.product.edit.accessibility}")
  private boolean validateProductEditAccessibility;

  @Value("${copyUrl.baseProductPath}")
  private String baseUrlForPdpWithL5;

  @Value("#{${validate.product.edit.accessibility.exclusion.list}}")
  private Set<String> validateProductEditAccessibilityExclusionList;

  @Value("${replace.undefined.with.product.code.switch}")
  private boolean replaceUndefinedWithProductCode;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${bulk.update.template.column.width}")
  private int bulkUpdateTemplateColumnWidth;

  @Value("${validate.seller.item.basic.details}")
  private boolean validateSellerItemBasicDetails;

  @Value("${item.basic.details.validation.relax.for.seller.types}")
  private String itemBasicDetailsValidationRelaxForSellerTypes;

  @Value("${product.suitability.feature.enabled}")
  private boolean productSuitabilityFeatureEnabled;

  @Value("${validate.product.accessibility}")
  private boolean validateProductAccessibility;

  @Value("${product.accessibility.list}")
  private String productAccessibilityList;

  @Value("${populate.label.for.pwp.promo}")
  private boolean populateLabelForPwpPromo;

  @Value("${youtube.regex}")
  private String youtubeRegex;

  @Value("${image.formats.supported}")
  private List<String> imageFormatsSupported;

  @Value("${pre.order.quota.feature.switch}")
  private boolean preOrderQuotaFeatureSwitch;

  @Override
  public List<EstimateItemPriceWebResponse> getEstimatedPriceByItemCodes(List<String> itemCodes,
      double lowestPriceCoefficient) {
    return itemCodes.stream().map(itemCode -> getEstimatedPrice(itemCode, lowestPriceCoefficient))
        .map(ResponseHelper::toEstimateItemPriceWebResponse).collect(Collectors.toList());
  }

  /**
   * To get estimated price for the ItemCode
   *
   * @param itemCode
   * @param lowestPriceCoefficient
   */
  private EstimateItemPriceResponse getEstimatedPrice(String itemCode, double lowestPriceCoefficient) {
    GdnRestSimpleResponse<EstimateItemPriceResponse> response =
        pbpFeign.getEstimatedPrice(itemCode, lowestPriceCoefficient);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public ProductDetailWebResponse getProductDetailsByProductId(String productId) {
    GdnRestSingleResponse<ProductDetailResponse> response = pcbFeign.getProductDetailsById(productId);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toProductDetailWebResponse(response.getValue());
  }

  @Override
  public Page<ProductItemDetailWebResponse> getProductItemsByKeywordAndCategoryCodes(String keyword,
      List<String> categoryCodes, Integer page, Integer size, boolean isOnlyExternal) {
    GdnRestListResponse<ProductItemDetailResponse> response = null;
    if (isUPCCode(keyword) && CollectionUtils.isNotEmpty(categoryCodes)) {
      UPCCodeSearchRequest upcCodeSearchRequest = new UPCCodeSearchRequest();
      upcCodeSearchRequest.setCategoryCodes(categoryCodes);
      upcCodeSearchRequest.setUpcCode(keyword);
      response = pcbFeign.getProductItemsByUPCCodeAndCategory(page, size, upcCodeSearchRequest, isOnlyExternal);
    } else {
      ProductSearchRequest productSearchRequest = new ProductSearchRequest();
      productSearchRequest.setKeyword(keyword);
      if (CollectionUtils.isNotEmpty(categoryCodes)) {
        productSearchRequest.setCategoryCodes(categoryCodes);
      }
      response = pbpFeign.getProductItemsByNameAndCategoryCodes(page, size, productSearchRequest, isOnlyExternal);
      ResponseHelper.validateResponse(response);
    }
    ResponseHelper.validateResponse(response);
    List<ProductItemDetailWebResponse> productItemDetailWebResponses =
        ResponseHelper.toProductItemDetailWebResponseList(response.getContent());
    return new PageImpl<>(productItemDetailWebResponses, PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<ProductItemDetailWebResponse> getProductItemsByUPCCodeAndCategoryIds(String upcCode,
      List<String> categoryIds, Integer page, Integer size, boolean isOnlyExternal) {
    UPCCodeSearchRequest upcCodeSearchRequest = new UPCCodeSearchRequest();
    upcCodeSearchRequest.setCategoryIds(categoryIds);
    upcCodeSearchRequest.setUpcCode(upcCode);
    GdnRestListResponse<ProductItemDetailResponse> response =
        pcbFeign.getProductItemsByUPCCodeAndCategory(page, size, upcCodeSearchRequest, isOnlyExternal);
    ResponseHelper.validateResponse(response);
    List<ProductItemDetailWebResponse> productItemDetailWebResponses =
        ResponseHelper.toProductItemDetailWebResponseList(response.getContent());
    return new PageImpl<>(productItemDetailWebResponses, PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public CreateProductResponse createProduct(String username, ProductCreationRequest productCreationRequest,
      String businessPartnerCode, String flowType) throws Exception {
    ProfileResponse profileResponse =
        accessibilityAndSellerStatusCheck(businessPartnerCode, productCreationRequest.getProductCode());
    RequestHelper.validateRequest(productCreationRequest, preOrderMaximumDays, preOrderMaximumWeek, false,
        warnaFamilyColorValidationSwitch);
    if (!Constants.FLOW_3.equalsIgnoreCase(flowType)) {
      userPicService.validateUserPicPickupPointsForBusinessPartner(
        getPickupPointsInCreationRequest(productCreationRequest), profileResponse);
    }
    checkBrandStatus(productCreationRequest);
    log.info("Calling create product with request : {} and flowType : {}", productCreationRequest, flowType);
    GdnBaseRestResponse response = pbpFeign.createProduct(productCreationRequest, flowType);
    return getCreateProductResponse(username, productCreationRequest, response, false);
  }

  private Set<String> getPickupPointsInCreationRequest(
    ProductCreationRequest productCreationRequest) {
    return productCreationRequest.getProductItemRequests().stream()
      .flatMap(productItemCreationRequest -> productItemCreationRequest.getPickupPoints().stream())
      .map(PickupPointCreateRequest::getPickupPointId).collect(Collectors.toSet());
  }

  private ProfileResponse accessibilityAndSellerStatusCheck(String businessPartnerCode, String productCode) {
    List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
    if (StringUtils.isNotEmpty(businessPartnerCode) && !accessibility.contains(
        Accessibilty.STORE_PRODUCT_PRODUCT_CREATION)) {
      log.error("Accessibility not present for seller : {} , {}, productCode : {}", businessPartnerCode, accessibility,
          productCode);
      throw new UnauthorizedException(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    ProfileResponse profileResponse = null;
    if (StringUtils.isNotEmpty(businessPartnerCode)) {
      profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
      if (Objects.isNull(profileResponse) || !ACTIVE.equals(profileResponse.getMerchantStatus())) {
        throw new ApiIncorrectInputDataException(
            String.format(ApiErrorCode.INVALID_BUSINESS_PARTNER_CODE.getDesc(), businessPartnerCode),
            ApiErrorCode.INVALID_BUSINESS_PARTNER_CODE.getCode());
      }
    }
    return profileResponse;
  }

  @Override
  public CreateProductResponse createProductV2(String username, ProductCreationRequest productCreationRequest,
      String businessPartnerCode, String flowType) throws Exception {
    ProfileResponse profileResponse = accessibilityAndSellerStatusCheck(businessPartnerCode,
        productCreationRequest.getProductCode());
    RequestHelper.validateRequest(productCreationRequest, preOrderMaximumDays, preOrderMaximumWeek, true,
        warnaFamilyColorValidationSwitch);
    sanitizeAllImagePaths(productCreationRequest);
    userPicService.validateUserPicPickupPointsForBusinessPartner(
      getPickupPointsInCreationRequest(productCreationRequest), profileResponse);
    checkBrandStatus(productCreationRequest);
    log.info("Calling create product with request : {} and flowType : {}", productCreationRequest, flowType);
    GdnBaseRestResponse response = pbpFeign.createNewProduct(productCreationRequest, flowType);
    return getCreateProductResponse(username, productCreationRequest, response, true);
  }

  private void sanitizeAllImagePaths(ProductCreationRequest productCreationRequest) {
    List<Image> commonImages = productCreationRequest.getCommonImages();
    String productCode = productCreationRequest.getProductCode();
    sanitizeImagePaths(commonImages, productCode);
    List<ProductItemCreationRequest> productItemRequests = productCreationRequest.getProductItemRequests();
    for (ProductItemCreationRequest productItemCreationRequest : productItemRequests) {
      sanitizeImagePaths(productItemCreationRequest.getImages(), productCode);
    }
  }

  private void sanitizeImagePaths(List<Image> commonImages, String productCode) {
    if (CollectionUtils.isNotEmpty(commonImages)) {
      for (Image commonImage : commonImages) {
        String sanitizedLocationPath =
            ImageValidator.replaceUndefinedInImageFileWithProductCode(replaceUndefinedWithProductCode,
                commonImage.getLocationPath(), productCode);
        commonImage.setLocationPath(sanitizedLocationPath);
      }
    }
  }

  private CreateProductResponse getCreateProductResponse(String username, ProductCreationRequest productCreationRequest,
      GdnBaseRestResponse response, boolean isMPPFlow) throws Exception {
    if (productCreationResponseValidationSwitch || bpBopisRestrictionEnabled) {
      ResponseHelper.validateProductCreationResponse(response);
    } else {
      ResponseHelper.validateResponse(response);
    }
    boolean isLogisticsSaveSuccess = true;
    if ("ERR-PBP400008".equals(response.getErrorCode())) {
      isLogisticsSaveSuccess = false;
    }
    if (!isMPPFlow && StringUtils.isNotEmpty(productCreationRequest.getBusinessPartnerCode())) {
      pickupPointService.validateAndSaveDefaultPickupPoint(username, productCreationRequest);
    }
    String productCode = categoryService.findInternalActivationIntervalInDaysByCategoryCode(
        productCreationRequest.getProductCategories().get(0).getCategory().getCategoryCode());
    return CreateProductResponse.builder().productCode(productCode)
        .isLogisticsSaveSuccess(isLogisticsSaveSuccess).build();
  }

  private void checkBrandStatus(ProductCreationRequest productCreationRequest) {
    if (StringUtils.equals(productCreationRequest.getBrandApprovalStatus(), BRAND_APPROVAL_STATUS_DRAFT)) {
      GdnRestSingleResponse<BrandWipResponse> brandWipResponse =
          pcbFeign.getBrandDetail(productCreationRequest.getBrandCode());
      ResponseHelper.validateBrandWipResponse(brandWipResponse);
      ResponseHelper.validateResponse(brandWipResponse);
      String brandState = brandWipResponse.getValue().getState();
      productCreationRequest.setBrandApprovalStatus(brandState);
      if (brandState.equals(BRAND_APPROVAL_STATUS_APPROVED)) {
        productCreationRequest.setBrandCode(brandWipResponse.getValue().getBrandCode());
      }
    }
  }

  @Override
  public List<List<SimpleCategoryWebResponse>> getTopCategorySuggestions(String keyword, Pageable pageable,
      String requestId) {
    GdnRestListResponse<CategoryHierarchyProductCountResponse> response =
        getCategoryHierarchyWithProductCount(keyword, pageable);
    return response.getContent().stream()
        .sorted(ProductServiceImpl::compareCategoryHierarchyProductCountResponseByProductCount)
        .limit(pageable.getPageSize()).map(
            categoryHierarchyResponse -> categoryHierarchyResponse.getCategoryHierarchy().stream()
                .filter(CategoryResponse::isActivated).map(ResponseHelper::toSimpleCategoryWebResponse)
                .collect(Collectors.toList())).filter(CollectionUtils::isNotEmpty).collect(Collectors.toList());
  }

  /**
   * @param hierarchyProductCountResponse1
   * @param hierarchyProductCountResponse2
   * @return
   */
  private static int compareCategoryHierarchyProductCountResponseByProductCount(
      CategoryHierarchyProductCountResponse hierarchyProductCountResponse1,
      CategoryHierarchyProductCountResponse hierarchyProductCountResponse2) {
    return Long
        .compare(hierarchyProductCountResponse2.getProductCount(), hierarchyProductCountResponse1.getProductCount());
  }

  @Override
  public ListBaseResponse<CategorySuggestionWebResponse> getCategorySuggestions(String keyword, Pageable pageable,
      String requestId, boolean isOnlyExternal) {
    List<CategorySuggestionWebResponse> categorySuggestionWebResponseList;
    Metadata metadata;
    if (isUPCCode(keyword)) {
      GdnRestListResponse<CategoryHierarchyResponse> response =
          getCategoryHierarchyWithProductCountForUPCCode(keyword, isOnlyExternal);
      categorySuggestionWebResponseList =
          ResponseHelper.toCategorySuggestionWebResponseListBaseResponse(response.getContent());
      metadata =
          new Metadata((int) response.getPageMetaData().getPageNumber(), (int) response.getPageMetaData().getPageSize(),
              (long) categorySuggestionWebResponseList.size());
    } else {
      GdnRestListResponse<CategoryHierarchyProductCountResponse> response =
          getCategoryHierarchyWithProductCount(keyword, pageable);
      categorySuggestionWebResponseList = toCategorySuggestionWebResponseListBaseResponse(response.getContent());
      metadata =
          new Metadata((int) response.getPageMetaData().getPageNumber(), (int) response.getPageMetaData().getPageSize(),
              (long) categorySuggestionWebResponseList.size());
    }
    return new ListBaseResponse<>(null, null, true, requestId, categorySuggestionWebResponseList, metadata);
  }

  private boolean isUPCCode(String keyword) {
    try {
      Long.parseLong(keyword);
      if (keyword.length() == 5 || keyword.length() == 8 || keyword.length() == 12 || keyword.length() == 13) {
        return true;
      }
      return false;
    } catch (NumberFormatException e) {
      return false;
    }
  }

  /**
   * @param categoryHierarchyProductCountResponseList
   * @return
   */
  private List<CategorySuggestionWebResponse> toCategorySuggestionWebResponseListBaseResponse(
      List<CategoryHierarchyProductCountResponse> categoryHierarchyProductCountResponseList) {
    Map<String, CategorySuggestionWebResponse> categoryIdCategorySuggestionWebResponseMap = new HashMap<>();
    Map<String, Set<String>> categoryIdChildrenCategoryIdMap = new HashMap<>();
    Set<String> topCategoryIds = new HashSet<>();
    buildMapResponsesAndChildCategoryMapAndTopCategories(categoryHierarchyProductCountResponseList,
        categoryIdCategorySuggestionWebResponseMap, categoryIdChildrenCategoryIdMap, topCategoryIds);
    return topCategoryIds.stream().map(topCategoryId -> ResponseHelper
        .buildCategorySuggestionTree(categoryIdCategorySuggestionWebResponseMap, categoryIdChildrenCategoryIdMap,
            topCategoryId)).sorted(ResponseHelper::compareCategorySuggestionWebResponseByProductCount)
        .collect(Collectors.toList());
  }

  /**
   * Build data from response
   *
   * @param categoryHierarchyProductCountResponseList
   * @param categoryIdCategorySuggestionWebResponseMap
   * @param categoryIdChildrenCategoryIdMap
   * @param topCategoryIds
   */
  private void buildMapResponsesAndChildCategoryMapAndTopCategories(
      List<CategoryHierarchyProductCountResponse> categoryHierarchyProductCountResponseList,
      Map<String, CategorySuggestionWebResponse> categoryIdCategorySuggestionWebResponseMap,
      Map<String, Set<String>> categoryIdChildrenCategoryIdMap, Set<String> topCategoryIds) {
    for (CategoryHierarchyProductCountResponse categoryHierarchyProductCountResponse : categoryHierarchyProductCountResponseList) {
      for (CategoryResponse categoryResponse : categoryHierarchyProductCountResponse.getCategoryHierarchy()) {
        if (categoryResponse.isActivated()) {
          CategorySuggestionWebResponse categorySuggestionWebResponse =
              categoryIdCategorySuggestionWebResponseMap.get(categoryResponse.getId());
          if (Objects.isNull(categorySuggestionWebResponse)) {
            categorySuggestionWebResponse = ResponseHelper.toCategorySuggestionWebResponse(categoryResponse, 0L);
            categoryIdCategorySuggestionWebResponseMap.put(categoryResponse.getId(), categorySuggestionWebResponse);
          }
          categorySuggestionWebResponse.setProductCount(
              categorySuggestionWebResponse.getProductCount() + categoryHierarchyProductCountResponse
                  .getProductCount());
          if (Objects.isNull(categoryResponse.getParentCategoryId())) {
            topCategoryIds.add(categoryResponse.getId());
          } else {
            if (Objects.isNull(categoryIdChildrenCategoryIdMap.get(categoryResponse.getParentCategoryId()))) {
              categoryIdChildrenCategoryIdMap.put(categoryResponse.getParentCategoryId(), new HashSet<>());
            }
            categoryIdChildrenCategoryIdMap.get(categoryResponse.getParentCategoryId()).add(categoryResponse.getId());
          }
        }
      }
    }
  }

  /**
   * Get CategoryHierarchyWithProductCount from PBP
   *
   * @param keyword
   * @param pageable
   * @return
   */
  private GdnRestListResponse<CategoryHierarchyProductCountResponse> getCategoryHierarchyWithProductCount(
      String keyword, Pageable pageable) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnRestListResponse<CategoryHierarchyProductCountResponse> response =
      pbpFeign.getCategoryHierarchyByKeywordWithProductCount(pageable.getPageNumber(),
        pageable.getPageSize(), keyword, businessPartnerCode);
    ResponseHelper.validateResponse(response);
    return response;
  }

  /**
   * Get CategoryHierarchyWithProductCount from PCB
   *
   * @param upcCode
   * @param isOnlyExternal
   * @return
   */
  private GdnRestListResponse<CategoryHierarchyResponse> getCategoryHierarchyWithProductCountForUPCCode(String upcCode,
      boolean isOnlyExternal) {
    GdnRestListResponse<CategoryHierarchyResponse> response =
        pcbFeign.getCategoryHierarchyByUPCCodeWithProductCount(upcCode, isOnlyExternal);
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public List<ProductItemDetailWebResponse> getProductItemSuggestionsByItemNameAndCategoryId(String itemName,
      String categoryId, Integer page, Integer size) {
    GdnRestListResponse<ProductItemDetailResponse> response =
        pcbFeign.getProductItemSuggestionsByItemNameAndCategoryId(itemName, categoryId, page, size);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toProductItemDetailWebResponseList(response.getContent());
  }

  @Override
  public int getProductsCount(boolean viewable) {
    String productsCount = productRedisTemplate.boundValueOps(VIEWABLE_PRODUCT_COUNT).get();
    int response;
    if (StringUtils.isBlank(productsCount)) {
      log.debug("Redis Entry Not Found: API to get productsCount for viewable criteria : {}", viewable);
      GdnRestSimpleResponse<Integer> productsCountByViewable = pbpFeign.getProductsCountByViewable(viewable);
      ResponseHelper.validateResponse(productsCountByViewable);
      response = productsCountByViewable.getValue();
      productRedisTemplate.boundValueOps(VIEWABLE_PRODUCT_COUNT)
          .set(String.valueOf(response), REDIS_PRODUCT_COUNT_TIMEOUT, TimeUnit.HOURS);
    } else {
      response = Integer.valueOf(productsCount);
      log.debug("data found in redis for viewable product count, count is : {} ", response);
    }
    return response;
  }

  @Override
  public Page<ActiveProductWebResponse> getProductListByMerchantAndCategoryCode(
      ActiveProductWebRequest activeProductWebRequest, String merchantCode) {
    List<String> categoryCodes = new ArrayList<>();
    if (StringUtils.isNotBlank(activeProductWebRequest.getCategoryCode())) {
      CategoryCodeRequest request = new CategoryCodeRequest();
      request.setCategoryCodes(Collections.singletonList(activeProductWebRequest.getCategoryCode()));
      GdnRestSingleResponse<CategoryCodeResponse> response = pcbFeign.getAllChildCategoryCodesByC1CategoryCode(request);
      ResponseHelper.validateResponse(response);
      if (CollectionUtils.isEmpty(response.getValue().getCategoryCodes())) {
        categoryCodes.add(activeProductWebRequest.getCategoryCode());
      } else {
        categoryCodes.addAll(response.getValue().getCategoryCodes());
      }
    }
    ActiveProductRequest activeProductRequest =
        RequestHelper.toActiveProductRequest(activeProductWebRequest, categoryCodes);
    activeProductRequest.setMerchantCode(merchantCode);
    GdnRestListResponse<ActiveProductResponse> gdnRestListResponse = xProductFeign.
        getActiveProductListByMerchantAndCategoryCode(activeProductWebRequest.getPage(),
            activeProductWebRequest.getSize(), activeProductRequest);
    ResponseHelper.validateResponse(gdnRestListResponse);
    return new PageImpl<>(ResponseHelper.toActiveProductWebResponseList(gdnRestListResponse.getContent()),
        PageRequest.of(activeProductWebRequest.getPage(), activeProductWebRequest.getSize()),
        gdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  public Page<ProductL3ListingWebResponse> getProductListByMerchantAndCategoryCodeV2(
      ProductL3ListingWebRequest webRequest, String merchantCode, int page, int size) throws Exception {
    ActiveProductRequest activeProductRequest =
        RequestHelper.toActiveProductRequest(webRequest);
    activeProductRequest.setMerchantCode(merchantCode);
    GdnRestListResponse<ActiveProductResponse> gdnRestListResponse =
        xProductFeign.getActiveProductListByMerchantAndCategoryCode(
            page, size, activeProductRequest);
    ResponseHelper.validateResponse(gdnRestListResponse);
    List<ActiveProductResponse> productResponses = gdnRestListResponse.getContent();
    List<String> categoryCodeList =
        productResponses.stream().map(ActiveProductResponse::getMasterCatalog)
            .collect(Collectors.toList());
    List<CategoryWebResponse> categoryWebResponseList =
        categoryService.getCategoriesByCategoryCodes(categoryCodeList);
    Map<String, CategoryWebResponse> categoryWebResponseMap = categoryWebResponseList.stream()
        .collect(Collectors.toMap(CategoryWebResponse::getCategoryCode, Function.identity(),
            (categoryResponse1,categoryResponse2) -> categoryResponse1));
    List<ProductL3ListingWebResponse> response = new ArrayList<>();
    for (ActiveProductResponse activeProductResponse : productResponses) {
      response.add(ResponseHelper.toActiveProductResponse(activeProductResponse, categoryWebResponseMap, productDetailPageUrlPrefix));
    }
    return new PageImpl<>(response,
        PageRequest.of(page, size), gdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<ReelProductDetailWebResponse> getProductListForReels(
      ReelProductListingWebRequest reelProductListingWebRequest, int page, int size) {
    GdnRestListResponse<ReelProductDetailWebResponse> gdnRestListResponse =
        xProductFeign.getReelProductList(page, size, reelProductListingWebRequest);
    ResponseHelper.validateResponse(gdnRestListResponse);
    if (Boolean.TRUE.equals(reelProductListingWebRequest.getTradingProduct())) {
      List<String> productSkus =
          gdnRestListResponse.getContent().stream().map(ReelProductDetailWebResponse::getProductSku)
              .distinct().collect(Collectors.toList());
      List<String> sellerCodes = getDistinctSellerCodesFromProductSkuList(productSkus);
      if (CollectionUtils.isNotEmpty(sellerCodes)) {
        BusinessPartnerFilterRequest businessPartnerFilterRequest =
            new BusinessPartnerFilterRequest();
        businessPartnerFilterRequest.setBusinessPartnerCodes(new HashSet<>(sellerCodes));
        SimpleMapStringResponse profileResponsesMap =
            businessPartnerService.getBusinessPartnerDetailsByList(Constants.ZERO,
                sellerCodes.size(), businessPartnerFilterRequest);
        gdnRestListResponse.getContent().forEach(reelProductDetailWebResponse -> {
          String sellerCode = StringUtils.substring(reelProductDetailWebResponse.getProductSku(), 0,
              reelProductDetailWebResponse.getProductSku().lastIndexOf(Constants.HYPHEN_VALUE));
          reelProductDetailWebResponse.setStoreName(
              profileResponsesMap.getValue().getOrDefault(sellerCode, Constants.HYPHEN_VALUE));

        });
      }
    }
    return new PageImpl<>(gdnRestListResponse.getContent(), PageRequest.of(page, size),
        gdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  private List<String> getDistinctSellerCodesFromProductSkuList(List<String> productSkus) {
    return productSkus.stream().map(productSku -> StringUtils.substring(productSku, Constants.ZERO,
        productSku.lastIndexOf(Constants.HYPHEN_VALUE))).distinct().collect(Collectors.toList());
  }

  @Override
  public Page<SuspensionWebResponse> getSuspendedItemListByMerchantAndCategoryCode(
      SuspensionWebRequest suspensionWebRequest, Pageable pageable) {
    GdnRestListResponse<SuspensionItemResponse> gdnRestListResponse = pbpFeign.
        getSuspendedItem(pageable.getPageNumber(), pageable.getPageSize(),
            RequestHelper.toSummaryFilterRequest(suspensionWebRequest));
    ResponseHelper.validateResponse(gdnRestListResponse);
    return new PageImpl<>(ResponseHelper.toSuspensionWebResponseList(gdnRestListResponse.getContent()),
        PageRequest.of(pageable.getPageNumber(), pageable.getPageSize()),
        gdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  @Override
  public ProductCountWebResponse getProductCounts(String type, String merchantCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(merchantCode), ErrorMessages.ERR_MER_CODE_NULL);
    log.info("Get the product counts for the type: {} and merchantCode : {}", type, merchantCode);
    if (ACTIVE_STATUS.equalsIgnoreCase(type)) {
      GdnRestSingleResponse<ProductLevel3SummaryCountResponse> response =
          pbpFeign.getActiveProductStockCount(merchantCode);
      ResponseHelper.validateResponse(response);
      return ResponseHelper.toProductCountWebResponse(response.getValue());
    } else if (INPROCESS_STATUS.equalsIgnoreCase(type)) {
      GdnRestSingleResponse<CountProductLevel3WipResponse> response = pbpFeign.getInProgressProductCount(merchantCode);
      ResponseHelper.validateResponse(response);
      return ResponseHelper.toProductCountWebResponse(response.getValue());
    } else if (INACTIVE_STATUS.equalsIgnoreCase(type)) {
      GdnRestSingleResponse<CountProductLevel3InactiveResponse> response =
          pbpFeign.getInActiveProductCount(merchantCode);
      ResponseHelper.validateResponse(response);
      return ResponseHelper.toProductCountWebResponse(response.getValue());
    } else {
      log.error("Not a valid parameter type : {}", type);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_REQUEST);
    }
  }


  @Override
  public Page<ProductLevel3SummaryResponse> findSummaryByFilter(InActiveProductWebRequest inActiveRequest,
      PageRequest pageRequest, SortOrder sort, String type) {
    if (InActiveProductStatus.ARCHIVED.toString().equalsIgnoreCase(type)) {
      log.info("Fetching the archived products for business partner code : {}",
          inActiveRequest.getBusinessPartnerCode());
      return getProductLevel3SummaryResponsesArchived(inActiveRequest, pageRequest, sort);
    } else if (InActiveProductStatus.SUSPENDED.toString().equalsIgnoreCase(type)) {
      log.info("Fetching the suspended products for business partner code : {}",
          inActiveRequest.getBusinessPartnerCode());
      return getProductLevel3SummaryResponsesSuspended(inActiveRequest, pageRequest);
    } else if (InActiveProductStatus.REJECTED.toString().equalsIgnoreCase(type)) {
      log.info("Fetching the rejected products for business partner code : {}",
          inActiveRequest.getBusinessPartnerCode());
      return getProductLevel3SummaryResponsesRejected(inActiveRequest, pageRequest);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.REQUIRED_PARAMETER, ErrorMessages.INVALID_TYPE_ERR);
    }
  }

  @Override
  public Page<ProductLevel3SummaryWebResponse> getActiveProductList(ActiveProductWebRequest request,
      PageRequest pageRequest) {
    log.info("Fetching the active products for request : {}", request);
    GdnRestListResponse<ProductLevel3SummaryResponse> gdnRestListResponse = pbpFeign
        .filterSummary(request.getBusinessPartnerCode(), pageRequest.getPageNumber(), pageRequest.getPageSize(),
            ORDERBY, SORTBY, RequestHelper.toProductLevel3SummaryRequest(request));
    ResponseHelper.validateResponse(gdnRestListResponse);

    List<ProductLevel3SummaryWebResponse> productLevel3SummaryWebResponseList =
        gdnRestListResponse.getContent().stream().map(ResponseHelper::toProductLevel3SummaryWebResponse)
            .collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(productLevel3SummaryWebResponseList)) {
      GdnRestSingleResponse<ProductCampaignAvailabilityResponse>
          campaignAvailabilityResponse =
          this.getCampaignAvailabilityResponse(productLevel3SummaryWebResponseList, request.getBusinessPartnerCode());
      ResponseHelper.validateResponse(campaignAvailabilityResponse);
      this.setProductCampaignAvailabilityFlagForListing(productLevel3SummaryWebResponseList,
          campaignAvailabilityResponse.getValue());
    }
    return new PageImpl<>(productLevel3SummaryWebResponseList, pageRequest,
        gdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  private GdnRestSingleResponse<ProductCampaignAvailabilityResponse> getCampaignAvailabilityResponse(
      List<ProductLevel3SummaryWebResponse> productLevel3SummaryWebResponseList, String merchantCode) {
    Set<String> itemSkus = Optional.ofNullable(productLevel3SummaryWebResponseList).orElse(new ArrayList<>()).stream()
        .map(ProductLevel3SummaryWebResponse::getItemSku).collect(Collectors.toSet());
    Set<com.gdn.x.campaign.dto.ItemInfoDto> itemInfoDtos = productLevel3SummaryWebResponseList.stream()
        .map(productLevel3SummaryWebResponse -> new com.gdn.x.campaign.dto.ItemInfoDto(
            productLevel3SummaryWebResponse.getItemSku(), productLevel3SummaryWebResponse.getPickupPointCode(), StringUtils.EMPTY))
        .collect(Collectors.toSet());
    ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest =
        ProductCampaignAvailabilityRequest.builder().itemSkus(itemSkus).itemInfo(itemInfoDtos).merchantCode(merchantCode).build();
    GdnRestSingleResponse<ProductCampaignAvailabilityResponse> campaignAvailability =
        getProductCampaignAvailabilityResponse(productCampaignAvailabilityRequest);
    return campaignAvailability;
  }

  private GdnRestSingleResponse<ProductCampaignAvailabilityResponse> getProductCampaignAvailabilityResponse(
      ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest) {
    GdnRestSingleResponse<ProductCampaignAvailabilityResponse> campaignAvailability;
    if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
      campaignAvailability = this.xCampaignFeign.getCampaignAvailabilityV2(productCampaignAvailabilityRequest);
    ResponseHelper.validateResponse(campaignAvailability);
    ResponseHelper.modifyProductCampaignAvailabilityResponse(campaignAvailability.getValue());
    } else {
      campaignAvailability = this.xCampaignFeign.getCampaignAvailability(productCampaignAvailabilityRequest);
      ResponseHelper.validateResponse(campaignAvailability);
    }
    return campaignAvailability;
  }

  private void setProductCampaignAvailabilityFlagForListing(List<ProductLevel3SummaryWebResponse> response,
      ProductCampaignAvailabilityResponse productCampaignAvailabilityResponse) {
    for (ProductLevel3SummaryWebResponse productLevel3SummaryWebResponse : response) {
      productLevel3SummaryWebResponse.setItemCampaignMapped(
          productCampaignAvailabilityResponse.getProductCampaignAvailabilityMap()
              .get(productLevel3SummaryWebResponse.getItemSku()));
    }
  }

  @Override
  public Page<ItemDetailWebResponse> getActiveProductNameList(String searchKey, String merchantCode,
      boolean isProductName, PageRequest pageRequest, boolean inStock) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(merchantCode), ErrorMessages.ERR_MER_CODE_NULL);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(searchKey), ErrorMessages.ERR_SEARCH_KEY_NULL);
    if (isProductName) {
      ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
      productSummaryRequest.setArchived(false);
      productSummaryRequest.setKeyword(searchKey);
      productSummaryRequest.setMerchantCode(merchantCode);
      productSummaryRequest.setInStock(inStock);
      Set<String> accessiblePickupPointCodes = userPicService.fetchAccessiblePickupPointCodes(null);
      if (CollectionUtils.isNotEmpty(accessiblePickupPointCodes)) {
        productSummaryRequest.setPickupPointCodes(new ArrayList<>(accessiblePickupPointCodes));
      }
      log.info("Fetching the active product name list for request : {}", productSummaryRequest);
      GdnRestListResponse<ProductNameSuggestionResponse> gdnRestListResponse = xProductFeign
          .getProductNamesByFilter(pageRequest.getPageNumber(), pageRequest.getPageSize(), productSummaryRequest);
      ResponseHelper.validateResponse(gdnRestListResponse);
      return new PageImpl<>(ResponseHelper.toProductNameWebResponse(gdnRestListResponse.getContent()), pageRequest,
          gdnRestListResponse.getPageMetaData().getTotalRecords());
    } else {
      ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
      itemSummaryRequest.setArchived(false);
      itemSummaryRequest.setSearchKey(searchKey);
      itemSummaryRequest.setMerchantCode(merchantCode);
      log.info("Fetching the active product name list for request : {}", itemSummaryRequest);
      GdnRestListResponse<ItemSummaryResponse> gdnRestListResponse = xProductFeign
          .getActiveProductNamesByMerchantCode(pageRequest.getPageNumber(), pageRequest.getPageSize(),
              itemSummaryRequest);
      ResponseHelper.validateResponse(gdnRestListResponse);
      return new PageImpl<>(ResponseHelper.toItemDetailWebResponse(gdnRestListResponse.getContent()), pageRequest,
          gdnRestListResponse.getPageMetaData().getTotalRecords());
    }
  }

  @Override
  public Page<InProcessWebResponse> getInprocessProductList(InProcessProductWebRequest request,
      PageRequest pageRequest) {
    log.info("Fetching the in-process products for request : {}", request);
    RequestHelper.validateAccessibilityForProductTab(validateProductAccessibility,
        Arrays.asList(Credential.getAccessibilities()), Boolean.parseBoolean(mandatoryParameterHelper.isExternal()),
        productAccessibilityList, mandatoryParameterHelper.getClientType());
    GdnRestListResponse<ProductLevel3WipResponse> gdnRestListResponse = pbpFeign
        .filterSummaryWithState(pageRequest.getPageNumber(), pageRequest.getPageSize(),
            RequestHelper.toProductLevel3WipSummaryRequest(request));
    ResponseHelper.validateResponse(gdnRestListResponse);
    return new PageImpl<>(ResponseHelper.toInProcessWebResponse(gdnRestListResponse.getContent()), pageRequest,
        gdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  public void toggleArchiveItems(List<String> itemSkus, boolean doArchive, String businessPartnerCode) {
    List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
    if (!accessibility.contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_ARCHIVE)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    for (String itemSku : itemSkus) {
      if (StringUtils.isNotBlank(itemSku)) {
        GdnPreconditions.checkArgument(itemSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
        log.info("Performing archive = {} for itemSku {}", doArchive, itemSku);
        GdnBaseRestResponse response = pbpFeign.toggleArchiveItem(DEFAULT_CLIENT_HOST, itemSku, doArchive);
        ResponseHelper.validateResponse(response);
      }
    }
  }

  @Override
  public Page<LogAuditTrailUpdatedProductResponse> getProductUpdateLogs(String gdnSku, Pageable pageable) {
    log.info("fetching the product update logs for gdnSku :  {}", gdnSku);
    GdnRestListResponse<LogAuditTrailUpdatedProductResponse> response =
        pbpFeign.getProductUpdateLogs(pageable.getPageNumber(), pageable.getPageSize(), gdnSku);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(response.getContent(), pageable, response.getPageMetaData().getTotalRecords());
  }

  @Override
  public ProductLevel3SummaryResponse updateSummary(String gdnSku, String businessPartnerCode,
      ProductLevel3UpdateSummaryRequest request) {
    List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
    if (!accessibility.contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_LISTING_ALLOW_UPDATE)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions
        .checkArgument(Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
            ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    RequestHelper.validateRequest(request);
    log.info("Updating the item sku : {} for business partner code : {} and request : {}", gdnSku, businessPartnerCode,
        request);
    GdnRestSingleResponse<ProductLevel3OrderResponse> response =
        this.pbpFeign.filterDetailOrderByGdnSku(businessPartnerCode, gdnSku);
    ResponseHelper.validateResponse(response);
    ProductLevel3OrderResponse productLevel3OrderResponse = response.getValue();
    RequestHelper.setProductOff2OnFlagNullAndRoundOffSalePrice(productLevel3OrderResponse, request);
    request.setAccessChannel(UpdateProductAccessChannel.MTA_WEB_UPDATE_LIST.getDesc());
    updateSummarySkuAccessibilityCheck(productLevel3OrderResponse, request);
    GdnRestSingleResponse<ProductLevel3SummaryResponse> productLevel3SummaryResponseGdnRestSingleResponse =
        this.pbpFeign.updateSummary(businessPartnerCode, gdnSku, DEFAULT_CLIENT_HOST, request);
    ResponseHelper.validateUpdateSummaryResponse(productLevel3SummaryResponseGdnRestSingleResponse, gdnSku);
    ProductLevel3SummaryResponse productLevel3SummaryResponse =
        productLevel3SummaryResponseGdnRestSingleResponse.getValue();
    if (Objects.isNull(productLevel3SummaryResponse.getMerchantSku())) {
      productLevel3SummaryResponse.setMerchantSku(StringUtils.EMPTY);
    }
    return productLevel3SummaryResponse;
  }

  public void updateSummarySkuAccessibilityCheck(ProductLevel3OrderResponse productLevel3OrderResponse,
      ProductLevel3UpdateSummaryRequest request) {
    List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
    String existingMerchantSku = productLevel3OrderResponse.getItems().get(0).getMerchantSku();
    String newMerchantSku = request.getMerchantSku();
    if (!StringUtils.equalsIgnoreCase(existingMerchantSku, newMerchantSku) && (!accessibility
        .contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_CHANGE_SELLER_SKU_CHANGE_DATA))) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  @Override
  public PostLiveProductCountResponse bulkDeleteProductWip(String businessPartnerCode,
      BulkDeleteProductWipRequest request) {
    log.info("Deleting the in-progress products : {} for business partner code : {} ", businessPartnerCode, request);
    GdnRestSingleResponse<PostLiveProductCountResponse> response =
        pbpFeign.bulkDeleteProductWip(businessPartnerCode, request);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public Map<String, ProductLevel3SummaryResponse> updateBulkSummary(String businessPartnerCode,
      Map<String, ProductLevel3UpdateSummaryRequest> request) {
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    if (!accessibilties.contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    for (Map.Entry<String, ProductLevel3UpdateSummaryRequest> entry : request.entrySet()) {
      String gdnSku = entry.getKey();
      ProductLevel3UpdateSummaryRequest requestItem = entry.getValue();
      requestItem.getPrices().removeAll(Collections.singleton(null));
      Optional.of(requestItem.getPrices().stream().findFirst()).orElseThrow(
          () -> new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
              "Product prices is not found for " + gdnSku));
      GdnPreconditions
          .checkArgument(requestItem.getPrices().get(0).getPrice() >= requestItem.getPrices().get(0).getSalePrice(),
              gdnSku + ": " + ErrorMessages.SELLING_PRICE_MUST_BE_LOWER_THAN_REGULAR_PRICE);
    }

    Map<String, ProductLevel3SummaryResponse> productLevel3Summaries = new HashMap<>();
    for (Map.Entry<String, ProductLevel3UpdateSummaryRequest> entry : request.entrySet()) {
      String gdnSku = entry.getKey();
      ProductLevel3SummaryResponse productLevel3SummaryResponse =
          this.updateSummary(gdnSku, businessPartnerCode, entry.getValue());
      productLevel3Summaries.put(gdnSku, productLevel3SummaryResponse);
    }
    return productLevel3Summaries;
  }

  @Override
  public void retryCreate(List<String> productBusinessPartnerIds) {
    boolean result = true;
    for (String productBusinessPartnerId : productBusinessPartnerIds) {
      log.info("Retrying to activate the product with product business partner id  : {} ", productBusinessPartnerId);
      GdnBaseRestResponse response = pbpFeign.retryCreate(productBusinessPartnerId);
      if(Objects.isNull(response) || !response.isSuccess()){
        String errorMessage = StringUtils.EMPTY;
        if(Objects.nonNull(response)){
          errorMessage = response.getErrorMessage();
        }
        log.error("Failed to activate the product with product business partner id : {} ",
            productBusinessPartnerId, errorMessage);
        result = false;
      }
    }
    if(!result) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
  }

  @Override
  public void downloadTemplateBulkUpdate(String username, String businessPartnerCode, boolean isOnlyExternalUser,
      BulkRequest bulkRequest, HttpServletResponse httpServletResponse) throws Exception {
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions
        .checkArgument(Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
            ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    List<PickupPointOutboundResponse> pickupPointCodeResponseList =
        businessPartnerService.getAllPickupPointsForBusinessPartner(businessPartnerCode);
    ResponseHelper.addPickupPointDetailsInProfileResponse(profileResponse, pickupPointCodeResponseList);
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList =
        filterSummaryByGdnSku(businessPartnerCode, bulkRequest.getGdnSkus());
    Map<String, Boolean> privilege = RequestHelper.getEditAccessibilities();
    Workbook workbook =
        ExcelTemplateUtil.generateWorkbookTemplateBulkUpdate(profileResponse, isOnlyExternalUser, privilege,
            productLevel3SummaryResponseList, multiPickupPointEnabled, cncForWarehouseFeatureSwitch,
            bulkUpdateTemplateColumnWidth, preOrderQuotaFeatureSwitch);
    ExcelTemplateUtil.generateFileTemplate(FILE_BULK_UPDATE_PRODUCT_TEMPLATE, workbook, httpServletResponse);
  }

  @Override
  public void downloadTemplateBulkUpdateByProductSku(String businessPartnerCode,
      boolean isOnlyExternalUser, List<String> productSkuList,
      HttpServletResponse httpServletResponse, int page, int size) throws Exception {
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions
        .checkArgument(Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
            ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    List<PickupPointOutboundResponse> pickupPointCodeResponseList;
    Set<String> userAccessiblePickupPointCodes = new HashSet<>();
    if (multiPickupPointEnabled) {
      userAccessiblePickupPointCodes = userPicService.fetchAccessiblePickupPointCodes(profileResponse);
      pickupPointCodeResponseList = businessPartnerService.getPickupPointsForBusinessPartner(businessPartnerCode,
        userAccessiblePickupPointCodes);
    } else {
      pickupPointCodeResponseList =
        businessPartnerService.getAllPickupPointsForBusinessPartner(businessPartnerCode);
    }
    ResponseHelper.addPickupPointDetailsInProfileResponse(profileResponse, pickupPointCodeResponseList);
    List<ProductLevel3SummaryResponse> productDataForExcel;
    if (multiPickupPointEnabled) {
      productDataForExcel = generateProductDataFromL5(productSkuList, page,
        size, businessPartnerCode, userAccessiblePickupPointCodes);
    } else {
      productDataForExcel = generateProductDataFromL4(productSkuList, businessPartnerCode, page,
        size);
    }
    Map<String, Boolean> privilege = RequestHelper.getEditAccessibilities();
    Workbook workbook =
        ExcelTemplateUtil.generateWorkbookTemplateBulkUpdate(profileResponse, isOnlyExternalUser, privilege,
            productDataForExcel, multiPickupPointEnabled, cncForWarehouseFeatureSwitch, bulkUpdateTemplateColumnWidth,
            preOrderQuotaFeatureSwitch);
    ExcelTemplateUtil
        .generateFileTemplate(FILE_BULK_UPDATE_PRODUCT_TEMPLATE, workbook, httpServletResponse);
  }

  @Override
  public void downloadTemplateForMultiPickupPointTemplate(String businessPartnerCode,
      HttpServletResponse httpServletResponse) throws Exception {
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions.checkArgument(
        Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
        ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    Set<String> accessiblePickupPointCodes;
    if (userPicService.shouldRestrictAccess(profileResponse)) {
      accessiblePickupPointCodes = mandatoryParameterHelper.getPickupPoints();
    } else {
      accessiblePickupPointCodes = Collections.emptySet();
    }
    List<PickupPointOutboundResponse> pickupPointCodeResponseList =
      businessPartnerService.getPickupPointsForBusinessPartner(businessPartnerCode,
        accessiblePickupPointCodes);
    ResponseHelper.addPickupPointDetailsInProfileResponse(profileResponse, pickupPointCodeResponseList);
    Workbook workbook = ExcelTemplateUtil.generateWorkbookTemplateMultiPickuppoint(profileResponse,
        cncForWarehouseFeatureSwitch);
    if (profileResponse.getCompany().isInternationalFlag()) {
      ExcelTemplateUtil.generateFileTemplate(multiPickupPointUploadTemplateFileEnglish, workbook, httpServletResponse);
    } else {
      ExcelTemplateUtil.generateFileTemplate(multiPickupPointUploadTemplateFile, workbook, httpServletResponse);
    }
  }

  private List<ProductLevel3SummaryResponse> generateProductDataFromL5(List<String> productSkuList,
    int page, int size, String businessPartnerCode, Set<String> pickupPoints) {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = new ArrayList<>();
    GdnRestListResponse<ProductLevel3SummaryResponse> productLevel3SummaryList = null;
    do {
      productLevel3SummaryList =
        this.pbpFeign.getL5SummaryByProductSkuList(page, size, businessPartnerCode,
          ProductSkuAndPickupPointCodeRequest.builder().value(productSkuList).pickupPointCodes(
            Optional.ofNullable(pickupPoints).map(ArrayList::new).orElse(null)).build());
      ResponseHelper.validateResponse(productLevel3SummaryList);
      productLevel3SummaryResponseList
        .addAll(productLevel3SummaryList.getContent());
      page++;
    } while (page < Math.ceil((double) productLevel3SummaryList.getPageMetaData().getTotalRecords() / size));
    return productLevel3SummaryResponseList;
  }

  private List<ProductLevel3SummaryResponse> generateProductDataFromL4(List<String> productSkuList,
    String businessPartnerCode, int page, int size) {
    ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    productLevel3SummaryRequest.setProductSkuList(productSkuList);
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = new ArrayList<>();
    GdnRestListResponse<ProductLevel3SummaryResponse> productLevel3SummaryList = null;
    do {
      productLevel3SummaryList = this.pbpFeign
        .filterSummary(businessPartnerCode, page, size, StringUtils.EMPTY, StringUtils.EMPTY,
          productLevel3SummaryRequest);
      ResponseHelper.validateResponse(productLevel3SummaryList);
      productLevel3SummaryResponseList
        .addAll(productLevel3SummaryList.getContent());
      page++;
    } while (page < Math.ceil((double) productLevel3SummaryList.getPageMetaData().getTotalRecords() / size));
    return productLevel3SummaryResponseList;
  }

  @Override
  public void downloadAllProduct(String username, boolean isOnlyExternalUser, String businessPartnerCode,
      ProductSummaryWebRequest request) throws Exception {
    if (validateBusinessPartnerCodeForSecurityEnabled && !StringUtils.equals(businessPartnerCode,
        request.getMerchantCode())) {
      throw new ValidationException(ErrorMessages.INVALID_GDN_SKU);
    }
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions
        .checkArgument(Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
            ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    Map<String, Boolean> privilegeMap = RequestHelper.getReadAccessibilities(profileResponse, isOnlyExternalUser);
    privilegeMap.put(IS_ONLY_EXTERNAL_USER, isOnlyExternalUser);
    String randomUUID = UUID.randomUUID().toString();
    String emailTo = profileResponse.getCompany().getEmail();
    String emailCc = username;
    Set<String> pickupPointCodes = Objects.isNull(request.getPickupPointCodes()) ?
        new HashSet<>() : new HashSet<>(request.getPickupPointCodes());
    String fileName =
        new StringBuilder().append(randomUUID).append(".").append(FileType.XLSX.name().toLowerCase()).toString();
    request.setPickupPointCodes(new ArrayList<>(userPicService.filterInaccessiblePickupPoints(profileResponse, pickupPointCodes)));
    ProductSummaryRequest productSummaryRequest = RequestHelper.toProductSummaryRequest(request);
    ProductDownloadRequest.ProductBuilder builder = new ProductDownloadRequest.ProductBuilder();
    builder.privilegedMap(privilegeMap).productSummaryRequest(productSummaryRequest)
        .accessiblePickupPoints(userPicService.fetchAccessiblePickupPointCodes(profileResponse))
        .language(LANGUAGE).request(randomUUID)
        .downloadType(DownloadType.ALL).fileType(FileType.XLSX)
        .bulkProcessType(BulkProcessEntity.PRODUCT).emailCC(emailCc).emailTo(emailTo)
        .filename(fileName).merchant(businessPartnerCode).username(username).directDownload(true);
    BulkDownloadRequest downloadRequest = builder.build();
    this.kafkaProducer.send(kafkaTopicProperties.getBulkDownloadAllEvent(), downloadRequest);
  }

  @Override
  public void downloadAllProductWithEAN(String username, boolean isOnlyExternalUser, String businessPartnerCode,
      ProductSummaryWebRequest request) {
    if (validateBusinessPartnerCodeForSecurityEnabled && !StringUtils.equals(businessPartnerCode,
        request.getMerchantCode())) {
      throw new ValidationException(ErrorMessages.INVALID_GDN_SKU);
    }
    var profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions.checkArgument(
        Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
        ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    String randomUUID = UUID.randomUUID().toString();
    String fileName = randomUUID + "." + FileType.XLSX.name().toLowerCase();
    var productSummaryRequest = RequestHelper.toProductSummaryRequest(request);
    var builder = new ProductDownloadEANRequest.ProductBuilder();
    builder.productSummaryRequest(productSummaryRequest).language(LANGUAGE)
        .request(randomUUID).downloadType(DownloadType.ALL).fileType(FileType.XLSX)
        .bulkProcessType(BulkProcessEntity.PRODUCT_EAN).emailCC(username)
        .emailTo(profileResponse.getCompany().getEmail()).filename(fileName).merchant(businessPartnerCode)
        .username(username).directDownload(true);
    BulkDownloadRequest downloadRequest = builder.build();
    this.kafkaProducer.send(kafkaTopicProperties.getBulkDownloadAllEvent(), downloadRequest);
  }

  @Override
  public ProductLevel3StockInfoWebSiteResponse getStockInfoWebSite(String merchantCode, String itemSku) {
    log.info("Fetching the stock info website for product business partner id  : {} and item sku : {}", merchantCode,
        itemSku);
    GdnRestSingleResponse<ProductLevel3StockInfoWebSiteResponse> response =
        pbpFeign.getStockInfoWebSite(merchantCode, itemSku);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public Double getCogsValue(String materialCode, String businessPartnerCode) {
    ProfileResponse businessPartner = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnRestSimpleResponse<CogsValueResponse> response;
    if (Objects.nonNull(businessPartner) && Objects.nonNull(businessPartner.getCompany())
        && !MERCHANT_TYPE_ORDERS_FULLFILL_BY_BLIBLI.contains(businessPartner.getCompany().getMerchantType())) {
      response = pbpFeign.getCogsValue(materialCode);
      ResponseHelper.validateResponse(response);
      return response.getValue().getMovingPrice();
    } else {
      log.error("BusinessPartner is invalid, businessPartner : {} ", businessPartner);
      return null;
    }
  }

  @Override
  public void downloadProductTemplate(String username, String businessPartnerCode, String categoryId,
      HttpServletResponse response, boolean isOnlyExternal) throws Exception {
    Blob blob = null;
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    Set<String> accessiblePickupPointCodes;
    if (userPicService.shouldRestrictAccess(profileResponse)) {
      accessiblePickupPointCodes = mandatoryParameterHelper.getPickupPoints();
    } else {
      accessiblePickupPointCodes = Collections.emptySet();
    }
    List<PickupPointOutboundResponse> pickupPointCodeResponseList =
      businessPartnerService.getPickupPointsForBusinessPartner(businessPartnerCode,
        accessiblePickupPointCodes);
    ResponseHelper.addPickupPointDetailsInProfileResponse(profileResponse, pickupPointCodeResponseList);
    MerchantType merchantType = RequestHelper.getMerchantType(profileResponse);
    boolean isInternationalMerchant = profileResponse.getCompany().isInternationalFlag();
    CategoryDetailResponse category = categoryService.getCategoryDetail(categoryId);
    String categoryName = category.getName().replaceAll(StringUtils.SPACE, UNDERSCORE);
    List<AttributeResponse> definingAttributes = generateTemplateDefiningAttribute(category);
    List<AttributeResponse> predefinedAttributes = generateTemplatePredefinedAttribute(category);
    blob = isInternationalMerchant ?
            gcsService.downloadFile(gcsProperties.getBucketName(), gcsProperties.getTemplateDirectory() + Constants.ROOT
                    + categoryUploadTemplateFileEnglish) :
            gcsService.downloadFile(gcsProperties.getBucketName(),
                    gcsProperties.getTemplateDirectory() + Constants.ROOT + categoryUploadTemplateFile);
    boolean bopisEligible =
        RequestHelper.isBopisEligible(RequestHelper.getMerchantTypeFromProfileResponse(profileResponse),
            category.isBopisEligible(), bopisCategoryRestrictionEnabled,
            Arrays.asList(bopisUnsupportedMerchantTypes.split(Constants.COMMA_DELIMITER_NO_SPACE)), isOnlyExternal);
    XSSFWorkbook workbook =
        ExcelTemplateUtil.getProductTemplate(category, profileResponse, isInternationalMerchant, definingAttributes,
            predefinedAttributes, blob, merchantType, pickupPointNameConcat, ppNameDelimiter,
            setShippingEligibility(profileResponse, bpBopisRestrictionEnabled, bopisEligible),
            isEligibleForBundlingColumns(profileResponse), instoreNewFlowEnabled, productSuitabilityFeatureEnabled);

    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    workbook.write(bos);
    BeanUtilsConfigurer.configure();
    byte[] bytes = bos.toByteArray();
    response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
    response.setHeader("Content-disposition", "attachment; filename=" + categoryName + "_template.xlsx");
    response.setContentLength(bytes.length);
    response.getOutputStream().write(bytes);
    response.getOutputStream().flush();
  }

  private boolean isEligibleForBundlingColumns(ProfileResponse profileResponse) {
    return productBundlingEnabled && Arrays.asList(
            productBundlingEligibleMerchantTypes.split(Constants.COMMA_DELIMITER_NO_SPACE))
        .contains(profileResponse.getCompany().getMerchantType());
  }

  private ShippingTypeEligibility setShippingEligibility(ProfileResponse profileResponse,
    boolean bpBopisRestrictionEnabled, boolean bopisEligible) {
    ShippingTypeEligibility shippingTypeEligibility = new ShippingTypeEligibility();
    if (bpBopisRestrictionEnabled) {
      shippingTypeEligibility.setEligibleForBigProduct(
        BooleanUtils.toBooleanDefaultIfNull(profileResponse.getBigProductFlag(), true));
      shippingTypeEligibility.setEligibleForBopisProduct(
        BooleanUtils.toBooleanDefaultIfNull(profileResponse.getBopisFlag(), true) && bopisEligible);
    }
    return shippingTypeEligibility;
  }


  @Override
  public boolean checkSyncStockModeAndProductPermission(String merchantCode) {
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(merchantCode);
    if (Objects.isNull(profileResponse.getCompany())) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, ErrorMessages.DATA_NOT_COMPLETE);
    }
    boolean isSyncStockMode = isSyncStockMode(profileResponse.getCompany().getInventoryFulfillment());
    boolean hasAccessibility = false;
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    if (accessibilties.contains(Accessibilty.FUNCTION_MAINTAIN_PRODUK_SKU_SYNC_STOCK)) {
      hasAccessibility = true;
    }
    return isSyncStockMode && hasAccessibility;
  }

  @Override
  public void updateProductDetails(ProductLevel3WebRequest productLevel3WebRequest, String businessPartnerCode,
      String isExternalOnly) throws ApplicationRuntimeException {
    ProductLevel3Request request =
        RequestHelper.toProductLevel3RequestFromProductLevel3WebRequest(productLevel3WebRequest);
    ProfileResponse businessPartner = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    profileResponseAndRequestValidation(businessPartner, request);
    List<PickupPointOutboundResponse> pickupPointCodeResponseList =
        businessPartnerService.getAllPickupPointsForBusinessPartner(businessPartnerCode);
    ResponseHelper.addPickupPointDetailsInProfileResponse(businessPartner, pickupPointCodeResponseList);
    checkPickupPoint(request.getItems().get(0).getPickupPointCode(), businessPartner.getPickupPoints());
    request.setBusinessPartnerCode(businessPartnerCode);
    GdnRestSingleResponse<ProductLevel3OrderResponse> productLevel3OrderResponseGdnRestSingleResponse =
        pbpFeign.filterDetailOrderByGdnSku(businessPartnerCode, request.getItems().get(0).getItemSku());
    ResponseHelper.validateResponse(productLevel3OrderResponseGdnRestSingleResponse);
    ProductLevel3OrderResponse productLevel3OrderResponse = productLevel3OrderResponseGdnRestSingleResponse.getValue();
    setProductOff2OnFlagNullAndRoundOffSalePrice(request, productLevel3OrderResponse);
    String clientId = mandatoryParameterHelper.getClientId();
    updateSkuAccessibilityCheck(productLevel3OrderResponse, request, clientId);
    brandChangeCheck(productLevel3OrderResponse, request);
    request.setAccessChannel(UpdateProductAccessChannel.MTA_WEB_UPDATE_DETAIL.getDesc());
    AgpSimpleQueryResponse agpResponse =
        agpQueryFeign.findNumberOfOrder(productLevel3WebRequest.getProductSku(), String.valueOf(0), String.valueOf(0), Constants.AGP_ITEM_STATUS);
    ResponseHelper.validateAgpQueryResponse(agpResponse);
    boolean hasOrder = agpResponse.getHits().getTotal() > 0;
    boolean updateLogistics =  StringUtils.equals(Constants.API_CLIENT_ID, clientId) ? false : true;
    GdnBaseRestResponse response = pbpFeign
        .updateAndReturn(DEFAULT_CLIENT_HOST, Boolean.valueOf(isExternalOnly), hasOrder, updateLogistics, request);
    ResponseHelper.validateResponseForErrorCode(response);
  }

  @Override
  public EditProductWebResponse editProductInfo(ProductEditInfoWebRequest productEditInfoWebRequest,
      String businessPartnerCode, boolean isOnlyExternal) throws ApplicationRuntimeException {
    ProductLevel3Request request =
        RequestHelper.toProductLevel3RequestFromProductEditInfoWebRequest(productEditInfoWebRequest);
    ProfileResponse businessPartner = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    profileResponseAndRequestValidation(businessPartner, request);
    request.setBusinessPartnerCode(businessPartnerCode);
    GdnRestSingleResponse<EditProductV2Response> response = pbpFeign.editProductInfo(request, request.getProductSku(), isOnlyExternal);
    return ResponseHelper.validateEditInfoResponse(response);
  }

  private void updateSkuAccessibilityCheck(ProductLevel3OrderResponse productLevel3OrderResponse,
      ProductLevel3Request request, String clientId) throws ApplicationRuntimeException {
    List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
    String existingMerchantSku = productLevel3OrderResponse.getItems().get(0).getMerchantSku();
    String newMerchantSku = request.getItems().get(0).getMerchantSku();
    if (clientId != Constants.API_CLIENT_ID && !StringUtils.equalsIgnoreCase(existingMerchantSku, newMerchantSku)
        && (!accessibility.contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_CHANGE_SELLER_SKU_CHANGE_DATA))) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  private void profileResponseAndRequestValidation(ProfileResponse businessPartner, ProductLevel3Request request)
      throws ApplicationRuntimeException {
    if (Objects.isNull(businessPartner) || !ACTIVE.equals(businessPartner.getMerchantStatus())) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.BUSINESS_PARTNER_NOT_ACTIVE);
    }
    RequestHelper.validateRequest(request);
  }

  private void checkPickupPoint(String pickupPointCode, List<PickupPointDTO> pickupPoints)
      throws ApplicationRuntimeException {
    pickupPoints.stream().map(PickupPointDTO::getCode).filter(code -> StringUtils.equals(code, pickupPointCode))
        .findFirst().orElseThrow(
        () -> new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, ErrorMessages.INVALID_PICKUP_POINT));
  }

  private void setProductOff2OnFlagNullAndRoundOffSalePrice(ProductLevel3Request request,
      ProductLevel3OrderResponse productLevel3OrderResponse) throws ApplicationRuntimeException {
    if (Objects.nonNull(productLevel3OrderResponse) && CollectionUtils.isNotEmpty(productLevel3OrderResponse.getItems())
        && (Objects.isNull(productLevel3OrderResponse.getItems().get(0).getOff2OnActiveFlag())
        || productLevel3OrderResponse.getItems().get(0).getOff2OnActiveFlag()
        .equals(request.getItems().get(0).getOff2OnActiveFlag()))) {
      request.getItems().get(0).setOff2OnActiveFlag(null);
    }
    for (ProductItemLevel3Request productItemLevel3Request : request.getItems()) {
      if (CollectionUtils.isNotEmpty(productItemLevel3Request.getPrices())) {
        for (ProductLevel3PriceRequest productLevel3PriceRequest : productItemLevel3Request.getPrices()) {
          productLevel3PriceRequest.setSalePrice(Math.ceil(productLevel3PriceRequest.getSalePrice()));
        }
      }
    }
  }


  private void editPriceAccessibilityCheck(ProductLevel3OrderResponse productLevel3OrderResponse,
      ProductLevel3Request request) throws ApplicationRuntimeException {
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    if (CollectionUtils.isNotEmpty(request.getItems().get(0).getPrices())) {
      Double existingPrice = productLevel3OrderResponse.getItems().get(0).getPrices().get(0).getPrice();
      Double newPrice = request.getItems().get(0).getPrices().get(0).getPrice();
      if (!existingPrice.equals(newPrice) && (!accessibilties
          .contains(Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA))) {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS, ErrorMessages.NO_ACCESS_TO_EDIT_PRICE);
      }
    }
  }

  private void editStockAccessibilityCheck(ProductLevel3Request request) throws ApplicationRuntimeException {
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    if (Objects.nonNull(request.getItems().get(0).getDeltaStock())) {
      Integer newStock = request.getItems().get(0).getDeltaStock();
      if (!newStock.equals(0) && !accessibilties.contains(Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA)) {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS, ErrorMessages.NO_ACCESS_TO_EDIT_STOCK);
      }
    }
  }

  private void brandChangeCheck(ProductLevel3OrderResponse productLevel3OrderResponse, ProductLevel3Request request)
      throws ApplicationRuntimeException {
    String existingBrand = productLevel3OrderResponse.getBrand();
    if (!request.getBrand().equals(existingBrand)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.BRAND_CANNOT_BE_CHANGED);
    }
  }

  @Override
  public ProductLevel3MasterWebResponse findDetailByGdnSku(String businessPartnerCode, String gdnSku) {
    String storeId = mandatoryParameterHelper.getStoreId();
    String requestId = mandatoryParameterHelper.getRequestId();
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions
        .checkArgument(Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
            ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    List<PickupPointOutboundResponse> pickupPointCodeResponseList =
        businessPartnerService.getAllPickupPointsForBusinessPartner(businessPartnerCode);
    ResponseHelper.addPickupPointDetailsInProfileResponse(profileResponse, pickupPointCodeResponseList);
    GdnRestSingleResponse<ProductLevel3Response> response = pbpFeign.findDetailByGdnSku(businessPartnerCode, gdnSku);
    ResponseHelper.validateResponse(response);
    List<String> itemsPickUpPoints = response.getValue().getPickupPointCodes();
    ProductLevel3WebResponse productLevel3WebResponse = ResponseHelper.toProductLevel3WebResponse(response.getValue());
    List<PickupPointDTO> pickupPoints = profileResponse.getPickupPoints();
    List<DistinctPickUpPoint> distinctPickUpPoints = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(response.getValue().getPickupPointCodes())) {
      distinctPickUpPoints = new ArrayList<>(
          pickupPoints.stream().filter(pickupPointDTO -> itemsPickUpPoints.contains(pickupPointDTO.getCode())).collect(
              Collectors.toMap(pickupPointDTO -> pickupPointDTO.getCode(), ResponseHelper::toDistinctPickUpPoint))
              .values());
    }
    productLevel3WebResponse.setDistinctPickUpPoints(distinctPickUpPoints);
    ProductLevel3MasterWebResponse productLevel3MasterWebResponse =
        new ProductLevel3MasterWebResponse(productLevel3WebResponse, false, false);
    String merchantType = ResponseHelper
        .getMerchantTypeFromPurchaseTermAndInventoryFulfillment(profileResponse.getCompany().getPurchaseTerm(),
            profileResponse.getCompany().getInventoryFulfillment());
    if (Constants.MERCHANT_TYPE_TD.equalsIgnoreCase(merchantType)) {
      productLevel3MasterWebResponse.setCogsViewable(true);
    }
    if (StringUtils.isNotBlank(productLevel3WebResponse.getCategoryId())) {
      GdnRestSimpleResponse<Boolean> value = pbpFeign.isPristineCategory(productLevel3WebResponse.getCategoryId());
      ResponseHelper.validateResponse(response);
      productLevel3MasterWebResponse.setPristineSupportedCategory(value.getValue());
    }
    if (StringUtils.isNoneBlank(productLevel3WebResponse.getProductCode())) {
      if (sharedProductDetailsForEditableFlag) {
        productLevel3MasterWebResponse.getProductLevel3().setProductEditable(
            isProductEditable(productLevel3WebResponse.getProductCode()));
      } else {
        Long l3count = getL3CountByProductCode(storeId, requestId, productLevel3WebResponse.getProductCode());
        if (l3count == 1) {
          productLevel3MasterWebResponse.getProductLevel3().setProductEditable(true);
        } else {
          productLevel3MasterWebResponse.getProductLevel3().setProductEditable(false);
        }
      }
    }
    this.setProductCampaignAvailabilityFlagForL4Detail(productLevel3MasterWebResponse);
    return productLevel3MasterWebResponse;
  }

  private void setProductCampaignAvailabilityFlagForL4Detail(
      ProductLevel3MasterWebResponse productLevel3MasterWebResponse) {
    Set<String> itemSkus =
        Optional.ofNullable(productLevel3MasterWebResponse.getProductLevel3().getItems()).orElse(new ArrayList<>())
            .stream().map(ProductItemLevel3WebResponse::getItemSku).collect(Collectors.toSet());
    Set<com.gdn.x.campaign.dto.ItemInfoDto> itemInfoDtos =
        Optional.ofNullable(productLevel3MasterWebResponse.getProductLevel3().getItems()).orElse(new ArrayList<>())
            .stream().map(productLevel3SummaryWebResponse -> new com.gdn.x.campaign.dto.ItemInfoDto(
            productLevel3SummaryWebResponse.getItemSku(), productLevel3SummaryWebResponse.getPickupPointCode(), StringUtils.EMPTY))
            .collect(Collectors.toSet());
    ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest =
        ProductCampaignAvailabilityRequest.builder().itemSkus(itemSkus).itemInfo(itemInfoDtos)
            .merchantCode(productLevel3MasterWebResponse.getProductLevel3().getBusinessPartnerCode()).build();
    GdnRestSingleResponse<ProductCampaignAvailabilityResponse> response =
        getProductCampaignAvailabilityResponse(productCampaignAvailabilityRequest);
    for (ProductItemLevel3WebResponse productItemLevel3WebResponse : productLevel3MasterWebResponse.getProductLevel3()
        .getItems()) {
      productItemLevel3WebResponse.setItemCampaignMapped(
          response.getValue().getProductCampaignAvailabilityMap().get(productItemLevel3WebResponse.getItemSku()));
    }
  }

  private boolean isSyncStockMode(String inventoryFulfillment) {
    return InventoryFulfillment.BLIBLI.getName().equals(inventoryFulfillment);
  }

  private List<ProductLevel3SummaryResponse> filterSummaryByGdnSku(String businessPartnerCode, List<String> gdnSkus) {
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    for (String gdnSku : gdnSkus) {
      try {
        GdnRestSingleResponse<ProductLevel3SummaryResponse> response =
            pbpFeign.filterSummaryByGdnSku(businessPartnerCode, gdnSku);
        ResponseHelper.validateResponse(response);
        responses.add(response.getValue());
      } catch (Exception e) {
        log.warn(ErrorMessages.ERROR_PRODUCT_NOT_FOUND, businessPartnerCode, gdnSku, e);
      }
    }
    return responses;
  }

  private Page<ProductLevel3SummaryResponse> getProductLevel3SummaryResponsesRejected(
      InActiveProductWebRequest inActiveRequest, PageRequest pageRequest) {
    GdnRestListResponse<RejectedSkuProductResponse> rejectedProductResponse = pbpFeign
        .filterProductBusinessPartnerSummaryByBusinessPartnerId(pageRequest.getPageNumber(), pageRequest.getPageSize(),
            inActiveRequest.getBusinessPartnerCode(), StringUtils.EMPTY, inActiveRequest.getOrderBy(),
            inActiveRequest.getSortBy());
    ResponseHelper.validateResponse(rejectedProductResponse);
    return new PageImpl<>(ResponseHelper
        .toProductLevel3SummaryResponseFromRejectedSkuProductResponse(rejectedProductResponse.getContent()),
        pageRequest, rejectedProductResponse.getPageMetaData().getTotalRecords());
  }

  private Page<ProductLevel3SummaryResponse> getProductLevel3SummaryResponsesSuspended(
      InActiveProductWebRequest inActiveRequest, PageRequest pageRequest) {
    GdnRestListResponse<SuspensionItemResponse> gdnRestListResponse = pbpFeign.
        getSuspendedItem(pageRequest.getPageNumber(), pageRequest.getPageSize(),
            RequestHelper.toSummaryFilterRequestFromInActiveProductWebRequest(inActiveRequest));
    ResponseHelper.validateResponse(gdnRestListResponse);
    return new PageImpl<>(
        ResponseHelper.toProductLevel3SummaryResponseFromSuspensionItemResponse(gdnRestListResponse.getContent()),
        pageRequest, gdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  private Page<ProductLevel3SummaryResponse> getProductLevel3SummaryResponsesArchived(
      InActiveProductWebRequest inActiveRequest, PageRequest pageRequest, SortOrder sort) {
    GdnRestListResponse<ProductLevel3SummaryResponse> gdnRestListResponse = pbpFeign
        .filterSummary(inActiveRequest.getBusinessPartnerCode(), pageRequest.getPageNumber(), pageRequest.getPageSize(),
            sort.getSortType(), sort.getSortBy(),
            RequestHelper.toProductLevel3SummaryRequestFromInActiveProductWebRequestTest(inActiveRequest));
    ResponseHelper.validateResponse(gdnRestListResponse);
    return new PageImpl<>(gdnRestListResponse.getContent(), pageRequest,
        gdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  private List<AttributeResponse> generateTemplateDefiningAttribute(CategoryDetailResponse category) throws Exception {
    List<AttributeResponse> attributes = new ArrayList<AttributeResponse>();
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (categoryAttribute.getAttribute().getAttributeType().equals(AttributeType.DEFINING_ATTRIBUTE.name())
          && !categoryAttribute.isMarkForDelete()) {
        attributes.add(categoryService.getAttributeDetail(categoryAttribute.getAttribute().getId()));
      }
    }
    return attributes;
  }

  private List<AttributeResponse> generateTemplatePredefinedAttribute(CategoryDetailResponse category) {
    List<AttributeResponse> attributes = new ArrayList<>();
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (RequestHelper.checkHideFromSellerAttribute(productSuitabilityFeatureEnabled, categoryAttribute)) {
        continue;
      }
      if (categoryAttribute.getAttribute().getAttributeType().equals(AttributeType.PREDEFINED_ATTRIBUTE.name())
          && !categoryAttribute.isMarkForDelete()) {
        if (BRAND.equals(categoryAttribute.getAttribute().getName())) {
          attributes.add(categoryAttribute.getAttribute());
        } else {
          AttributeResponse attributeResponse =
              categoryService.getAttributeDetail(categoryAttribute.getAttribute().getId());
          attributes.add(attributeResponse);
        }
      }
    }
    return attributes;
  }

  private List<PredefinedAllowedAttributeValueResponse> removeDefaultInternalCreatedBrands(List<PredefinedAllowedAttributeValueResponse> finalBrandList,
      Map<String, PredefinedAllowedAttributeValueResponse> internalCreatedBrands,
      List<PredefinedAllowedAttributeValueResponse> finalBrandWithDefaultOnTop) {
    finalBrandList.removeIf(predefinedAllowedAttributeValueResponse -> internalCreatedBrands
        .containsKey(predefinedAllowedAttributeValueResponse.getValue()));
    List<String> defaultBrand = finalBrandWithDefaultOnTop.stream()
        .map(predefinedAllowedAttributeValueResponse -> predefinedAllowedAttributeValueResponse.getValue())
        .collect(Collectors.toList());
    finalBrandList = finalBrandList.stream().filter(predefinedAllowedAttributeValueResponse -> !defaultBrand
        .contains(predefinedAllowedAttributeValueResponse.getValue())).collect(Collectors.toList());
    return finalBrandList;
  }

  private void getActiveBrandsByCategoryId(String categoryId, List<PredefinedAllowedAttributeValueResponse> allBrands,
      List<PredefinedAllowedAttributeValueResponse> finalBrandList,
      List<PredefinedAllowedAttributeValueResponse> allActiveBrands) throws Exception {
    List<PredefinedAllowedAttributeValueResponse> activeBrands;
    try {
      activeBrands = brandService.activeBrandsByCategoryId(categoryId);
      List<String> activeBrandValues = activeBrands.stream()
          .map(predefinedAllowedAttributeValueResponse -> predefinedAllowedAttributeValueResponse.getValue())
          .collect(Collectors.toList());
      allBrands = allBrands.stream().filter(predefinedAllowedAttributeValueResponse -> !activeBrandValues
          .contains(predefinedAllowedAttributeValueResponse.getValue()))
          .sorted(Comparator.comparing(PredefinedAllowedAttributeValueResponse::getValue)).collect(Collectors.toList());
      List<String> allActiveBrandsList = allActiveBrands.stream()
          .map(predefinedAllowedAttributeValueResponse -> predefinedAllowedAttributeValueResponse.getValue())
          .collect(Collectors.toList());
      activeBrands = activeBrands.stream().filter(predefinedAllowedAttributeValueResponse -> !allActiveBrandsList
          .contains(predefinedAllowedAttributeValueResponse.getValue()))
          .sorted(Comparator.comparing(PredefinedAllowedAttributeValueResponse::getValue)).collect(Collectors.toList());
      finalBrandList.addAll(activeBrands);
      finalBrandList.addAll(allBrands);
    } catch (Exception e) {
      allBrands = allBrands.stream().sorted(Comparator.comparing(PredefinedAllowedAttributeValueResponse::getValue))
          .collect(Collectors.toList());
      finalBrandList.addAll(allBrands);
      log.error("Error while getting active brands from PBP , categoryId :{}", categoryId, e);
    }
  }

  private PredefinedAllowedAttributeValueResponse processInReviewBrands(
      List<PredefinedAllowedAttributeValueResponse> allBrands, PredefinedAllowedAttributeValueResponse inReviewBrand) {
    allBrands.removeIf(predefinedAllowedAttributeValueResponse -> predefinedAllowedAttributeValueResponse.getValue()
        .equals(inReviewBrand.getValue()));
    inReviewBrand.setValue(inReviewBrand.getValue() + IN_REVIEW);
    return inReviewBrand;
  }

  private List<PredefinedAllowedAttributeValueResponse> getInReviewBrands(String businessPartnerCode) throws Exception {
    List<PredefinedAllowedAttributeValueResponse> response =
        brandService.getBrandSuggestions(StringUtils.EMPTY, businessPartnerCode, true, true);
    if (CollectionUtils.isEmpty(response)) {
      return new ArrayList<>();
    } else {
      List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponses = response;
      log.info("predefinedAllowedAttributeValueResponses responses before sort");
      predefinedAllowedAttributeValueResponses.sort(Comparator
          .comparing(predefinedAllowedAttributeValueResponse -> predefinedAllowedAttributeValueResponse.getValue()));
      return predefinedAllowedAttributeValueResponses;
    }
  }

  @Override
  public void unsynchronizeProduct(String businessPartnerCode, String productSku, String itemSku) {
    log.info("Unsynchronizing a product for business partner code : {}, productSku : {} and itemSku : {}",
        businessPartnerCode, productSku, itemSku);
    List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
    if (!accessibility.contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_CHANGE_PRODUCT_SYNCHRONIZATION_CHANGE_DATA)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions
        .checkArgument(Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
            ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    GdnBaseRestResponse response = pbpFeign.unsynchronizeProduct(DEFAULT_CLIENT_HOST, productSku, itemSku);
    ResponseHelper.validateResponseForErrorCode(response);
  }

  @Override
  public void synchronizeProduct(String businessPartnerCode, String productSku, String itemSku) {
    log.info("Synchronizing a product for business partner code : {}, productSku : {} and itemSku : {}",
        businessPartnerCode, productSku, itemSku);
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    if (!accessibilties
        .contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_CHANGE_PRODUCT_SYNCHRONIZATION_CHANGE_DATA)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions
        .checkArgument(Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
            ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    GdnBaseRestResponse response = pbpFeign.synchronizeProduct(DEFAULT_CLIENT_HOST, productSku, itemSku);
    ResponseHelper.validateResponseForErrorCode(response);
  }

  @Override
  public void updateProductImage(UpdateImageRequest request, boolean isOnlyExternal) throws Exception {
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    if (!accessibilties.contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_UPLOAD_IMAGE)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    GdnBaseRestResponse response = pbpFeign.updateProductImage(isOnlyExternal, request);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public YouTubeAPIWebResponse validateYouTubeUrl(String youTubeUrl) throws Exception {
    GdnRestSingleResponse<ProductSystemParameterResponse> response =
        pbpFeign.findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH);
    ResponseHelper.validateResponse(response);
    String videoId = validateUrlAndGetVideoId(Pattern.compile(youtubeRegex), youTubeUrl);
    if (Objects.nonNull(videoId) && Objects.nonNull(validateUrlAndGetVideoId(YOUTUBE_PATTERN, youTubeUrl))) {
      if (Boolean.parseBoolean(response.getValue().getValue())) {
        String[] apiKeys = youTubeDataApiKey.split(",");
        int apiKeyIndex = (int) (Math.random() * apiKeys.length);
        String youTubeApiKey = apiKeys[apiKeyIndex];
        log.debug("Validating youtube videoId: {} using api key index: {}", videoId, apiKeyIndex);
        return ValidateUrlUtil.isYouTubeUrlActive(videoId, youTubeApiKey, youTube);
      } else {
        return YouTubeAPIWebResponse.builder().validationSwitch(false).isValid(true).build();
      }
    }
    return YouTubeAPIWebResponse.builder().isValid(false)
        .validationSwitch(Boolean.parseBoolean(response.getValue().getValue())).build();
  }

  private static String validateUrlAndGetVideoId(Pattern pattern, String youtubeUrl) {
    Matcher matcher = pattern.matcher(youtubeUrl);
    if (matcher.find()) {
      return matcher.group();
    }
    return null;
  }

  @Override
  public Integer getMinimumPrice() {
    GdnRestSimpleResponse<Integer> response = pbpFeign.getMinimumPrice();
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public List<BrandPredefinedAttributeValueWebResponse> getBrandPredefinedAllowedAttributeValueDetail(String brandCode,
      String brandStatus) {
    GdnRestSingleResponse<BrandPredefinedAttributeValueResponse> response =
        pcbFeign.getBrandPredefinedValueDetail(brandCode, brandStatus);
    ResponseHelper.validateBrandDetailResponse(response);
    return ResponseHelper.toBrandPredefinedAttributeValueWebResponse(response.getValue());
  }

  @Override
  public boolean updatePriceAndStock(ProductPriceAndStockUpdateWebRequest productPriceAndStockUpdateWebRequest,
      String businessPartnerCode) {
    WholesalePriceSkuResponse wholesalePriceSkuResponse;
    if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
      WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest =
          RequestHelper.getWholesalePriceSkuDetailListRequest(productPriceAndStockUpdateWebRequest);
      GdnRestListResponse<WholesalePriceSkuResponse> wholesalePriceSkuDetailV2 = this.productPricingFeign
          .getWholesalePriceSkuDetailV2(mandatoryParameterHelper.getStoreId(),
              mandatoryParameterHelper.getRequestId(), wholesalePriceSkuDetailListRequest);
      ResponseHelper.validateResponse(wholesalePriceSkuDetailV2);
      wholesalePriceSkuResponse = wholesalePriceSkuDetailV2.getContent().get(0);
    } else {
      GdnRestSingleResponse<WholesalePriceSkuResponse> wholesaleResponse =
          this.productPricingFeign.getWholesalePriceDetail(productPriceAndStockUpdateWebRequest.getItemSku());
      ResponseHelper.validateResponse(wholesaleResponse);
      log.info("Wholesale response : {}", wholesaleResponse);
      wholesalePriceSkuResponse = wholesaleResponse.getValue();
    }
    GdnPreconditions.checkArgument(
        !(Boolean.TRUE.equals(productPriceAndStockUpdateWebRequest.getWholesalePriceActivated()) && Boolean.TRUE
            .equals(wholesalePriceSkuResponse.getPromoActive())), "Wholesale Promo flag is on");
    log.info("Updating price and wholesale details for : {}", productPriceAndStockUpdateWebRequest.getItemSku());
    updateItemPrice(productPriceAndStockUpdateWebRequest);
    updateItemStock(businessPartnerCode, productPriceAndStockUpdateWebRequest);
    return true;
  }

  private void updateItemStock(String businessPartnerCode, ProductPriceAndStockUpdateWebRequest productPriceAndStockUpdateWebRequest) {
    ProductLevel3StockRequest productLevel3StockRequest =
        ProductLevel3StockRequest.builder().gdnSku(productPriceAndStockUpdateWebRequest.getItemSku())
            .deltaStock(productPriceAndStockUpdateWebRequest.getDeltaStock())
            .minimumStock(productPriceAndStockUpdateWebRequest.getMinimumStock()).build();
    log.info("Updating stock info for : {}", productPriceAndStockUpdateWebRequest.getItemSku());
    GdnBaseRestResponse stockUpdateResponse =
        this.pbpFeign.updateItemStock(DEFAULT_CLIENT_HOST, businessPartnerCode, productLevel3StockRequest);
    ResponseHelper.validateResponse(stockUpdateResponse);
  }

  private void updateItemPrice(ProductPriceAndStockUpdateWebRequest productPriceAndStockUpdateWebRequest) {
    ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest =
        RequestHelper.toProductPriceAndWholesaleRequest(productPriceAndStockUpdateWebRequest);
    GdnBaseRestResponse priceUpdateResponse = this.pbpFeign
        .updateItemPrice(DEFAULT_CLIENT_HOST, productPriceAndStockUpdateWebRequest.getItemSku(),
            productPriceAndWholesaleRequest);
    ResponseHelper.validateResponse(priceUpdateResponse);
  }

  /**
   * Get CategoryHierarchyWithProductCount from PCB
   *
   * @param categoryCode
   * @return
   */
  public WholesaleMappingResponse getCategoryWholesaleConfig(String categoryId, String categoryCode) {
    GdnRestSingleResponse<WholesaleMappingResponse> wholesaleMappingResponse =
        pcbFeign.getCategoryWholesaleConfiguration(categoryId, categoryCode);
    ResponseHelper.validateResponse(wholesaleMappingResponse);
    return wholesaleMappingResponse.getValue();
  }

  @Override
  public List<PriceChangeCompatibleResponse> getPriceChangeCompatibility(String storeId, String requestId,
      List<PriceChangeCompatibleRequest> priceChangeCompatibleRequests) throws Exception {
    List<PriceChangeCompatibleResponse> priceChangeCompatibleResponseList = new ArrayList<>();
    WholesalePriceSkuDetailListRequest request = new WholesalePriceSkuDetailListRequest();
    List<ItemInfoDto> itemInfoList = new ArrayList<>();
    request.setItemSkus(priceChangeCompatibleRequests.stream().map(PriceChangeCompatibleRequest::getItemSku)
        .collect(Collectors.toSet()));
    for(PriceChangeCompatibleRequest priceChangeCompatibleRequest : priceChangeCompatibleRequests) {
      ItemInfoDto itemInfoDto = new ItemInfoDto();
      itemInfoDto.setItemSku(priceChangeCompatibleRequest.getItemSku());
      itemInfoDto.setPickupPointCode(priceChangeCompatibleRequest.getPickUpPointCode());
      itemInfoDto.setItemPickupPointId(
          priceChangeCompatibleRequest.getItemSku() + Constants.DASH_SEPARATOR + priceChangeCompatibleRequest
              .getPickUpPointCode());
      itemInfoList.add(itemInfoDto);
    }
    request.setItemInfo(itemInfoList);
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponses =
        productPricingService.getItemWholesaleConfig(storeId, requestId, request);
    for (PriceChangeCompatibleRequest priceChangeCompatibleRequest : priceChangeCompatibleRequests) {
      PriceChangeCompatibleResponse priceChangeCompatibleResponse =
          getPriceChangeCompatibilityForOneSku(priceChangeCompatibleRequest, wholesalePriceSkuResponses.get(
              priceChangeCompatibleRequest.getItemSku() + Constants.DASH_SEPARATOR + priceChangeCompatibleRequest
                  .getPickUpPointCode()));
      priceChangeCompatibleResponse.setPickUpPointCode(priceChangeCompatibleRequest.getPickUpPointCode());
      priceChangeCompatibleResponseList.add(priceChangeCompatibleResponse);
    }
    return priceChangeCompatibleResponseList;
  }

  @Override
  public List<WholesalePromoResponse> getWholesalePromoStatus(String storeId, String requestId, List<WholeSaleDetailListWebRequest>
      wholeSaleDetailListWebRequests) {
    List<WholesalePromoResponse> wholesalePromoResponses = new ArrayList<>();
    GdnRestListResponse<WholesalePriceSkuResponse> wholesalePriceSkuResponses;
    Set<String> request = wholeSaleDetailListWebRequests.stream().map(WholeSaleDetailListWebRequest::getItemSku)
        .collect(Collectors.toSet());
    if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
      List<ItemInfoDto> itemInfoDtoList = RequestHelper.toItemInfoDtoList(wholeSaleDetailListWebRequests);
      wholesalePriceSkuResponses = this.productPricingFeign.getWholesalePriceSkuDetailV2(storeId,
          requestId, new WholesalePriceSkuDetailListRequest(request, itemInfoDtoList));
    } else {
      wholesalePriceSkuResponses = this.productPricingFeign.getWholesalePriceSkuDetail(storeId,
          requestId, new WholesalePriceSkuDetailListRequest(request, null));
    }

    ResponseHelper.validateResponse(wholesalePriceSkuResponses);
    for (WholesalePriceSkuResponse wholesaleResponse : wholesalePriceSkuResponses.getContent()) {
      WholesalePromoResponse wholesalePromoResponse = new WholesalePromoResponse();
      wholesalePromoResponse.setItemSku(wholesaleResponse.getItemSku());
      wholesalePromoResponse.setPickUpPointCode(wholesaleResponse.getPickUpPointCode());
      wholesalePromoResponse.setWholesalePromo(
          Objects.nonNull(wholesaleResponse.getPromoActive()) ? wholesaleResponse.getPromoActive() : false);
      wholesalePromoResponses.add(wholesalePromoResponse);
    }
    return wholesalePromoResponses;
  }

  private PriceChangeCompatibleResponse getPriceChangeCompatibilityForOneSku(
      PriceChangeCompatibleRequest priceChangeCompatibleRequest, WholesalePriceSkuResponse wholesalePriceSkuResponse)
      throws Exception {
    boolean updateAllowed = true;
    PriceChangeCompatibleResponse priceChangeCompatibleResponse = new PriceChangeCompatibleResponse();
    priceChangeCompatibleResponse.setItemSku(priceChangeCompatibleRequest.getItemSku());
    WholesaleMappingResponse categoryWholesaleConfig =
        this.getCategoryWholesaleConfig(null, priceChangeCompatibleRequest.getCategoryCode());
    List<MinWholesaleDiscountResponse> minWholesaleDiscountResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(categoryWholesaleConfig.getWholesaleConfig())) {
      minWholesaleDiscountResponses = categoryWholesaleConfig.getWholesaleConfig().get(0).getMinWholesaleDiscount();
      minWholesaleDiscountResponses.sort(Comparator.comparing(MinWholesaleDiscountResponse::getPrice));
    }
    if (Objects.isNull(wholesalePriceSkuResponse.getSkuStatus()) || CollectionUtils
        .isEmpty(Collections.singleton(wholesalePriceSkuResponse.getWholesaleRules())) || (
        CollectionUtils.isNotEmpty(minWholesaleDiscountResponses)
            && minWholesaleDiscountResponses.get(0).getPrice() > priceChangeCompatibleRequest.getSalePrice())) {
      priceChangeCompatibleResponse.setUpdateAllowed(true);
      return priceChangeCompatibleResponse;
    }
    Map<Integer, Double> quantityDiscountMap;
    if (Constants.PERCENTAGE.equalsIgnoreCase(categoryWholesaleConfig.getConfigurationType())) {
      quantityDiscountMap = categoryWholesaleConfig.getWholesaleConfig().stream().collect(Collectors
          .toMap(WholesaleConfigResponse::getQuantity,
              percentage -> percentage.getMinWholesaleDiscount().get(0).getPercentage()));
    } else {
      quantityDiscountMap = categoryWholesaleConfig.getWholesaleConfig().stream().collect(Collectors
          .toMap(WholesaleConfigResponse::getQuantity, wholesaleConfig -> ResponseHelper
              .getDiscountBracket(wholesaleConfig.getMinWholesaleDiscount(),
                  priceChangeCompatibleRequest.getSalePrice())));
    }
    updateAllowed = ResponseHelper.getThreshold(wholesalePriceSkuResponse.getWholesaleRules(), quantityDiscountMap);
    priceChangeCompatibleResponse.setUpdateAllowed(updateAllowed);
    return priceChangeCompatibleResponse;
  }

  @Override
  public ProductScoreRuleWebResponse getProductScoreRule(String categoryCode) {
    GdnRestSingleResponse<ProductScoreRuleResponse> productScoreRuleResponse =
        xProductFeign.getProductScoreRule(categoryCode);
    ResponseHelper.validateResponse(productScoreRuleResponse);
    return ResponseHelper.toProductScoreRuleWebResponse(productScoreRuleResponse.getValue());
  }

  @Override
  public ProductSystemParameterSwitchWebResponse getSystemParameterSwitches() {
    GdnRestSingleResponse<ProductSystemParameterSwitchResponse> response =
        this.pbpFeign.getSystemParameterSwitch();
    ResponseHelper.validateResponse(response);
    return new ProductSystemParameterSwitchWebResponse(response.getValue().getProductSystemParameterSwitchValues());
  }

  @Override
  public UpcCodeAndImagesWebResponse getUpcCodeAndImages(String storeId, String requestId, String productSku) {
    GdnRestSingleResponse<ProductAndItemsResponse> response =
        xProductFeign.getProductAndItems(storeId, requestId, productSku, false);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toUpcCodeAndImagesWebResponse(response.getValue());
  }

  @Override
  public Page<ProductLevel3SummaryDetailsWebResponse> getProductLevel3VariantList(
      ProductLevel3VariantsWebRequest request, PageRequest pageRequest) {
    log.info("Fetching the all variants for products for request : {}", request);
    GdnRestListResponse<ProductLevel3SummaryDetailsResponse> gdnRestListResponse = pbpFeign
        .filterSummaryDetails(request.getBusinessPartnerCode(), pageRequest.getPageNumber(), pageRequest.getPageSize(),
            RequestHelper.toProductLevel3SummaryDetailsRequest(request));
    ResponseHelper.validateResponse(gdnRestListResponse);
    List<ProductLevel3SummaryDetailsWebResponse> productLevel3SummaryDetailsWebResponseList =
        gdnRestListResponse.getContent().stream().map(ResponseHelper::toProductLevel3SummaryDetailsWebResponse)
            .collect(Collectors.toList());
    return new PageImpl<>(productLevel3SummaryDetailsWebResponseList, pageRequest,
        gdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  private void setProductItemCampaignAvailabilityFlag(
      List<ProductLevel3SummaryDetailsWebResponse> productLevel3SummaryDetailsWebResponseList,
      ProductCampaignAvailabilityResponse response) {
    for (ProductLevel3SummaryDetailsWebResponse productLevel3SummaryDetailsWebResponse : productLevel3SummaryDetailsWebResponseList) {
      productLevel3SummaryDetailsWebResponse.setItemCampaignMapped(
          response.getProductCampaignAvailabilityMap().get(productLevel3SummaryDetailsWebResponse.getItemSku()));
    }
  }

  public OrderPlacedWebResponse checkSuccessfullOrderPlacedForProductSku(String productSku) throws Exception {
    AgpSimpleQueryResponse agpResponse =
        agpQueryFeign.findNumberOfOrder(productSku, String.valueOf(0), String.valueOf(0), "DF,X");
    ResponseHelper.validateAgpQueryResponse(agpResponse);
    return new OrderPlacedWebResponse(productSku, agpResponse.getHits().getTotal() > 0);
  }

  private Long getL3CountByProductCode(String storeId, String requestId, String productCode) {
    GdnRestSingleResponse<SimpleLongResponse> response =
        xProductFeign.getL3CountByProductCode(storeId, requestId, productCode);
    ResponseHelper.validateResponse(response);
    return response.getValue().getValue();
  }

  private boolean isProductEditable(String productCode) {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        xProductFeign.sharedProductByProductCode(productCode);
    ResponseHelper.validateResponse(response);
    return !response.getValue().getResult();
  }

  @Override
  public String getPinpointStatusByProductSku(String productSku, String businessPartnerCode) {
    StringJoiner response = new StringJoiner(Constants.COMMA_DELIMITER_NO_SPACE);
    GdnPreconditions.checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    GdnRestSingleResponse<ProductPickupPointListResponse> pickupPointCodesByProductSku =
        this.xProductFeign.getPickupPointCodesByProductSku(productSku);
    ResponseHelper.validateResponse(pickupPointCodesByProductSku);
    ProfileResponse profileResponse =
        businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    List<PickupPointOutboundResponse> pickupPointCodeResponseList =
        businessPartnerService.getAllPickupPointsForBusinessPartner(businessPartnerCode);
    ResponseHelper.addPickupPointDetailsInProfileResponse(profileResponse, pickupPointCodeResponseList);
    List<String> pickupPointListWithGeolocation = profileResponse.getPickupPoints().stream()
        .filter(pickupPointDTO -> Objects.nonNull(pickupPointDTO.getGeolocation()))
        .map(PickupPointDTO::getCode)
        .collect(Collectors.toList());
    for (String pickupPointCodeByProductSku : pickupPointCodesByProductSku.getValue()
        .getPickupPointCodes()) {
      if (!pickupPointListWithGeolocation.contains(pickupPointCodeByProductSku)) {
        response.add(pickupPointCodeByProductSku);
      }
    }
    return response.toString();
  }

  @Override
  public Page<PickupPointCodeWebResponse> getPickupPointCodesByProductSku(int page, int size, String productSku,
      boolean needCorrection, String businessPartnerCode, boolean fbbActivated) throws Exception {
    GdnRestListResponse<PickupPointCodeResponse> pickupPointCodeResponses =
      pbpFeign.getPickupPointCodes(page, size, productSku, needCorrection, businessPartnerCode, fbbActivated);
    ResponseHelper.validateResponse(pickupPointCodeResponses);
    List<PickupPointCodeWebResponse> pickupPointCodeWebResponses = pickupPointCodeResponses.getContent().stream().map(
            pickupPointCodeResponse -> new PickupPointCodeWebResponse(pickupPointCodeResponse.getItemSku(),
                pickupPointCodeResponse.getItemName(), pickupPointCodeResponse.getPickupPointCode()))
        .collect(Collectors.toList());
    return new PageImpl<>(pickupPointCodeWebResponses, PageRequest.of(page, size),
        pickupPointCodeResponses.getPageMetaData().getTotalRecords());
  }

  @Override
  public UniquePickupPointCodeWebResponse getUniquePickupPointCodesByProductSku(String productSku)
      throws Exception {
    GdnRestSingleResponse<UniquePickupPointCodeResponse> uniquePickupPointCodeResponse =
        pbpFeign.getUniquePickupPointCodes(productSku);
    ResponseHelper.validateResponse(uniquePickupPointCodeResponse);
    UniquePickupPointCodeWebResponse uniquePickupPointCodeWebResponse =
        new UniquePickupPointCodeWebResponse(uniquePickupPointCodeResponse.getValue().getItemSkus(),
            uniquePickupPointCodeResponse.getValue().getPickupPointCodes());
    return uniquePickupPointCodeWebResponse;
  }

  @Override
  public ItemsPriceStockImagesUpdateWebResponse updateItemsPriceStockImages(String businessPartnerCode,
      UpdateItemsPriceStockImagesWebRequest updateItemsPriceStockImagesWebRequest) {
    UpdateItemsPriceStockImagesRequest updateItemsPriceStockImagesRequest =
        RequestHelper.toUpdateItemsPriceStockImagesRequest(updateItemsPriceStockImagesWebRequest);
    GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> response =
        pbpFeign.updateItemsPriceStockImages(businessPartnerCode, updateItemsPriceStockImagesRequest);
    ResponseHelper.validateResponse(response);
    ItemsPriceStockImagesUpdateWebResponse itemsPriceStockImagesUpdateWebResponse =
        ResponseHelper.toItemsPriceStockImagesUpdateWebResponse(response.getValue());
    return itemsPriceStockImagesUpdateWebResponse;
  }

  @Override
  public Page<HistorySummaryWebResponse> getProductEditHistorySummary(
      HistorySummaryWebRequest historySummaryWebRequest, int page, int size) {
    GdnRestListResponse<HistoryResponse> response = pbpFeign.getProductHistorySummary(page, size, RequestHelper.toHistoryRequest(historySummaryWebRequest));
    ResponseHelper.validateResponse(response);
    List<HistorySummaryWebResponse> historySummaryWebResponse =
        ResponseHelper.toHistorySummaryWebResponse(response.getContent());
    return new PageImpl<>(historySummaryWebResponse, PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void updateLogistics(ProductLevel3UpdateWebRequest productLevel3UpdateWebRequest, String businessPartnerCode,
      String isExternalOnly, boolean isNeedCorrection) {
    ProductLevel3UpdateRequest request = RequestHelper.toProductLevel3UpdateRequest(productLevel3UpdateWebRequest);
    ProfileResponse businessPartner = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    if (Objects.isNull(businessPartner) || !ACTIVE.equals(businessPartner.getMerchantStatus())) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.BUSINESS_PARTNER_NOT_ACTIVE);
    }
    request.setBusinessPartnerCode(businessPartnerCode);
    request.setAccessChannel(UpdateProductAccessChannel.MTA_WEB_UPDATE_DETAIL.getDesc());
    GdnBaseRestResponse response = pbpFeign.updateLogistics(Boolean.valueOf(isExternalOnly), request, isNeedCorrection);
    ResponseHelper.validateResponseForErrorCode(response);
  }

  @Override
  public OrderStatusWebResponse getOrderStatusByProductCode(String productCode) {
    GdnRestSimpleResponse<ProductSkuResponseList> pbpResponse = this.pbpFeign.getProductSkusByProductCode(productCode);
    ResponseHelper.validateResponse(pbpResponse);
    boolean hasOrder = false;
    for (String productSku : pbpResponse.getValue().getProductSkuList()) {
      AgpSimpleQueryResponse agpResponse =
          agpQueryFeign.findNumberOfOrder(productSku, String.valueOf(0), String.valueOf(0), Constants.AGP_ITEM_STATUS);
      ResponseHelper.validateAgpQueryResponse(agpResponse);
      if (agpResponse.getHits().getTotal() > 0) {
        hasOrder = true;
        break;
      }
    }
    return OrderStatusWebResponse.builder().productCode(productCode).hasOrder(hasOrder).build();
  }

  @Override
  public PickupPointUpdateWebResponse updatePickupPoints(PickupPointUpdateWebRequest pickupPointUpdateWebRequest,
      String businessPartnerCode) throws Exception {
    ProfileResponse businessPartner = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    if (Objects.isNull(businessPartner) || !ACTIVE.equals(businessPartner.getMerchantStatus())) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.BUSINESS_PARTNER_NOT_ACTIVE);
    }
    if (pickupPointUpdateWebRequest.isMarkDefaultAddress() && StringUtils.isNotBlank(
        pickupPointUpdateWebRequest.getDefaultPickupPointCode())) {
      pickupPointService.markDefaultAddressForBusinessPartner(StringUtils.EMPTY, StringUtils.EMPTY, businessPartnerCode,
          pickupPointUpdateWebRequest.getDefaultPickupPointCode());
      businessPartnerService.updateDefaultPickupPointCode(
          RequestHelper.toMarkPickupPointDefaultRequest(businessPartnerCode,
              pickupPointUpdateWebRequest.getDefaultPickupPointCode()));
    }
    PickupPointUpdateRequest pickupPointUpdateRequest =
        RequestHelper.toPickupPointUpdateRequest(pickupPointUpdateWebRequest, businessPartnerCode);
    GdnRestSingleResponse<PickupPointUpdateResponse> response =
        pbpFeign.updatePickupPointCodes(pickupPointUpdateRequest);
    ResponseHelper.validateResponseForErrorCode(response);
    return ResponseHelper.toPickupPointUpdateWebResponse(response.getValue());
  }

  @Override
  public Page<ProductItemNameWebResponse> getProductVariantsNameByProductSku(String productSku, int page, int size) {
    GdnRestListResponse<ProductItemNameResponse> responseGdnRestListResponse =
        pbpFeign.getProductVariantsName(page, size, productSku);
    ResponseHelper.validateResponse(responseGdnRestListResponse);
    List<ProductItemNameWebResponse> productItemNameWebResponses =
        ResponseHelper.toProductItemNameWebResponse(responseGdnRestListResponse.getContent());
    return new PageImpl<>(productItemNameWebResponses, PageRequest.of(page, size),
        responseGdnRestListResponse.getPageMetaData().getTotalRecords());
  }
  @Override
  public ProductLevel3DetailWebResponse getL3DetailByProductSku(String productSku, String businessPartnerCode,
      boolean isNeedCorrection)
      throws Exception {
    GdnPreconditions.checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    ProfileResponse businessPartner = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    if (Objects.isNull(businessPartner) || !ACTIVE.equals(businessPartner.getMerchantStatus())) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.BUSINESS_PARTNER_NOT_ACTIVE);
    }
    GdnRestSingleResponse<ProductLevel3DetailResponse> response =
        pbpFeign.getL3DetailByProductSku(productSku, isNeedCorrection);
    ResponseHelper.validateResponse(response);
    ProductLevel3DetailWebResponse productLevel3DetailWebResponse =
        ResponseHelper.toProductLevel3DetailWebResponse(response.getValue());
    String merchantType = ResponseHelper
        .getMerchantTypeFromPurchaseTermAndInventoryFulfillment(businessPartner.getCompany().getPurchaseTerm(),
            businessPartner.getCompany().getInventoryFulfillment());
    if (Constants.MERCHANT_TYPE_TD.equalsIgnoreCase(merchantType)) {
      productLevel3DetailWebResponse.setCogsViewable(true);
    }
    return productLevel3DetailWebResponse;
  }

  @Override
  public InventorySummaryWebResponse getInventorySummary(String itemSku, boolean isWareHouse, String merchantCode)
      throws Exception {
    InventorySummaryWebResponse response = new InventorySummaryWebResponse();
    InventoryDetailInfoRequestDTO infoRequestDTO = new InventoryDetailInfoRequestDTO();
    getPickupPointCodeAndSetInRequest(itemSku, infoRequestDTO);
    infoRequestDTO.setWebMerchantCode(merchantCode);
    infoRequestDTO.setWebItemSku(itemSku);
    GdnRestListResponse<InventoryDetailInfoResponseDTO> inventoryInfoResponseListResponse =
        xInventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Arrays.asList(infoRequestDTO)));
    ResponseHelper.validateResponse(inventoryInfoResponseListResponse);
    GdnRestSingleResponse<ReservedStockSummaryResponse> reservedStockResponse =
        xInventoryFeign.reservedStockSummaryByWebSKU(itemSku, isWareHouse);
    ResponseHelper.validateResponse(reservedStockResponse);
    if (CollectionUtils.isNotEmpty(inventoryInfoResponseListResponse.getContent())) {
      response = ResponseHelper
          .getInventorySummary(inventoryInfoResponseListResponse.getContent().get(0), reservedStockResponse.getValue(),
              isWareHouse);
    }
    return response;
  }

  private void getPickupPointCodeAndSetInRequest(String itemSku, InventoryDetailInfoRequestDTO infoRequestDTO) {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemsResponse =
        xProductFeign.getItemPickupPointCodeByItemSkus(new SimpleListStringRequest(Arrays.asList(itemSku)));
    ResponseHelper.validateResponse(itemsResponse);
    Map<String, List<ItemSkuPickupPointCodeResponse>> itemSkuPPCodeMap =
        itemsResponse.getContent().stream().collect(Collectors.groupingBy(ItemSkuPickupPointCodeResponse::getItemSku));
    if (itemSkuPPCodeMap.containsKey(itemSku) && itemSkuPPCodeMap.get(itemSku).size() == 1) {
      infoRequestDTO.setPickupPointCode(itemSkuPPCodeMap.get(itemSku).get(0).getPickupPointCode());
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND);
    }
  }

  @Override
  public ProductL3CountWebResponse getL3ProductCounts(String type, String merchantCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(merchantCode), ErrorMessages.ERR_MER_CODE_NULL);
    log.info("Get the l3 product counts for the type: {} and merchantCode : {}", type, merchantCode);
    if (ACTIVE_STATUS.equalsIgnoreCase(type)) {
      return ResponseHelper
          .toProductL3CountWebResponse(getActiveProducts(type, merchantCode), getInReviewProducts(merchantCode),
              merchantCode);
    } else if (INACTIVE_STATUS.equalsIgnoreCase(type)) {
      GdnRestSingleResponse<CountProductLevel3InactiveResponse> inActiveProductCount =
          pbpFeign.getInActiveProductCount(merchantCode);
      ResponseHelper.validateResponse(inActiveProductCount);
      return ResponseHelper
          .toProductL3CountWebResponse(getActiveProducts(type, merchantCode), getInReviewProducts(merchantCode),
              inActiveProductCount.getValue(), merchantCode);
    } else {
      log.error("Not a valid parameter type for L3 product counts : {}", type);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_REQUEST);
    }
  }

  private ProductCountResponse getActiveProducts(String type, String merchantCode) {
    GdnRestSingleResponse<ProductCountResponse> productCountResponse =
        xProductFeign.getProductCountByType(type, merchantCode);
    ResponseHelper.validateResponse(productCountResponse);
    return productCountResponse.getValue();
  }

  private CountProductLevel3WipResponse getInReviewProducts(String merchantCode) {
    GdnRestSingleResponse<CountProductLevel3WipResponse> inReviewProductResponse =
        pbpFeign.getInProgressProductCount(merchantCode);
    ResponseHelper.validateResponse(inReviewProductResponse);
    return inReviewProductResponse.getValue();
  }

  @Override
  public void updateItemListing(String productSku, List<QuickEditWebRequest> quickEditWebRequests) throws Exception {
    RequestHelper.validateAccessibilityForProductTab(validateProductAccessibility, Arrays.asList(Credential.getAccessibilities()),
        Boolean.parseBoolean(mandatoryParameterHelper.isExternal()), productAccessibilityList,
        mandatoryParameterHelper.getClientType());
    List<QuickEditRequest> quickEditRequests = RequestHelper.toQuickEditRequestList(quickEditWebRequests);
    GdnBaseRestResponse response =
        pbpFeign.itemListingUpdate(new ProductLevel3QuickEditRequest(quickEditRequests), productSku);
    ResponseHelper.validateResponseForErrorCode(response);
  }

  @Override
  public void toggleArchiveProducts(List<String> productSkus, boolean doArchive, String businessPartnerCode) {
    for (String productSku : productSkus) {
      if (StringUtils.isNotBlank(productSku)) {
        GdnPreconditions.checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
        log.info("Performing archive = {} for productSku {}", doArchive, productSku);
        SimpleListStringRequest request = new SimpleListStringRequest();
        request.setValue(Arrays.asList(productSku));
        GdnRestSingleResponse<ItemBulkArchiveResponse> response = pbpFeign.archiveProducts(doArchive, request);
        ResponseHelper.validateResponse(response);
        ItemBulkArchiveResponse itemBulkArchiveResponse = response.getValue();
        if (CollectionUtils.isNotEmpty(itemBulkArchiveResponse.getFailedItemSkus()) && itemBulkArchiveResponse
            .getFailedItemSkus().contains(productSku)) {
          throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
              ErrorMessages.ARCHIVAL_FAILED + itemBulkArchiveResponse.getFailedItemSkus());
        }
      }
    }
  }

  @Override
  public Page<ProductLevel3ListingWebResponse> getProductL3List(ProductSummaryWebRequest productSummaryRequest,
      int page, int size) {
    List<ProductLevel3ListingWebResponse> listingWebResponses = new ArrayList<>();
    GdnRestListResponse<ProductL3SummaryResponse> productL3SummaryResponse =
        xProductFeign.getFilterSummaryL3(page, size, false,
            RequestHelper.toProductSummaryRequest(productSummaryRequest));
    ResponseHelper.validateResponse(productL3SummaryResponse);
    Map<String, InventoryStockInfoDTO> productSkuInventoryMap = new HashMap<>();
    Map<String, InventoryDetailInfoResponseDTO> itemInventoryMapL4 = new HashMap<>();
    Map<String, CampaignPriceSkuResponse> itemCampaignMap = new HashMap<>();
    Map<String, String> skuXSuspensionReasonMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(productL3SummaryResponse.getContent())) {
      Map<String, String> skuXCategoryName = this.toProductSkuCategoryMap(productL3SummaryResponse.getContent());
      if (Boolean.TRUE.equals(productSummaryRequest.getSuspended())) {
        List<String> productSkuList =
            productL3SummaryResponse.getContent().stream().map(ProductL3SummaryResponse::getProductSku)
                .collect(Collectors.toList());
        skuXSuspensionReasonMap.putAll(this.toProductSkuSuspensionReasonMap(productSkuList));
      } else {
        GdnRestSingleResponse<ProductSystemParameterResponse> l3StockSwitch =
            pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK);
        ResponseHelper.validateResponse(l3StockSwitch);
        if (Boolean.parseBoolean(l3StockSwitch.getValue().getValue())) {
          List<String> skusForL3Stock = productL3SummaryResponse.getContent().stream()
              .filter(productSummary -> (productSummary.getL5Count() != Constants.NO_VARIANTS_COUNT))
              .map(ProductL3SummaryResponse::getProductSku).collect(Collectors.toList());
          productSkuInventoryMap.putAll(toL3InventoryMap(skusForL3Stock, productSummaryRequest.getMerchantCode()));
        }
        this.toL4InventoryAndCampaignMaps(productL3SummaryResponse.getContent(), itemInventoryMapL4, itemCampaignMap);
      }
      listingWebResponses = productL3SummaryResponse.getContent().stream().map(
          response -> ResponseHelper.toProductLevel3ListingWebResponse(response, skuXCategoryName,
            skuXSuspensionReasonMap, productSkuInventoryMap, itemInventoryMapL4, itemCampaignMap,
            RequestHelper.toProductDetailPage(response.getProductSku(), productDetailPageUrlPrefix)))
        .collect(Collectors.toList());
    }
    return new PageImpl<>(listingWebResponses, PageRequest.of(page, size),
        productL3SummaryResponse.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<ProductLevel3SummaryWebResponse> getItemsByProductSku(String productSku, String businessPartnerCode,
      PageRequest pageRequest) {
    GdnPreconditions.checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    log.info("Fetching the items for product sku : {}", productSku);
    GdnRestListResponse<ProductLevel3SummaryResponse> gdnRestListResponse = pbpFeign
        .filterSummary(businessPartnerCode, pageRequest.getPageNumber(), pageRequest.getPageSize(), ORDERBY, SORTBY,
            RequestHelper.toProductLevel3SummaryRequest(productSku));
    ResponseHelper.validateResponse(gdnRestListResponse);
    List<ProductLevel3SummaryWebResponse> productLevel3SummaryWebResponseList =
        gdnRestListResponse.getContent().stream().map(ResponseHelper::toProductLevel3SummaryWebResponse)
            .collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(productLevel3SummaryWebResponseList)) {
      GdnRestSingleResponse<ProductCampaignAvailabilityResponse> campaignAvailabilityResponse =
          this.getCampaignAvailabilityResponse(productLevel3SummaryWebResponseList, businessPartnerCode);
      ResponseHelper.validateResponse(campaignAvailabilityResponse);
      this.setProductCampaignAvailabilityFlagForListing(productLevel3SummaryWebResponseList,
          campaignAvailabilityResponse.getValue());
      List<CampaignPriceSkuRequest> campaignPriceSkuRequests = new ArrayList<>();
      for (ProductLevel3SummaryWebResponse productLevel3SummaryWebResponse : productLevel3SummaryWebResponseList) {
        CampaignPriceSkuRequest campaignPriceRequest =
            new CampaignPriceSkuRequest(productLevel3SummaryWebResponse.getItemSku(),
                productLevel3SummaryWebResponse.getCategoryCode(),
              productLevel3SummaryWebResponse.getPickupPointCode(), productLevel3SummaryWebResponse.getPrices().get(0).getSalePrice());
        campaignPriceSkuRequests.add(campaignPriceRequest);
      }
      Map<String, CampaignPriceSkuResponse> itemCampaignMap = new HashMap<>();
      GdnRestSingleResponse<CampaignPriceResponse> campaignL4;
      if (CollectionUtils.isNotEmpty(campaignPriceSkuRequests)) {
        campaignL4 = getCampaignPriceResponse(campaignPriceSkuRequests);
        itemCampaignMap.putAll(campaignL4.getValue().getItemSkuToPriceResponse());
      }
      productLevel3SummaryWebResponseList =
          ResponseHelper.getCampaignData(productLevel3SummaryWebResponseList, itemCampaignMap);
    }
    return new PageImpl<>(productLevel3SummaryWebResponseList, pageRequest,
        gdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  private GdnRestSingleResponse<CampaignPriceResponse> getCampaignPriceResponse(
      List<CampaignPriceSkuRequest> campaignPriceSkuRequests) {
    GdnRestSingleResponse<CampaignPriceResponse> campaignL4;
    if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
      campaignL4 = xCampaignFeign.getCampaignPriceInfoV2(
          CampaignPriceRequest.builder().campaignPriceSkuRequestList(campaignPriceSkuRequests).build());
      ResponseHelper.validateResponse(campaignL4);
      ResponseHelper.modifyCampaignPriceResponse(campaignL4.getValue());
    } else {
      campaignL4 = xCampaignFeign.getCampaignPriceInfo(
          CampaignPriceRequest.builder().campaignPriceSkuRequestList(campaignPriceSkuRequests).build());
      ResponseHelper.validateResponse(campaignL4);
    }
    return campaignL4;
  }

  private Map<String, String> toProductSkuCategoryMap(List<ProductL3SummaryResponse> summaryResponseList) {
    Map<String, String> categoryMap = new HashMap<>();
    Set<String> categoryCodeSet = summaryResponseList.stream()
        .filter(productL3SummaryResponse -> Objects.nonNull(productL3SummaryResponse.getCategoryCode()))
        .map(ProductL3SummaryResponse::getCategoryCode).collect(Collectors.toSet());
    CategoryMultipleIdRequest categoryNameRequest = new CategoryMultipleIdRequest();
    categoryNameRequest.setCategoryCode(new ArrayList<>(categoryCodeSet));
    GdnRestSingleResponse<CategoryNamesResponse> categoryResponse =
        pcbFeign.getCategoryNames(categoryNameRequest, 0, 50);
    ResponseHelper.validateResponse(categoryResponse);
    summaryResponseList.forEach(productResponse -> categoryMap.put(productResponse.getProductSku(),
        categoryResponse.getValue().getCategoryMap().get(productResponse.getCategoryCode())));
    return categoryMap;
  }

  private Map<String, String> toProductSkuSuspensionReasonMap (List<String> productSkuList) {
    GdnRestListResponse<ProductSuspensionHistoryResponse> suspensionResponseList =
        pbpFeign.fetchProductSuspensionHistory(new ProductSkuListRequest(productSkuList));
    ResponseHelper.validateResponse(suspensionResponseList);
    return suspensionResponseList.getContent().stream().collect(
        Collectors.toMap(ProductSuspensionHistoryResponse::getProductSku, ProductSuspensionHistoryResponse::getReason));
  }

  private Map<String, InventoryStockInfoDTO> toL3InventoryMap(List<String> productSkuList, String merchantCode) {
    List<List<String>> productSkuPartition = Lists.partition(productSkuList, inventoryApiBatchSize);
    List<InventoryStockInfoDTO> inventoryDTOList = new ArrayList<>();
    for (List<String> productSkus : productSkuPartition) {
      GdnRestListResponse<InventoryStockInfoDTO> inventoryL3Response =
          xInventoryFeign.findDetailByWebProductSkus(!Boolean.parseBoolean(mandatoryParameterHelper.isExternalOnly()), new
              InventoryDetailStockInfoRequestDTO(merchantCode, productSkus, new ArrayList<>()));
      ResponseHelper.validateResponse(inventoryL3Response);
      inventoryDTOList.addAll(inventoryL3Response.getContent());
    }
    return inventoryDTOList.stream()
        .collect(Collectors.toMap(InventoryStockInfoDTO::getWebProductSku, Function.identity()));
  }

  private void toL4InventoryAndCampaignMaps(List<ProductL3SummaryResponse> productSummaryList,
      Map<String, InventoryDetailInfoResponseDTO> itemInventoryMapL4,
      Map<String, CampaignPriceSkuResponse> itemCampaignMap) {
    List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestList = new ArrayList<>();
    List<CampaignPriceSkuRequest> campaignPriceSkuRequests = new ArrayList<>();
    for (ProductL3SummaryResponse productSummary : productSummaryList) {
      if (productSummary.getVariantCount() == Constants.NO_VARIANTS_COUNT && Objects
          .nonNull(productSummary.getItemL4SummaryResponse())) {
        InventoryDetailInfoRequestDTO inventoryRequest = new InventoryDetailInfoRequestDTO();
        getPickupPointCodeAndSetInRequest(productSummary.getItemL4SummaryResponse().getItemSku(), inventoryRequest);
        inventoryRequest.setWebItemSku(productSummary.getItemL4SummaryResponse().getItemSku());
        inventoryRequest.setWebMerchantCode(productSummary.getMerchantCode());
        inventoryDetailInfoRequestList.add(inventoryRequest);
        CampaignPriceSkuRequest campaignPriceRequest =
          new CampaignPriceSkuRequest(productSummary.getItemL4SummaryResponse().getItemSku(),
            productSummary.getCategoryCode(),
            productSummary.getItemL4SummaryResponse().getPickupPointCode(), productSummary.getItemL4SummaryResponse().getPrice()
              .stream().findFirst().orElse(new PriceDTO()).getOfferPrice());
        campaignPriceSkuRequests.add(campaignPriceRequest);
      }
    }
    if (CollectionUtils.isNotEmpty(inventoryDetailInfoRequestList)) {
      GdnRestListResponse<InventoryDetailInfoResponseDTO> inventoryL4 = xInventoryFeign
          .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(inventoryDetailInfoRequestList));
      ResponseHelper.validateResponse(inventoryL4);
      itemInventoryMapL4.putAll(inventoryL4.getContent().stream()
          .collect(Collectors.toMap(InventoryDetailInfoResponseDTO::getWebItemSku, Function.identity())));
    }
    if (CollectionUtils.isNotEmpty(campaignPriceSkuRequests)) {
      GdnRestSingleResponse<CampaignPriceResponse> campaignL4 = getCampaignPriceResponse(campaignPriceSkuRequests);
      ResponseHelper.validateResponse(campaignL4);
      itemCampaignMap.putAll(campaignL4.getValue().getItemSkuToPriceResponse());

    }
  }

  @Override
  public VendorNotesResponse getVendorNotes(String productCode) {
    GdnRestSimpleResponse<VendorNotesResponse> response = pbpFeign.getVendorNotes(productCode);
    ResponseHelper.validateResponse(response);
    VendorNotesResponse vendorNotesResponse = response.getValue();
    if (CollectionUtils.isEmpty(vendorNotesResponse.getMerchantModifiedFields())) {
      vendorNotesResponse.setMerchantModifiedFields(new ArrayList<>());
    }
    return vendorNotesResponse;
  }

  @Override
  public void updateVendorNotes(String productCode,
      VendorNotesRequest vendorNotesRequest) {
    GdnBaseRestResponse response = pbpFeign.updateVendorNotes(productCode, vendorNotesRequest);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public EditProductWebResponse submitNeedForRevisionProduct(
      NeedRevisionSubmitWebRequest needRevisionSubmitWebRequest) {
    GdnRestSingleResponse<EditProductV2Response> editProductResponse =
        pbpFeign.submitNeedRevisionProduct(RequestHelper.toNeedRevisionSubmitRequest(needRevisionSubmitWebRequest));
    return ResponseHelper.validateEditInfoResponse(editProductResponse);
  }

  @Override
  public boolean appealProductsInProgress(AppealProductWebRequest appealProductWebRequest,
      String businessPartnerCode) {
    GdnRestSingleResponse<AppealProductResponse> updateAppealInProgressProduct =
        pbpFeign.updateAppealInProgressProduct(
            RequestHelper.toAppealProductRequest(appealProductWebRequest, businessPartnerCode));
    return ResponseHelper.validateAppealProductResponse(updateAppealInProgressProduct);
  }

  @Override
  public EditProductWebResponse updateProductInfo(UpdateProductLevel3InfoRequest request, String businessPartnerCode,
      boolean isOnlyExternal, String productSku) throws Exception {
    ProfileResponse businessPartner = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    if (Objects.isNull(businessPartner) || !ACTIVE.equals(businessPartner.getMerchantStatus())) {
      log.error("Business partner not active productSku : {}", productSku);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.BUSINESS_PARTNER_NOT_ACTIVE);
    }
    if (Objects.nonNull(request.getProductType())) {
      List<Integer> productType =
          Arrays.stream(ProductType.values()).map(productTypeCode -> productTypeCode.getCode()).collect(Collectors.toList());
      if (!productType.contains(request.getProductType())) {
        log.error("Product Type is not valid for productSku : {}, ProductType : {}", productSku,
            request.getProductType());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.ERROR_INVALID_PRODUCT_TYPE);
      }
    }
    if (StringUtils.isNotEmpty(request.getVideoUrl())) {
      YouTubeAPIWebResponse youTubeAPIWebResponse = this.validateYouTubeUrl(request.getVideoUrl());
      if (!youTubeAPIWebResponse.isValid()) {
        log.error("youtube url invalid : {} productSku :{}",request.getVideoUrl(), productSku);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.ERROR_INVALID_YOUTUBE_URL);
      }
    }
    request.setBusinessPartnerCode(businessPartnerCode);
    request.setOnlyExternal(isOnlyExternal);
    GdnRestSingleResponse<EditProductV2Response> response = pbpFeign.updateProductInfo(request, productSku);
    return ResponseHelper.validateEditInfoResponse(response);
  }

  @Override
  public Map<String, String> fetchItemNamesByItemSku(List<String> itemSkus) {
    GdnRestSingleResponse<SimpleMapStringResponse> response =
        xProductFeign.getItemNameByItemSkus(new SimpleListStringRequest(itemSkus));
    ResponseHelper.validateResponse(response);
    return response.getValue().getValue();
  }

  @Override
  public ItemsPriceStockImagesUpdateWebResponse updateImages(String productSku, String businessPartnerCode,
      ProductImageEditWebRequest productImageEditWebRequest) throws Exception {
    log.info("Updating images for product sku : {} request : {}", productSku, productImageEditWebRequest);
    if (StringUtils.isNotBlank(productImageEditWebRequest.getImageData()) && (StringUtils.isNotBlank(
        productImageEditWebRequest.getImagePath()))) {
      log.error("Cannot support both image path and image data for product sku: {}", productSku);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_REQUEST);
    }
    ProductImageEditRequest productImageEditRequest =
        RequestHelper.toProductImageEditRequest(productImageEditWebRequest, productSku);
    if (StringUtils.isNotBlank(productImageEditWebRequest.getImageData())) {
      String image = productImageEditWebRequest.getImageData();
      InputStream inputStream = imageService.getImageInputStream(image);
      if (Objects.isNull(inputStream)) {
        log.error("Error while getting image from image data for product Sku : {}", productSku);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.IMAGE_READ_ERROR);
      }
      GdnRestSingleResponse<ProductLevel3DetailResponse> response = pbpFeign.getL3DetailByProductSku(productSku, false);
      ResponseHelper.validateResponse(response);
      ProductLevel3DetailResponse productLevel3DetailResponse = response.getValue();
      String productCode = productLevel3DetailResponse.getProductCode();
      String productName = productLevel3DetailResponse.getProductName();
      String id = UUID.randomUUID().toString().substring(0, 8);
      String filename = productName + Constants.HYPHEN + Constants.FULL_PREFIX + Constants.HYPHEN + id;
      filename = filename.toLowerCase().replaceAll("[^a-zA-Z0-9]+", "_");
      String path = fileStorageService.generatePath(productCode, filename);
      String imageFilePath = productCode + Constants.ROOT + filename + Constants.JPEG;
      String originalFileType = Constants.JPEG;
      if (imageFormatsSupported.contains(Constants.WEBP_FORMAT)) {
        path = path.replace(Constants.JPEG, Constants.WEBP_EXTENSION);
        imageFilePath = imageFilePath.replace(Constants.JPEG, Constants.WEBP_EXTENSION);
        originalFileType = Constants.WEBP_FORMAT;
      }
      UploadImageRequest request =
          new UploadImageRequest((imageFilePath).replaceAll("//", "/"), productCode,
              IOUtils.toByteArray(inputStream), true, false, originalFileType);
      boolean success = this.imageService.uploadImage(request);
      if (!success) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.IMAGE_UPLOAD_ERROR);
      }
      productImageEditRequest.setImagePath(path);
      productImageEditRequest.setProductCode(productCode);
    }
    boolean success = this.imageService.imageExistsAndValid(productImageEditRequest.getImagePath());
    if (!success) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_REQUEST);
    }
    productImageEditRequest.setBusinessPartnerCode(businessPartnerCode);
    GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> variantResponse =
        pbpFeign.updateImages(productImageEditRequest);
    ResponseHelper.validateResponse(variantResponse);
    return ResponseHelper.toItemsPriceStockImagesUpdateWebResponse(variantResponse.getValue());
  }

  @Override
  public Page<ItemLevel4ListingWebResponse> getL4ItemListByProductSku(String storeId,
    String requestId, int page, Integer size, ItemLevel4WebRequest request) {
    log.info("Fetching the active Item Summary Level 4 Paginated List for ProductSkus : {}",
      request);
    GdnRestListResponse<ProductBundleRecipeEditableResponse> bundleRecipeEditableInfoByItemSkus =
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 0, 0), null);
    if (request.isNeedRevision()) {
      GdnRestListResponse<ItemSummaryL4Response> l4ProductDetailsByProductSku =
          pbpFeign.getL4ProductDetailsByProductSku(request.getProductSkus().iterator().next(), page, size);
      ResponseHelper.validateResponse(l4ProductDetailsByProductSku);
      Set<String> itemCodeSet =
          Optional.ofNullable(l4ProductDetailsByProductSku.getContent()).orElse(new ArrayList<>()).stream()
              .filter(Objects::nonNull).map(ItemSummaryL4Response::getItemCode).collect(Collectors.toSet());
      if (productBundlingEnabled) {
        bundleRecipeEditableInfoByItemSkus =
            productAssemblyFeign.getBundleRecipeEditableInfoByItemCodes(new ArrayList<>(itemCodeSet));
      }
      List<ItemLevel4ListingWebResponse> webResponse =
          ResponseHelper.fromItemSummaryL4ResponseToItemLevel4ListingWebResponse(l4ProductDetailsByProductSku,
              bundleRecipeEditableInfoByItemSkus);
      return new PageImpl<>(webResponse, PageRequest.of(page, webResponse.size()),
          l4ProductDetailsByProductSku.getPageMetaData().getTotalRecords());
    }
    ItemLevel4ListingWebRequest webRequest = RequestHelper.toItemLevel4WebRequest(request);
    log.info("Web Request for x-product feign API call is : {}", webRequest);
    GdnRestListResponse<ItemLevel4ListingResponse> l4ItemListByProductSku =
        xProductFeign.getL4ItemListByProductSku(storeId, requestId, page, size, webRequest);
    ResponseHelper.validateResponse(l4ItemListByProductSku);
    log.info("L4 Item Summary web Request was successfully fetched from x-product request : ",
      request);
    if (CollectionUtils.isEmpty(l4ItemListByProductSku.getContent())) {
      log.info("Contents of Item Response fetched was found to be Empty");
    } else {
      log.error("Item Summary response fetched from x-product was found to be null");
    }
    Set<String> itemCodeSet =
        Optional.ofNullable(l4ItemListByProductSku.getContent()).orElse(new ArrayList<>()).stream()
            .filter(Objects::nonNull).map(ItemLevel4ListingResponse::getItemCode).collect(Collectors.toSet());
    if (productBundlingEnabled) {
      bundleRecipeEditableInfoByItemSkus =
          productAssemblyFeign.getBundleRecipeEditableInfoByItemCodes(new ArrayList<>(itemCodeSet));
    }
    List<ItemLevel4ListingWebResponse> webResponse =
      ResponseHelper.toItemLevel4ListingWebResponse(l4ItemListByProductSku, bundleRecipeEditableInfoByItemSkus,  productDetailPageUrlPrefix);
    return new PageImpl<>(webResponse, PageRequest.of(page, webResponse.size()),
        l4ItemListByProductSku.getPageMetaData().getTotalRecords());
  }


  @Override
  public Page<HistoryUpdateWebResponse> getProductUpdateHistory(
    HistoryUpdateWebRequest historyUpdateWebRequest, int page, int size) throws Exception {
    HistoryUpdateRequest historyUpdateRequest = new HistoryUpdateRequest();
    BeanUtils.copyProperties(historyUpdateWebRequest, historyUpdateRequest);
    RequestHelper.setTimeFilterForHistoryRequest(historyUpdateRequest);
    GdnRestListResponse<HistoryUpdateResponse> response =
      this.pbpFeign.getProductUpdateHistory(page, size, historyUpdateRequest);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toHistoryUpdateWebResponseList(response.getContent()),
      PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<ItemPickupPointListingL3WebResponse> getItemPickupPointListingByProductSku(int page, int size,
      boolean onlyDefaultViewConfig, String productSku,
      ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest, boolean concatenateValueWithValueType) {
    itemPickupPointListingL3WebRequest.setPickupPointCodes(
      userPicService.filterInaccessiblePickupPoints(null,
        itemPickupPointListingL3WebRequest.getPickupPointCodes()));
    GdnRestListResponse<ItemPickupPointListingL3Response> response =
        pbpFeign.getItemPickupPointL3Listing(page, size, onlyDefaultViewConfig, concatenateValueWithValueType,
            RequestHelper.toItemPickupPointListingL3Request(productSku, itemPickupPointListingL3WebRequest));
    Map<String, Integer> itemSkuXinProgressCFMap = new HashMap<>();
    ResponseHelper.validateResponse(response);
    if(cfCountAtListingEnabled) {
      itemSkuXinProgressCFMap = fetchInProgressConsignmentCount(response);
    }
    if (!itemPickupPointListingL3WebRequest.isNeedCorrection() && resizeImageRemoval) {
      List<String> pathsToRemove =
          Arrays.asList(resizeImagePathList.split(Constants.COMMA_DELIMITER_NO_SPACE));
      Optional.ofNullable(response).map(GdnRestListResponse::getContent).orElseGet(ArrayList::new)
          .stream().filter(Objects::nonNull).forEach(
              itemPickupPointListingL3Response -> Optional.of(itemPickupPointListingL3Response)
                  .map(ItemPickupPointListingL3Response::getImages)
                  .ifPresent(imageResponses -> imageResponses.removeIf(isEligibleForRemoval(pathsToRemove))));
    }
    if (!itemPickupPointListingL3WebRequest.isNeedCorrection() && overrideImageActiveFlagForL5Listing) {
      response.getContent().stream().filter(item -> Objects.nonNull(item.getImages()))
          .forEach(item -> item.getImages().forEach(imageResponse -> imageResponse.setActiveLocation(true)));
    }
    return new PageImpl<>(
        ResponseHelper.toItemPickupPointListingL3WebResponse(response.getContent(), itemSkuXinProgressCFMap,
            baseUrlForPdpWithL5, populateLabelForPwpPromo), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  private Predicate<ImageResponse> isEligibleForRemoval(List<String> pathsToRemove) {
    return imageResponse -> Objects.isNull(imageResponse) || containsAnyPath(imageResponse,
        pathsToRemove);
  }

  private boolean containsAnyPath(ImageResponse imageResponse, List<String> pathsToRemove) {
    String locationPath = Optional.ofNullable(imageResponse.getLocationPath()).orElse(StringUtils.EMPTY);
    return pathsToRemove.stream().anyMatch(locationPath::contains) && !imageResponse.isActiveLocation();
  }

  private Map<String, Integer> fetchInProgressConsignmentCount(
    GdnRestListResponse<ItemPickupPointListingL3Response> response) {
    List<ItemPickupPointListingL3Response> content = response.getContent();
    if (content.isEmpty()) {
      return Collections.emptyMap();
    }

    List<String> fbbActiveL4s = content.stream().filter(
        item -> Optional.ofNullable(item).filter(ItemPickupPointListingL3Response::isFbbActivated)
          .isPresent()).map(ItemPickupPointListingL3Response::getItemSku).filter(Objects::nonNull)
      .collect(Collectors.toList());

    if(CollectionUtils.isNotEmpty(fbbActiveL4s)) {
      Response<List<CountConsignmentFormsByItemSkuResponse>> inProgressConsignmentForms =
        fbbFeign.countInProgressConsignmentFormsByItemSkus(
          CountConsignmentFormsByItemSkusRequest.builder().itemSkus(fbbActiveL4s)
            .businessPartnerCode(content.get(0).getMerchantCode()).build(), headerAuthenticatorFbb);

    if (!ResponseHelper.validateResponseForConsignmentCount(inProgressConsignmentForms)) {
      return Collections.emptyMap();
    }

    return inProgressConsignmentForms.getData().stream().collect(
        Collectors.toMap(CountConsignmentFormsByItemSkuResponse::getItemSku,
          CountConsignmentFormsByItemSkuResponse::getTotal,
          (existingCFCount, newCFCount) -> newCFCount));
    }
    return Collections.emptyMap();
  }

  @Override
  public List<BusinessPartnerPickupPointWebResponse> getPickupDetailByCode(
    List<String> pickupPointCodes, String merchantCode) throws Exception {
    log.info("Fetching Pickup point details by pickup point codes {}", pickupPointCodes);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(pickupPointCodes),
      "PickupPoints cannot be empty");
    List<List<String>> partition = Lists.partition(pickupPointCodes, pickupPointCodeBatchSize);
    List<BusinessPartnerPickupPointWebResponse> webResponse = new ArrayList<>();
    for (List<String> partitionedRequest : partition) {
      SimpleListStringRequest request = new SimpleListStringRequest();
      request.setValue(partitionedRequest);
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(partitionedRequest),
        "Partition list  cannot be empty");
      GdnRestListResponse<BusinessPartnerPickupPointOutboundResponse> response =
          this.xProductFeign.getPickupDetailByCodes(request, merchantCode);
      ResponseHelper.validateResponse(response);
      log.info("Business Partner PP Response from x-Product was successfully fetched : {}",
        response);
      webResponse.addAll(ResponseHelper.toBusinessPartnerPickupPointWebResponse(response.getContent()));
    }
    return webResponse;
  }

  @Override
  public ItemsPriceStockImagesUpdateWebResponse editL5ProductStockAndPrice(
      ProductVariantUpdateWebRequest productVariantUpdateWebRequest, String businessPartnerCode)
      throws Exception {
    RequestHelper.validateProductEditAccessibility(validateProductEditAccessibility,
        validateProductEditAccessibilityExclusionList, mandatoryParameterHelper.getChannelId());
    Set<String> pickupPoints = RequestHelper.validateModifiedPickupPoints(
        productVariantUpdateWebRequest.getProductItems());
    if (CollectionUtils.isNotEmpty(pickupPoints)) {
      userPicService.validateUserPicPickupPoints(pickupPoints);
    }
    ProductVariantUpdateRequest productVariantUpdateRequest =
        RequestHelper.toProductVariantUpdateRequest(productVariantUpdateWebRequest, businessPartnerCode);
    GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> response =
        pbpFeign.editL5PriceStockInfo(productVariantUpdateRequest);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.getItemsPriceStockImagesUpdateWebResponse(response.getValue());
  }

  private Map<String, List<PredefinedAllowedAttributeValueResponse>> getAllInReviewBrands(
    String storeId, String requestId, String businessPartnerCode) {
    List<BrandInReviewResponse> response =
      brandService.getAllInReviewBrand(storeId, requestId);
    Map<String, List<PredefinedAllowedAttributeValueResponse>> inReviewBrandMap = new HashMap<>();
    for (BrandInReviewResponse brandInReviewResponse : response) {
      PredefinedAllowedAttributeValueResponse valueResponse =
        toPreDefinedAllowedAttributeValue(brandInReviewResponse);
      if (brandInReviewResponse.getBusinessPartnerCode().equalsIgnoreCase(businessPartnerCode)) {
        List<PredefinedAllowedAttributeValueResponse> existingList =
          inReviewBrandMap.getOrDefault(businessPartnerCode, new ArrayList<>());
        existingList.add(valueResponse);
        inReviewBrandMap.put(businessPartnerCode, existingList);
      } else if (brandInReviewResponse.getBusinessPartnerCode().equalsIgnoreCase(INTERNAL)) {
        List<PredefinedAllowedAttributeValueResponse> existingList =
          inReviewBrandMap.getOrDefault(INTERNAL, new ArrayList<>());
        existingList.add(valueResponse);
        inReviewBrandMap.put(INTERNAL, existingList);
      } else {
        List<PredefinedAllowedAttributeValueResponse> existingList =
          inReviewBrandMap.getOrDefault(OTHERS, new ArrayList<>());
        existingList.add(valueResponse);
        inReviewBrandMap.put(OTHERS, existingList);
      }
    }
    return inReviewBrandMap;
  }

  private static PredefinedAllowedAttributeValueResponse toPreDefinedAllowedAttributeValue(
    BrandInReviewResponse response){
    PredefinedAllowedAttributeValueResponse valueResponse =
      new PredefinedAllowedAttributeValueResponse();
    valueResponse.setPredefinedAllowedAttributeCode(response.getBrandRequestCode());
    valueResponse.setValue(response.getBrandName());
    valueResponse.setBrandApprovalStatus(response.getState().name());
    valueResponse.setProtectedBrand(response.isProtectedBrand());
    return valueResponse;
  }

  @Override
  public TemplateDownloadFilePathWebResponse getExternalDownloadTemplateFilePaths() {
    TemplateDownloadFilePathWebResponse templateDownloadFilePathWebResponse = new TemplateDownloadFilePathWebResponse();
    templateDownloadFilePathWebResponse.setInternalTemplateDownloadFilePathMap(
        fileStorageService.getExternalDownloadTemplateFilePaths());
    return templateDownloadFilePathWebResponse;
  }

  @Override
  public List<ItemDetailWebResponse> getItemBasicDetails(String productSku, boolean fetchAllDetails) {
    if (validateSellerItemBasicDetails) {
      checkIfSellerIsEligibleToFetchData(productSku);
    }
    GdnRestListResponse<ItemBasicDetailV2Response> itemBasicDetails = xProductFeign.getItemBasicDetails(productSku);
    ResponseHelper.validateResponse(itemBasicDetails);
    if (validateSellerItemBasicDetails) {
      checkIfFetchedDataIsAuthorizedToBeReturned(productSku, itemBasicDetails);
    }
    List<String> skuCodes =
        itemBasicDetails.getContent().stream().map(ItemBasicDetailV2Response::getItemCode).collect(Collectors.toList());
    Map<String, List<ItemAttributeWebResponse>> itemCodeToItemAttributeValuesMap = new HashMap<>();
    if (fetchAllDetails) {
      GdnRestListResponse<ProductItemResponse> completeItemResponse =
          pcbFeign.getItemDeatilBySkuCodes(new SkuCodesRequest(skuCodes));
      ResponseHelper.validateResponse(completeItemResponse);
      itemCodeToItemAttributeValuesMap = completeItemResponse.getContent().stream().collect(
          Collectors.toMap(ProductItemResponse::getSkuCode,
              productItemResponse -> ResponseHelper.toItemAttributeValueWebResponses(
                  productItemResponse.getProductItemAttributeValueResponses())));
    }
    return ResponseHelper.toItemDetailWebResponseFromV2Response(itemBasicDetails.getContent(),
        itemCodeToItemAttributeValuesMap, new HashMap<>(), productDetailPageUrlPrefix);
  }

  private void checkIfSellerIsEligibleToFetchData(String productSku) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    profileResponse = Optional.ofNullable(profileResponse).orElse(new ProfileResponse());
    if (!itemBasicDetailsValidationRelaxForSellerTypes.contains(Optional.ofNullable(
            Optional.ofNullable(profileResponse.getCompany()).orElse(new CompanyDTO()).getMerchantType())
        .orElse(StringUtils.EMPTY)) && !productSku.startsWith(businessPartnerCode)) {
      log.error("Seller {} of Type {} not authorized to access product {} ", businessPartnerCode,
          profileResponse.getCompany().getMerchantType(), productSku);
      throw new UnauthorizedException(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  public void checkIfFetchedDataIsAuthorizedToBeReturned(String productSku,
      GdnRestListResponse<ItemBasicDetailV2Response> itemBasicDetails) {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    if (CollectionUtils.isNotEmpty(itemBasicDetails.getContent()) && !Optional.ofNullable(
            itemBasicDetails.getContent().get(0).getMerchantCode()).orElse(StringUtils.EMPTY)
        .startsWith(businessPartnerCode)) {
      ProfileResponse profileResponse =
          businessPartnerService.filterByBusinessPartnerCode(itemBasicDetails.getContent().get(0).getMerchantCode());
      profileResponse = Optional.ofNullable(profileResponse).orElse(new ProfileResponse());
      if (!itemBasicDetailsValidationRelaxForSellerTypes.contains(Optional.ofNullable(
              Optional.ofNullable(profileResponse.getCompany()).orElse(new CompanyDTO()).getMerchantType())
          .orElse(StringUtils.EMPTY))) {
        log.error("Seller {} of Type {} not authorized to access product {} ", businessPartnerCode,
            profileResponse.getCompany().getMerchantType(), productSku);
        throw new UnauthorizedException(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
      }
    }
  }

  @Override
  public List<ItemDetailWebResponse> getItemBasicDetails(List<String> itemSkus, boolean fetchBundleRecipe)
      throws Exception {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ERR_ITEM_SKU_NULL);
    GdnPreconditions.checkArgument((itemSkus.size() < maxAllowedItemSkus), ErrorMessages.ERR_ITEM_SKU_LIMIT_EXCEED);
    checkProductSkuOrItemSkuStartsWithBPCode(itemSkus, validateBusinessPartnerCodeForSecurityEnabled,
        mandatoryParameterHelper.getBusinessPartnerCode());
    GdnRestListResponse<ItemBasicDetailV2Response> itemBasicDetails =
        xProductFeign.getItemBasicDetails(fetchBundleRecipe, new SimpleListStringRequest(itemSkus));
    ResponseHelper.validateResponse(itemBasicDetails);
    List<String> categoryCodeList =
        Optional.ofNullable(itemBasicDetails.getContent()).orElse(new ArrayList<>()).stream()
            .map(ItemBasicDetailV2Response::getCategoryCode).collect(Collectors.toList());
    List<CategoryWebResponse> categoryWebResponseList = categoryService.getCategoriesByCategoryCodes(categoryCodeList);
    Map<String, CategoryWebResponse> categoryWebResponseMap = categoryWebResponseList.stream().collect(
        Collectors.toMap(CategoryWebResponse::getCategoryCode, Function.identity(),
            (categoryResponse1, categoryResponse2) -> categoryResponse1));
    return ResponseHelper.toItemDetailWebResponseFromV2Response(itemBasicDetails.getContent(), new HashMap<>(),
        categoryWebResponseMap, productDetailPageUrlPrefix);
  }

  @Override
  public Page<ItemL5ListingResponse> getItemL5Details(String productSku, Integer page, Integer size,
      Boolean cncActivated, boolean fetchOnlyBundleVariants) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.INVALID_REQUEST);
    checkProductSkuOrItemSkuStartsWithBPCode(Collections.singletonList(productSku),
        validateBusinessPartnerCodeForSecurityEnabled, mandatoryParameterHelper.getBusinessPartnerCode());
    GdnRestListResponse<ItemL5ListingResponse> itemL5Response =
        xProductFeign.getItemL5Details(page, size, productSku, cncActivated, fetchOnlyBundleVariants,
            new SimpleListStringRequest());
    ResponseHelper.validateResponse(itemL5Response);
    return new PageImpl<>(itemL5Response.getContent(),
        PageRequest.of(Objects.nonNull(page) ? page : 0, itemL5Response.getContent().size()),
        itemL5Response.getPageMetaData().getTotalRecords());
  }

  @Override
  public List<InventoryWarehouseStockWebResponse> getWarehouseStockByItemSkusAndWarehouseCode(String warehouseCode,
      List<String> itemSkus){
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(warehouseCode), ErrorMessages.INVALID_WAREHOUSE_CODE);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ERR_ITEM_SKU_NULL);
    WarehouseInventoryStockInfoRequestDTO request = new WarehouseInventoryStockInfoRequestDTO();
    request.setWarehouseCodes(Collections.singletonList(warehouseCode));
    request.setWebItemSkus(itemSkus);
    GdnRestListResponse<WarehouseInventoryDetailResponseDTO> warehouseResponse;
    if (getStockForNonDistributedWareHouse) {
      warehouseResponse = xInventoryFeign.getStockForNonDistributedWareHouseByItemSkusAndWareHouseCodes(true,
          new ListRequestDTO<>(List.of(request)));
    } else {
      warehouseResponse = xInventoryFeign.findByWebItemSkusAndWarehouseCodes(new ListRequestDTO<>(List.of(request)));
    }
    List<WarehouseInventoryDetailResponseDTO> warehouseInventoryDetailResponseDTOList =
        Optional.ofNullable(warehouseResponse.getContent()).orElse(new ArrayList<>());
    return ResponseHelper.getWarehouseStockDetails(warehouseInventoryDetailResponseDTOList, itemSkus, warehouseCode);
  }

  @Override
  public void downloadWorkOrderTemplates(String type, HttpServletResponse response) throws Exception {
    Blob blob = gcsService.downloadFile(gcsProperties.getBucketName(),
        gcsProperties.getTemplateDirectory() + Constants.ROOT + type + XLSX);
    GdnRestListResponse<MasterWarehouseListWebResponse> masterWarehouseListWebResponseGdnRestListResponse =
        productAssemblyService.getMasterWarehouseListResponse();
    ResponseHelper.validateResponse(masterWarehouseListWebResponseGdnRestListResponse);
    List<String> wareHouseNamesCodesList = masterWarehouseListWebResponseGdnRestListResponse.getContent().stream()
        .map(warehouse -> warehouse.getWarehouseCode() + PIPE + warehouse.getWarehouseName())
        .collect(Collectors.toList());
    XSSFWorkbook workbook = ExcelTemplateUtil.dropDownCreation(blob, wareHouseNamesCodesList, type);
    ExcelTemplateUtil.generateFileTemplate(type, workbook, response);
  }

  @Override
  public AppealProductConfigResponse fetchAppealProductConfig(String storeId, String requestId,
    String businessPartnerCode) {
    GdnRestSingleResponse<AppealProductConfigResponse> updateAppealInProgressProduct =
      pbpFeign.getAppealProductConfig(storeId, requestId, businessPartnerCode);
    ResponseHelper.validateResponse(updateAppealInProgressProduct);
    return updateAppealInProgressProduct.getValue();
  }

  @Override
  public void submitEvidenceForIPR(SubmitEvidenceIPRRequest submitEvidenceIPRRequest) {
    GdnBaseRestResponse response =
        pdtFeign.submitEvidence(mandatoryParameterHelper.getRequestId(), submitEvidenceIPRRequest);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public List<UpcStatusWebResponse> getUpcStatus(UpcStatusWebRequest upcStatusWebRequest) {
    GdnRestListResponse<UpcStatusResponse> upcStatusResponseGdnRestListResponse =
        xProductFeign.getUpcStatus(new UpcStatusRequest(upcStatusWebRequest.getMerchantCode(),
            upcStatusWebRequest.getUpcCodes()));
    ResponseHelper.validateResponse(upcStatusResponseGdnRestListResponse);
    return upcStatusResponseGdnRestListResponse.getContent().stream().map(
        upcStatusResponse -> UpcStatusWebResponse.builder().upcCode(upcStatusResponse.getUpcCode())
            .exists(upcStatusResponse.isExists()).build()).collect(Collectors.toList());
  }

  @Override
  public void republishProductToAgp(String productSku) {
    GdnBaseRestResponse response = xProductFeign.republishProductsToAgp(Collections.singletonList(productSku));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void republishItemToAgp(String itemSku) {
    GdnBaseRestResponse response = xProductFeign.republishItemsToAgp(Collections.singletonList(itemSku));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void republishItemPickupPointToAgp(String itemSku, String pickupPointCode, boolean republishToAgp) {
    GdnBaseRestResponse response = xProductFeign.republishItemPickupPointToAgp(republishToAgp,
        Collections.singletonList(new ItemPickupPointRequest(itemSku, pickupPointCode)));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public List<ProductBasicWebResponse> getProductBasicDetailsByProductSkus(List<String> productSkus)
      throws Exception {
    GdnRestListResponse<ProductBasicWebResponse> response =
        xProductFeign.getProductBasicDetails(new SimpleListStringRequest(productSkus));
    ResponseHelper.validateResponse(response);
    return response.getContent();
  }

  @Override
  public void downloadAllProductBasicInfo(String username, String businessPartnerCode,
      ProductSummaryWebRequest request) {
    RequestHelper.validateBusinessPartnerCode(businessPartnerCode, request,
        validateBusinessPartnerCodeForSecurityEnabled);
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions
        .checkArgument(Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
            ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    Map<String, Boolean> privilegeMap = RequestHelper.getReadAccessibilities(profileResponse, false);
    String randomUUID = UUID.randomUUID().toString();
    String fileName = BULK_BASIC_INFO_FILE_NAME + Constants.DOT + FileType.XLSX.name().toLowerCase();
    ProductSummaryRequest productSummaryRequest = RequestHelper.toProductSummaryRequest(request);
    ProductBasicInfoDownloadRequest productBasicInfoDownloadRequest =
        RequestHelper.toProductBasicInfoDownloadRequest(username, businessPartnerCode, privilegeMap, randomUUID,
            fileName, productSummaryRequest);
    this.kafkaProducer.send(kafkaTopicProperties.getBulkDownloadAllEvent(), businessPartnerCode,
        productBasicInfoDownloadRequest);
  }

  @Override
  public void reindexBrandCollection(String brandRequestCode) {
    GdnBaseRestResponse response = pcbFeign.reindexBrandCollection(brandRequestCode);
    ResponseHelper.validateResponse(response);
  }
}
