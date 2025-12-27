package com.gdn.partners.pcu.external.service.impl;

import com.blibli.oss.common.response.Response;
import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gda.mta.product.dto.BulkDeleteProductWipRequest;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.NeedRevisionSubmitRequest;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemLevel3OrderResponse;
import com.gda.mta.product.dto.ProductItemLevel3Request;
import com.gda.mta.product.dto.ProductItemLevel3Response;
import com.gda.mta.product.dto.ProductLevel3ImageRequest;
import com.gda.mta.product.dto.ProductLevel3OrderResponse;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3QuickEditRequest;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3StockInfoWebSiteResponse;
import com.gda.mta.product.dto.ProductLevel3StockRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryCountResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductScoreResponse;
import com.gda.mta.product.dto.ProductSkuAndPickupPointCodeRequest;
import com.gda.mta.product.dto.ProductSkuListRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gda.mta.product.dto.SummaryFilterRequest;
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
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.fbb.core.web.model.request.CountConsignmentFormsByItemSkusRequest;
import com.gdn.fbb.core.web.model.response.v3.CountConsignmentFormsByItemSkuResponse;
import com.gdn.mta.bulk.dto.SystemParameterConfigResponse;
import com.gdn.mta.product.commons.constant.ProductLevel3InactiveSummaryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.commons.constant.UpdateProductAccessChannel;
import com.gdn.mta.product.entity.PickupPointCodeResponse;
import com.gdn.mta.product.entity.UniquePickupPointCodeResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pbp.dto.productcategory.CategoryHierarchyProductCountResponse;
import com.gdn.partners.pbp.dto.productlevel1.ProductSearchRequest;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.EstimateItemPriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.PostLiveProductCountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
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
import com.gdn.partners.pcu.external.client.helper.HitsResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.client.model.BusinessPartnerPickupPointOutboundResponse;
import com.gdn.partners.pcu.external.client.model.InventoryDetailStockInfoRequestDTO;
import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gdn.partners.pcu.external.client.model.SubmitEvidenceIPRRequest;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.properties.GCSProperties;
import com.gdn.partners.pcu.external.service.BrandService;
import com.gdn.partners.pcu.external.service.BulkProcessService;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.CategoryService;
import com.gdn.partners.pcu.external.service.GCSService;
import com.gdn.partners.pcu.external.service.ImageService;
import com.gdn.partners.pcu.external.service.PickupPointService;
import com.gdn.partners.pcu.external.service.ProductAssemblyService;
import com.gdn.partners.pcu.external.service.ProductPricingService;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.external.service.impl.config.KafkaTopicProperties;
import com.gdn.partners.pcu.external.service.impl.exception.ApplicationException;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.service.impl.helper.BeanUtils;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.partners.pcu.external.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.external.streaming.model.bulk.ProductBasicInfoDownloadRequest;
import com.gdn.partners.pcu.external.streaming.model.bulk.ProductDownloadRequest;
import com.gdn.partners.pcu.external.streaming.model.bulk.ProductDownloadEANRequest;
import com.gdn.partners.pcu.external.web.model.ItemImageWebResponse;
import com.gdn.partners.pcu.external.web.model.enums.InActiveProductStatus;
import com.gdn.partners.pcu.external.web.model.request.ActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.AppealProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkRequest;
import com.gdn.partners.pcu.external.web.model.request.HistorySummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.HistoryUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InProcessProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemLevel4WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointDeleteWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointWebRequest;
import com.gdn.partners.pcu.external.web.model.request.NeedRevisionSubmitWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointUpdateItemsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PriceChangeCompatibleRequest;
import com.gdn.partners.pcu.external.web.model.request.PriceUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductImageEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductL3ListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3CommonImageWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3PriceWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3SummaryDetailsImageWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3UpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3VariantsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductPriceAndStockUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantPriceStockAndImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelProductListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.SuspensionWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpcStatusWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpdateItemsPriceStockImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.WholeSaleDetailListWebRequest;
import com.gdn.partners.pcu.external.web.model.response.ActiveProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.AppealProductConfigResponse;
import com.gdn.partners.pcu.external.web.model.response.BrandPredefinedAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerPickupPointWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategorySuggestionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CreateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.EstimateItemPriceWebResponse;
import com.gdn.partners.pcu.external.web.model.response.HistorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.HistoryUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InProcessWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventoryWarehouseStockWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemL5ListingResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemLevel4ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointListingL3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemsPriceStockImagesUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.MasterWarehouseListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.OrderPlacedWebResponse;
import com.gdn.partners.pcu.external.web.model.response.OrderStatusWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointCodeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PredefinedAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PriceChangeCompatibleResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBasicWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBundleRecipeEditableResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductCountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemNameWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3MasterWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryDetailsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductScoreRuleWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductSystemParameterSwitchWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ReelProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SimpleCategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SuspensionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UniquePickupPointCodeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcCodeAndImagesWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesalePromoResponse;
import com.gdn.partners.pcu.external.web.model.response.YouTubeAPIWebResponse;
import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.GeolocationDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.MarkPickupPointAsDefaultRequest;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.request.CampaignPriceSkuRequest;
import com.gdn.x.campaign.request.ProductCampaignAvailabilityRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignPriceSkuResponse;
import com.gdn.x.campaign.response.ProductCampaignAvailabilityResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummary;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummaryResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryDetailResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryStockInfoDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryResponseDTO;
import com.gdn.x.product.enums.AdjustmentTypeEnum;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.rest.web.model.ItemAndPickupPointBasicDetailResponse;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpcStatusRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.B2BResponse;
import com.gdn.x.product.rest.web.model.response.BundleRecipeV2Response;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
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
import com.gdn.x.product.rest.web.model.response.UpcStatusResponse;
import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.BrandPredefinedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.UPCCodeSearchRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
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
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.PageInfo;
import com.google.api.services.youtube.model.Thumbnail;
import com.google.api.services.youtube.model.ThumbnailDetails;
import com.google.api.services.youtube.model.Video;
import com.google.api.services.youtube.model.VideoListResponse;
import com.google.api.services.youtube.model.VideoLocalization;
import com.google.api.services.youtube.model.VideoSnippet;
import com.google.cloud.storage.Blob;
import com.google.common.collect.ImmutableMap;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.BoundValueOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;


public class ProductServiceImplTest {

  public static final String SRC_TEST_RESOURCES_QR_PIN_PNG = "./src/test/resources/qrPin.png";
  public static final String BUCKET_NAME = "bucket-name";
  public static final String MERCHANT_MODIFIED_FIELD = "merchant-modified-field";
  private static final String BULK_DOWNLOAD_ALL_EVENT = "BULK_DOWNLOAD_ALL_EVENT";
  private static final String VIDEO_REGEX_WITH_SHORTS =
      "(?<=watch\\?v=|/videos/|embed\\/|youtu"
          + ".be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F"
          + "|embed%\u200C\u200B2F|youtu.be%2F|%2Fv%2F|shorts\\/)[^#\\&\\?\\n]*";
  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private XProductFeign xProductFeign;

  @Mock
  private ProductAssemblyFeign productAssemblyFeign;

  @Mock
  private FileStorageServiceImpl fileStorageService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private XInventoryFeign xInventoryFeign;

  @Mock
  private AGPQueryFeign agpQueryFeign;

  @Mock
  private ProductPricingFeign productPricingFeign;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private CategoryService categoryService;

  @Mock
  private ProductAssemblyService productAssemblyService;

  @Mock
  private ProductPricingService productPricingService;

  @Mock
  private BrandService brandService;

  @Mock
  private FbbFeign fbbFeign;

  @InjectMocks
  private ProductServiceImpl productService;

  @Mock
  private StringRedisTemplate productRedisTemplate;

  @Mock
  private BoundValueOperations<String, String> boundValueOperations;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private ImageService imageService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private HttpServletResponse httpServletResponse;

  @Mock
  private ServletOutputStream servletOutputStream;

  @Mock
  private YouTube youTube;

  @Mock
  private YouTube.Videos videos;

  @Mock
  private UserPicService userPicService;

  @Mock
  private YouTube.Videos.List list;

  @Mock
  private PDTFeign pdtFeign;

  private VideoListResponse videoListResponse;

  @Captor
  private ArgumentCaptor<UploadImageRequest> uploadImageRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductLevel3SummaryRequest> productLevel3SummaryRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductLevel3WipSummaryRequest> productLevel3WipSummaryRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductDownloadRequest> productDownloadRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductBasicInfoDownloadRequest> productBasicInfoDownloadRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<UpdateImageRequest> updateImageRequestArgumentCaptor;

  @Captor
  private  ArgumentCaptor<ProductLevel3SummaryDetailsRequest> productLevel3SummaryDetailsRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductLevel3QuickEditRequest> productLevel3QuickEditRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<HistoryUpdateRequest> historyUpdateRequestArgumentCaptor;


  @Mock
  private XCampaignFeign xCampaignFeign;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private GCSService gcsConfig;

  @Mock
  private Blob blob;

  @Mock
  private GCSProperties gcsProperties;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private static final String REQUEST_ID = "requestId";
  private static final double PRICE = 1.5;
  private static final double OFFER_PRICE = 10000;
  private static final String PRODUCT_ID = "PRODUCT_ID";
  private static final String ID = "id";
  private static final long TOTAL_ELEMENTS = 10;
  private EstimateItemPriceResponse estimateItemPriceResponse;
  private MarkPickupPointAsDefaultRequest markPickupPointAsDefaultRequest = new MarkPickupPointAsDefaultRequest();
  private static final String USERNAME = "USERNAME";
  private static final String BUSINESSPARTNER_CODE = "BUSINESSPARTNER_CODE";
  private static final String ERROR_MESSAGE_FOR_NOT_ACTIVE_BP = "merchant status for BUSINESSPARTNER_CODE is Inactive";
  private static final String FLOW_TYPE = "FLOW1";
  private static final String FLOW_TYPE_3 = "FLOW3";
  private static final String GDN_SKU = "GDN_SKU";
  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final PageMetaData pageMetaData = new PageMetaData(100, 1, 100);
  private static final String KEYWORD = "keyword";
  private static final String KEYWORDS = "1";
  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String CATEGORY_CODE_3 = "categoryCode3";
  private static final String CATEGORY_CODE_4 = "categoryCode4";
  private static final String CATEGORY_CODE_5 = "categoryCode5";
  private static final String CATEGORY_CODE_6 = "categoryCode6";
  private static final String CATEGORY_CODE_7 = "categoryCode7";
  private static final String CATEGORY_CODE_8 = "categoryCode8";
  private static final String CATEGORY_CODE_9 = "categoryCode9";
  private static final String CATEGORY_ID_1 = "categoryId1";
  private static final String CATEGORY_ID_2 = "categoryId2";
  private static final String CATEGORY_ID_3 = "categoryId3";
  private static final String CATEGORY_ID_4 = "categoryId4";
  private static final String CATEGORY_ID_5 = "categoryId5";
  private static final String CATEGORY_ID_6 = "categoryId6";
  private static final String CATEGORY_ID_7 = "categoryId7";
  private static final String CATEGORY_ID_8 = "categoryId8";
  private static final String CATEGORY_ID_9 = "categoryId9";
  private static final String CATEGORY_NAME_1 = "categoryName1";
  private static final String CATEGORY_NAME_2 = "categoryName2";
  private static final String CATEGORY_NAME_3 = "categoryName3";
  private static final String CATEGORY_NAME_4 = "categoryName4";
  private static final String CATEGORY_NAME_5 = "categoryName5";
  private static final String CATEGORY_NAME_6 = "categoryName6";
  private static final String CATEGORY_NAME_7 = "categoryName7";
  private static final String CATEGORY_NAME_8 = "categoryName8";
  private static final String CATEGORY_NAME_9 = "categoryName9";
  private static final String CATEGORY_NAME_ENGLISH_1 = "categoryNameEnglish1";
  private static final String CATEGORY_NAME_ENGLISH_2 = "categoryNameEnglish2";
  private static final String CATEGORY_NAME_ENGLISH_3 = "categoryNameEnglish3";
  private static final String CATEGORY_NAME_ENGLISH_4 = "categoryNameEnglish4";
  private static final long PRODUCT_COUNT_1 = 16;
  private static final long PRODUCT_COUNT_2 = 8;
  private static final long PRODUCT_COUNT_3 = 4;
  private static final long PRODUCT_COUNT_4 = 2;
  private static final long PRODUCT_COUNT_5 = 1;
  private static final long PRODUCT_COUNT_6 = 1;
  private static final String VIEWABLE_PRODUCT_COUNT = "VIEWABLE_PRODUCT_COUNT";
  private static final long REDIS_PRODUCT_COUNT_TIMEOUT = 5l;
  private static final String PRODUCTS_COUNT = "1234";
  private ProductDetailResponse productDetailResponse;
  private ProductCreationRequest productCreationRequest;
  private GdnRestSingleResponse<ProductDetailResponse> response;
  private ProductLevel3Response productLevel3Response;
  private GdnBaseRestResponse gdnBaseRestResponse;
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String PRODUCT_SKU_NEW = "TOQ-16110-00001";
  private static final String PRODUCT_SKU_NEW_1 = "TOQ-16110-00001-11";
  private static final String SUSPENSION_REASON = "Wrong product";
  private static final String PRODUCT_NAME_2 = "PRODUCT_NAME 2";
  private static final String ACTIVITY = "ACTIVITY";
  private static final String CATEGORY_ID = "CATEGORY_ID";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_CODE_1 = "brandCode1";
  private static final String BRAND_APPROVAL_STATUS_APPROVED = "APPROVED";
  private static final String BRAND_APPROVAL_STATUS_DRAFT = "DRAFT";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String ITEM_NAME = "ITEM_NAME";
  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String WAREHOUSE_CODE = "WAREHOUSE_CODE";
  private static final String IMAGE_URL = "IMAGE_URL";
  private static final double TEST_PRICE = 53.53;
  private static final long TOTAL_RECORDS = 100;
  private static final String ORDER_BY = "orderBy";
  private static final String SORT_BY = "sortBy";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_NAME = "pickupPointName";
  private static final String PROMO_TYPE = "PROMO_TYPE";
  private static final PageRequest pageRequest = PageRequest.of(0, 10);
  private static final String SEARCH_KEY = "SEARCH_KEY";
  private static final String ACTIVE = "ACTIVE";
  private static final String INPROCESS = "INPROCESS";
  private static final String INACTIVE = "INACTIVE";
  private static final String TYPE = "type";
  private static final String DATA_LINK = "DATA_LINK";
  private static final String INVALID_TYPE_ERR = "Invalid value for type request parameter for inActive api";
  private static final String ORDERBY = "createdDate";
  private static final String SORTBY = "desc";
  private static final String BRAND = "Brand";
  private static final String BRAND1 = "Brand1";
  private static final String CRITERIA = "IN_PROGRESS";
  private static final boolean DO_ARCHIVE = false;
  private static final String ITEM_SKU_1 = "BUSINESSPARTNER_CODE_ITEM_SKU";
  private static final String PRODUCT_SKU_1 = "BUSINESSPARTNER_CODE_PRODUCT_SKU";
  private static final String PRODUCT_BUSINESS_PARTNER_ID = "PRODUCT_BUSINESS_PARTNER_ID";
  public static final String DEFAULT_CLIENT_HOST = "MTA_WEB";
  public static final String EMAIL = "EMAIL";
  public static final String PURCHASE_TERM = "PURCHASE_TERM";
  public static final Integer productSize = 100;
  public static final String BUSINESS_PARTNER_CATEGORY = "KATEGORI_TEST";
  public static final String BUSINESS_PARTNER_NAME = "TEST";
  public static final String ATTRIBUTE_ID1 = "ATTRIBUTE_ID1";
  public static final String ATTRIBUTE_ID2 = "ATTRIBUTE_ID2";
  public static final String ATTRIBUTE_ID3 = "ATTRIBUTE_ID3";
  public static final String ATTRIBUTE_ID4 = "ATTRIBUTE_ID4";
  private static final String ITEM_CODE = "itemCode";
  private static final String INTERNAL = "INTERNAL";
  private static final String COLOUR_FAMILY = "Family Colour";
  private static final String WARNA = "Warna";
  private static final String COLOR = "Color";
  private static final String GARANSI = "Garansi";
  private static final String WARRANTY = "Warranty";
  private static final Integer DELTA_STOCK = 200;
  private static final String PRODUCT_DESCRIPTION = "Description";
  private static final String UPC_CODE = "12345678";
  private static final String LOCATION_PATH = "locationPath";
  private static final String UNAUTHORIZE_ERROR = "Unauthorize access :You are not authorized";
  private static final String UNAUTHORIZE_ERROR1 = "You are not authorized";
  private static final String STORE_ID = "10001";
  private static final long NORMAL_PRICE = 1000;
  private static final long SALES_PRICE = 2000;
  private static final String YOUTUBE_URL = "https://www.youtube.com/watch?v=P1xAhgKTqDA";
  private static final String YOUTUBE_INVALID_URL = "https://www.youtube.com/watch?v";
  private static final Integer MINIMUM_PRICE = 1;
  private static final Long VERSION = Long.valueOf(2);
  private static final String API_KEY = "apiKey";
  private static final String PRODUCT_DETAIL_LINK = "https://wwwuatb.gdn-app.com//product-detail";
  private static final String VIDEO_ID = "P1xAhgKTqDA";
  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String DESCRIPTION = "description";
  private static final String MATERIAL_CODE = "materialCode";
  private static final String URL = "url";
  private static final String TITLE = "title";
  private static final String SWITCH_VARIABLE = "variable";
  private static final String REVIEW_TYPE = "prelive";
  private static final String ORDERBY_ARCHIVED = "isArchived";
  private static final String SORTBY_ASC = "asc";
  private static final String PREORDER_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private static final String PICKUP_POINT_CODE_1 = "ppCode1";
  private static final String CHANGED_BY = "changedBy";
  private static final boolean IS_WARE_HOUSE = true;
  private static final String PREORDER_ERROR = "PreOrder type must be DAYS, WEEK or DATE";
  private static final String VARIANT_NAME = "VARIANT_NAME";
  private static final String MERCHANT_TYPE_CC = "CC";
  private static final String MERCHANT_TYPE_TD = "TD";
  private static final String MERCHANT_TYPE_TC = "TC";
  private static final String MERCHANT_TYPE_RC = "RC";
  private static final String OTHER = "other";

  private GdnRestListResponse<ProductItemResponse> productItemResponses;
  private GdnRestSingleResponse<ProductLevel3Response> productResponse;
  private List<ProductItemResponse> productItemResponseList;
  private ProductItemResponse productItemResponse;
  private List<String> categoryCodes;
  private GdnRestListResponse<ProductItemDetailResponse> productItemDetailResponses;
  private List<ProductItemDetailResponse> productItemDetailResponseList;
  private ProductItemDetailResponse productItemDetailResponse;
  private ProductSearchRequest productSearchRequest;
  private GdnRestSingleResponse<BrandWipResponse> gdnBrandWipResponse;
  private Set<String> itemSkusSet = new HashSet<>();
  private WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest;
  private List<PriceChangeCompatibleRequest> priceChangeCompatibleRequests = new ArrayList<>();
  private List<PriceChangeCompatibleRequest> priceChangeCompatibleRequests_1 = new ArrayList<>();
  private WholesaleMappingResponse wholesaleMappingResponse = new WholesaleMappingResponse();
  private Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap = new HashMap<>();
  private WholesalePriceSkuResponse wholesalePriceSkuResponse= new WholesalePriceSkuResponse();
  private Map<Integer, Double> wholesaleRules = new HashMap<>();
  private MinWholesaleDiscountResponse minWholesaleDiscountResponse = new MinWholesaleDiscountResponse();
  private WholesaleConfigResponse wholesaleConfigResponse = new WholesaleConfigResponse();

  private ActiveProductRequest activeProductRequest;
  private ProductL3ListingWebRequest productL3ListingWebRequest;
  private ProductL3ListingWebResponse productL3ListingWebResponse;
  private ActiveProductRequest activeProductRequestForQR;
  private CategoryWebResponse categoryWebResponse;
  private ActiveProductResponse activeProductResponse;
  private List<ActiveProductResponse> activeProductResponseList;
  private GdnRestListResponse<ActiveProductResponse> activeProductResponseGdnRestListResponse;
  private ItemDetailResponse itemDetailResponse;

  private CategoryCodeRequest categoryCodeRequest;
  private CategoryCodeResponse categoryCodeResponse;
  private GdnRestSingleResponse<CategoryCodeResponse> categoryCodeResponseGdnRestSingleResponse;
  private ActiveProductWebRequest activeProductWebRequest;
  private ProductSummaryWebRequest productSummaryWebRequest;
  private ActiveProductWebRequest activeProductWebRequest1;
  private ActiveProductWebRequest activeProductWebRequest2;
  private ActiveProductRequest activeProductRequest1;
  private ActiveProductResponse activeProductResponse1;
  private List<ActiveProductResponse> activeProductResponseList1;
  private GdnRestListResponse<ActiveProductResponse> gdnRestListResponse1;
  private SummaryFilterRequest summaryFilterRequest;
  private ProductLevel3SummaryRequest productLevel3SummaryRequest;
  private ProductLevel3SummaryRequest productLevel3SummaryRequest1;
  private ProductLevel3SummaryResponse productLevel3SummaryResponse;
  private GdnRestListResponse<SuspensionItemResponse> suspensionItemResponseGdnRestListResponse;
  private GdnRestSingleResponse<ProductLevel3SummaryCountResponse>
      productLevel3SummaryCountResponseGdnRestSingleResponse;
  private GdnRestSingleResponse<CountProductLevel3WipResponse> countProductLevel3WipResponseGdnRestSingleResponse;
  private GdnRestSingleResponse<CountProductLevel3InactiveResponse>
      countProductLevel3InactiveResponseGdnRestSingleResponse;
  private GdnRestListResponse<ProductLevel3SummaryResponse> productLevel3SummaryResponseGdnRestListResponse;
  private GdnRestListResponse<ItemSummaryResponse> itemSummaryResponseGdnRestListResponse;
  private GdnRestListResponse<ProductNameSuggestionResponse> productNameSuggestionResponseGdnRestListResponse;
  private GdnRestListResponse<LogAuditTrailUpdatedProductResponse>
      logAuditTrailUpdatedProductResponseGdnRestListResponse;
  private InActiveProductWebRequest inActiveProductWebRequest;
  private GdnRestListResponse<RejectedSkuProductResponse> rejectedSkuProductResponseGdnRestListResponse;
  private SortOrder sort;
  private InProcessProductWebRequest inProcessProductWebRequest;
  private GdnRestListResponse<ProductLevel3WipResponse> responseGdnRestListResponse;
  private ProductLevel3WipResponse productLevel3WipResponse;
  private ItemSummaryRequest itemSummaryRequest;
  private List<String> itemSkus = new ArrayList<>();
  private List<String> productSkus = new ArrayList<>();
  private ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest;
  private GdnRestSingleResponse<ProductLevel3OrderResponse> productLevel3OrderResponseGdnRestSingleResponse;
  private GdnRestSingleResponse<ProductLevel3SummaryResponse> productLevel3SummaryResponseGdnRestSingleResponse;
  private BulkDeleteProductWipRequest bulkDeleteProductWipRequest;
  private Map<String, ProductLevel3UpdateSummaryRequest> productLevel3UpdateSummaryRequestMap;
  private ProfileResponse profileResponse;
  private CategoryDetailResponse categoryDetailResponse;
  private BulkRequest bulkRequest;
  private AttributeResponse attributeResponse1;
  private AttributeResponse attributeResponse2;
  private AttributeResponse attributeResponse3;
  private AttributeResponse attributeResponse4;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse1;
  private PredefinedAttributeValueWebResponse predefinedAttributeValueWebResponse;
  private PredefinedAttributeValueWebResponse predefinedAttributeValueWebResponse1;
  private PredefinedAttributeValueWebResponse predefinedAttributeValueWebResponse2;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse2;
  private CategoryAttributeResponse categoryAttributeResponse;
  private ProductLevel3Request productLevel3Request;
  private GdnRestSingleResponse<ProductLevel3Response> productLevel3ResponseGdnRestSingleResponse;
  private ProductLevel3OrderResponse level3OrderResponse;
  private GdnRestSingleResponse<ProductLevel3OrderResponse> level3OrderResponseGdnRestSingleResponse;
  private ProductLevel3WebRequest productLevel3WebRequest;
  private ProductLevel3UpdateWebRequest productLevel3UpdateWebRequest;
  private PickupPointUpdateWebRequest pickupPointUpdateWebRequest;
  private ProductLevel3UpdateRequest productLevel3UpdateRequest;
  private ProductItemLevel3Request productItemLevel3Request;
  private UPCCodeSearchRequest upcCodeSearchRequest;
  List<CategoryAttributeResponse> listCategoryAttributeResponse = new ArrayList<CategoryAttributeResponse>();
  private CompanyDTO company;
  private ActiveProductRequest activeProductRequest2;
  private UpdateImageRequest updateImageRequest;
  private ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest = new ProductCampaignAvailabilityRequest();
  private ProductCampaignAvailabilityResponse productCampaignAvailabilityResponse = new ProductCampaignAvailabilityResponse();
  private ProductPriceAndStockUpdateWebRequest productPriceAndStockUpdateWebRequest =
      new ProductPriceAndStockUpdateWebRequest();
  private PriceUpdateWebRequest priceUpdateWebRequest = new PriceUpdateWebRequest();
  private YouTubeAPIWebResponse youTubeAPIWebResponse;
  private GdnRestSingleResponse<ProductSystemParameterResponse> systemParameterResponse;
  private ProductSystemParameterResponse productSystemParameterResponse;
  private ProductSystemParameterResponse productSystemParameterResponse1;
  private BrandPredefinedAttributeValueResponse brandPredefinedAttributeValueResponse;
  private Map<String, Object> switchVariableMap = new HashMap<>();
  private SystemParameterConfigResponse systemParameterConfigResponse;
  private CategoryAttributeResponse categoryAttributeResponse1;
  private CategoryAttributeResponse categoryAttributeResponse2;
  private CategoryAttributeResponse categoryAttributeResponse3;
  private ProductEditInfoWebRequest productEditInfoWebRequest;
  private EditProductV2Response editProductResponse;
  private GdnRestSingleResponse<EditProductV2Response> editProductResponseGdnRestSingleResponse;
  private EditProductWebResponse editProductWebResponse;
  private GdnRestListResponse<ProductLevel3SummaryDetailsResponse> productLevel3SummaryDetailsResponseGdnRestListResponse;
  private PreOrderRequest preOrderRequest;
  private UniquePickupPointCodeResponse uniquePickupPointCodeResponse;
  private UpdateItemsPriceStockImagesWebRequest updateItemsPriceStockImagesWebRequest =
      new UpdateItemsPriceStockImagesWebRequest();
  private UpdateItemsPriceStockImagesRequest updateItemsPriceStockImagesRequest =
      new UpdateItemsPriceStockImagesRequest();
  private ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
  private ReservedStockSummaryResponse reservedStockSummaryResponse = new ReservedStockSummaryResponse();
  private ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
  private ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
      new ProductLevel3ViewConfigResponse();
  private GdnRestListResponse<ProductL3SummaryResponse> productL3SummaryResponseGdnRestListResponse;
  private GdnRestListResponse<ProductSuspensionHistoryResponse> productSuspensionHistoryResponseGdnRestListResponse;
  private GdnRestSingleResponse<CategoryNamesResponse> categoryNamesResponseGdnRestSingleResponse;
  private ProductL3SummaryResponse productL3SummaryResponse;
  private ProductSuspensionHistoryResponse productSuspensionHistoryResponse;
  private ItemL4SummaryResponse itemL4SummaryResponse;
  private ProductSummaryRequest productSummaryRequest;
  private InventoryStockInfoDTO inventoryStockInfoDTO;
  private InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO;
  private WebInventoryResponseDTO webInventoryResponseDTO;
  private WarehouseInventoryResponseDTO warehouseInventoryResponseDTO;
  private CampaignPriceResponse campaignPriceResponse;
  private CampaignPriceSkuResponse campaignPriceSkuResponse;
  private PriceDTO priceDTO;
  private CampaignPriceRequest campaignPriceRequest;
  private CampaignPriceRequest campaignPriceRequest1;
  private HistoryUpdateWebRequest historyUpdateWebRequest;
  private HistoryUpdateResponse historyUpdateResponse;
  private ProductVariantUpdateWebRequest productVariantUpdateWebRequest;
  private BrandInReviewResponse brandInReviewResponse1;
  private BrandInReviewResponse brandInReviewResponse2;
  private BrandInReviewResponse brandInReviewResponse3;
  private WholeSaleDetailListWebRequest wholeSaleDetailListWebRequests;
  private List<PickupPointOutboundResponse> pickupPointResponseList;
  private ProductLevel3ViewConfigResponse viewConfigResponseB2b;
  private B2BResponse b2BResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(httpServletResponse.getOutputStream()).thenReturn(servletOutputStream);
    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setId(PRODUCT_ID);
    response = new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID);
    gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(Boolean.TRUE);
    gdnBrandWipResponse = new GdnRestSingleResponse<>();
    gdnBrandWipResponse.setSuccess(true);
    BrandWipResponse brandWipResponse = new BrandWipResponse();
    brandWipResponse.setState(BRAND_APPROVAL_STATUS_DRAFT);
    gdnBrandWipResponse.setValue(brandWipResponse);


    productSystemParameterResponse = new ProductSystemParameterResponse();
    productSystemParameterResponse.setVariable(Constants.YOUTUBE_VALIDATION_SWITCH);
    productSystemParameterResponse.setValue("true");
    systemParameterResponse = new GdnRestSingleResponse<>();
    systemParameterResponse.setSuccess(true);
    systemParameterResponse.setValue(productSystemParameterResponse);

    productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    List<ProductCategoryRequest> productCategoryRequestList = new ArrayList<>();
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setCategoryCode(CATEGORY_CODE);
    productCategoryRequest.setCategory(categoryRequest);
    productCategoryRequestList.add(productCategoryRequest);
    productCreationRequest.setProductCategories(productCategoryRequestList);
    productCreationRequest.setBrandCode(BRAND_CODE);

    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setId(PRODUCT_ID);
    response = new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID);
    categoryCodes = Arrays.asList(CATEGORY_CODE);
    productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setId(ID);
    productItemDetailResponse.setGeneratedItemName(PRODUCT_NAME);
    productItemDetailResponseList = Arrays.asList(productItemDetailResponse);
    ReflectionTestUtils.setField(productService,"baseUrlForPdpWithL5","https://www.blibli.com/p/p/productSku");
    productItemDetailResponses =
        new GdnRestListResponse<>(productItemDetailResponseList, new PageMetaData(PAGE, SIZE, TOTAL_ELEMENTS),
            REQUEST_ID);
    productItemDetailResponses.getPageMetaData().setTotalRecords(TOTAL_ELEMENTS);
    productSearchRequest = new ProductSearchRequest();
    productSearchRequest.setKeyword(PRODUCT_NAME);
    productSearchRequest.setCategoryCodes(categoryCodes);
    productItemResponse = new ProductItemResponse();
    productItemResponse.setId(PRODUCT_ID);
    productItemResponseList = new ArrayList<>();
    productItemResponseList.add(productItemResponse);
    productItemResponses = new GdnRestListResponse<>(productItemResponseList, new PageMetaData(), REQUEST_ID);
    response = new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID);
    estimateItemPriceResponse = new EstimateItemPriceResponse();
    estimateItemPriceResponse.setOfferPrice(OFFER_PRICE);
    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setId(PRODUCT_ID);
    response = new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID);
    when(this.productRedisTemplate.boundValueOps(VIEWABLE_PRODUCT_COUNT)).thenReturn(this.boundValueOperations);

    categoryCodeRequest = new CategoryCodeRequest();
    categoryCodeRequest.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));

    categoryCodeResponse = new CategoryCodeResponse();
    categoryCodeResponse.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    categoryCodeResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(categoryCodeResponse, REQUEST_ID);

    activeProductRequest = new ActiveProductRequest();
    activeProductRequest.setMerchantCode(MERCHANT_CODE);
    activeProductRequest.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    activeProductRequest.setDiscoverable(true);
    activeProductRequest.setBuyable(true);
    activeProductRequest.setSearchKey(SEARCH_KEY);

    productL3ListingWebRequest = new ProductL3ListingWebRequest();
    productL3ListingWebRequest.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    productL3ListingWebRequest.setSearchKey(SEARCH_KEY);
    productL3ListingWebRequest.setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    productL3ListingWebRequest.setCncActivated(Boolean.TRUE);

    productL3ListingWebResponse = new ProductL3ListingWebResponse();
    productL3ListingWebResponse.setProductName(PRODUCT_NAME);
    productL3ListingWebResponse.setProductSku(PRODUCT_SKU);
    productL3ListingWebResponse.setCategoryName(CATEGORY_NAME_1);
    productL3ListingWebResponse.setVariantCount(1);
    productL3ListingWebResponse.setImageUrl(IMAGE_URL);

    categoryWebResponse = new CategoryWebResponse();
    categoryWebResponse.setCategoryCode(CATEGORY_CODE);
    categoryWebResponse.setName(CATEGORY_NAME_1);

    activeProductResponse = new ActiveProductResponse();
    activeProductResponse.setProductCode(PRODUCT_CODE);
    activeProductResponse.setProductName(PRODUCT_NAME);
    activeProductResponse.setProductSku(PRODUCT_SKU);
    activeProductResponse.setImageUrl(IMAGE_URL);
    activeProductResponse.setMerchantCode(MERCHANT_CODE);
    activeProductResponse.setItemCount(1);
    itemDetailResponse = new ItemDetailResponse();
    itemDetailResponse.setItemName(ITEM_NAME);
    itemDetailResponse.setItemSku(ITEM_SKU);
    List<ItemDetailResponse> itemDetailResponseList = new ArrayList<>();
    itemDetailResponseList.add(itemDetailResponse);
    activeProductResponse.setItemDetailResponses(itemDetailResponseList);
    activeProductResponseList = new ArrayList<>();
    activeProductResponseList.add(activeProductResponse);
    activeProductResponseGdnRestListResponse =
        new GdnRestListResponse<>(activeProductResponseList, pageMetaData, REQUEST_ID);

    activeProductWebRequest = new ActiveProductWebRequest();
    activeProductWebRequest.setCategoryCode(CATEGORY_CODE);
    activeProductWebRequest.setSearchKey(SEARCH_KEY);
    activeProductWebRequest.setDiscoverable(true);
    activeProductWebRequest.setBuyable(true);
    activeProductWebRequest.setPage(PAGE);
    activeProductWebRequest.setSize(SIZE);

    activeProductWebRequest1 = new ActiveProductWebRequest();
    activeProductWebRequest1.setPage(PAGE);
    activeProductWebRequest1.setSize(SIZE);
    activeProductWebRequest1.setBuyable(true);
    activeProductWebRequest1.setDiscoverable(true);

    activeProductWebRequest2 = ActiveProductWebRequest.builder().businessPartnerCode(BUSINESSPARTNER_CODE)
        .categoryCodes(Arrays.asList(CATEGORY_CODE)).pickupPointCodes(Arrays.asList(PICKUP_POINT_CODE))
        .searchKey(SEARCH_KEY).inventoryFilter("AVAILABLE").buyable(true).displayable(true).build();

    activeProductRequest1 = new ActiveProductRequest();
    activeProductRequest1.setMerchantCode(MERCHANT_CODE);
    activeProductRequest1.setCategoryCodes(Collections.EMPTY_LIST);
    activeProductRequest1.setBuyable(true);
    activeProductRequest1.setDiscoverable(true);
    activeProductRequest1.setSearchKey(null);
    activeProductResponse1 = new ActiveProductResponse();
    activeProductResponse1.setMerchantCode(MERCHANT_CODE);
    activeProductResponse1.setItemCount(1);
    List<ItemDetailResponse> itemDetailResponseList1 = new ArrayList<>();
    activeProductResponse1.setItemDetailResponses(itemDetailResponseList1);
    activeProductResponseList1 = new ArrayList<>();
    activeProductResponseList1.add(activeProductResponse1);
    gdnRestListResponse1 = new GdnRestListResponse<>(activeProductResponseList1, pageMetaData, REQUEST_ID);

    summaryFilterRequest = SummaryFilterRequest.builder().build();
    suspensionItemResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(new SuspensionItemResponse()), pageMetaData, REQUEST_ID);

    ProductLevel3SummaryCountResponse productLevel3SummaryCountResponse = new ProductLevel3SummaryCountResponse();
    Map<ProductLevel3InventoryCriteria, Long> stockConditionCounts = new HashMap<>();
    stockConditionCounts.put(ProductLevel3InventoryCriteria.AVAILABLE, 10L);
    stockConditionCounts.put(ProductLevel3InventoryCriteria.STOCK_ALERT, 20L);
    stockConditionCounts.put(ProductLevel3InventoryCriteria.OOS, 30L);
    productLevel3SummaryCountResponse.setStockConditionCounts(stockConditionCounts);
    productLevel3SummaryCountResponseGdnRestSingleResponse =
        new GdnRestSingleResponse(productLevel3SummaryCountResponse, REQUEST_ID);

    Map<ProductLevel3WipSummaryCriteria, Long> totalItemsByCriterias =
        new HashMap<ProductLevel3WipSummaryCriteria, Long>();
    totalItemsByCriterias.put(ProductLevel3WipSummaryCriteria.FAILED, 10L);
    totalItemsByCriterias.put(ProductLevel3WipSummaryCriteria.IN_PROGRESS, 20L);
    totalItemsByCriterias.put(ProductLevel3WipSummaryCriteria.NEED_CORRECTION, 30L);
    CountProductLevel3WipResponse countProductLevel3WipResponse = new CountProductLevel3WipResponse();
    countProductLevel3WipResponse.setTotalItemsByCriterias(totalItemsByCriterias);
    countProductLevel3WipResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(countProductLevel3WipResponse, REQUEST_ID);

    Map<ProductLevel3InactiveSummaryCriteria, Long> totalInactiveItemsByCriterias = new HashMap<>();
    totalInactiveItemsByCriterias.put(ProductLevel3InactiveSummaryCriteria.ARCHIVED, 10L);
    totalInactiveItemsByCriterias.put(ProductLevel3InactiveSummaryCriteria.REJECTED, 20L);
    totalInactiveItemsByCriterias.put(ProductLevel3InactiveSummaryCriteria.SUSPENDED, 30L);
    CountProductLevel3InactiveResponse countProductLevel3InactiveResponse = new CountProductLevel3InactiveResponse();
    countProductLevel3InactiveResponse.setTotalItemsByCriterias(totalInactiveItemsByCriterias);
    countProductLevel3InactiveResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(countProductLevel3InactiveResponse, REQUEST_ID);

    inActiveProductWebRequest = InActiveProductWebRequest.builder().businessPartnerCode(BUSINESSPARTNER_CODE)
        .categoryCodes(Arrays.asList(CATEGORY_CODE)).pickupPointCodes(Arrays.asList(PICKUP_POINT_CODE))
        .searchKey(SEARCH_KEY).orderBy(ORDER_BY).sortBy(SORT_BY).build();
    sort = new SortOrder(SORT_BY, ORDER_BY);
    productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    productLevel3PriceResponse.setPrice(0.0);
    productLevel3PriceResponse.setSalePrice(0.0);
    productLevel3SummaryResponse.setPrices(Arrays.asList(productLevel3PriceResponse));
    productLevel3SummaryResponse.setViewConfigs(Arrays.asList(productLevel3ViewConfigResponse));
    productLevel3SummaryResponse.setSynchronizeStock(Boolean.TRUE);
    productLevel3SummaryResponse.setOff2OnActiveFlag(Boolean.TRUE);
    productLevel3SummaryResponse.setArchived(Boolean.TRUE);
    productLevel3SummaryResponse.setProductDetailPageLink(DATA_LINK);
    productLevel3SummaryResponse.setItemName(ITEM_NAME);
    productLevel3SummaryResponse.setProductCode(PRODUCT_CODE);
    productLevel3SummaryResponse.setItemSku(ITEM_SKU);
    productLevel3SummaryResponse.setWholesalePriceActivated(Boolean.TRUE);
    productLevel3SummaryResponse.setProductScore(90.0);
    productLevel3SummaryResponse.setCategoryCode(CATEGORY_CODE);
    productLevel3SummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    productLevel3SummaryResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(productLevel3SummaryResponse), pageMetaData, REQUEST_ID);
    rejectedSkuProductResponseGdnRestListResponse =
        new GdnRestListResponse<>(Collections.singletonList(new RejectedSkuProductResponse()), pageMetaData,
            REQUEST_ID);
    productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    productLevel3SummaryRequest.setArchived(true);
    productLevel3SummaryRequest.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    productLevel3SummaryRequest.setPickupPointCodes(Arrays.asList(PICKUP_POINT_CODE));
    productLevel3SummaryRequest.setSearchKey(SEARCH_KEY);

    populateProductLevel3SummaryRequest1();

    inProcessProductWebRequest = InProcessProductWebRequest.builder().businessPartnerCode(BUSINESSPARTNER_CODE)
        .categoryCodes(Arrays.asList(CATEGORY_CODE)).searchKey(PRODUCT_NAME).orderBy(ORDER_BY).sortBy(SORT_BY)
        .criteria(CRITERIA).build();
    productLevel3WipResponse = new ProductLevel3WipResponse();
    productLevel3WipResponse.setProductName(PRODUCT_NAME);
    productLevel3WipResponse.setCategoryName(CATEGORY_NAME_1);
    productLevel3WipResponse.setBrandName(BRAND);
    responseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(productLevel3WipResponse), pageMetaData, REQUEST_ID);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setItemSku(ITEM_SKU);
    itemSummaryResponse.setGeneratedItemName(ITEM_NAME);
    itemSummaryResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(itemSummaryResponse), pageMetaData, REQUEST_ID);

    productNameSuggestionResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(new ProductNameSuggestionResponse(PRODUCT_NAME, PRODUCT_SKU_NEW)),
            pageMetaData, REQUEST_ID);

    itemSummaryRequest = new ItemSummaryRequest();
    itemSummaryRequest.setArchived(false);
    itemSummaryRequest.setMerchantCode(MERCHANT_CODE);
    itemSummaryRequest.setSearchKey(SEARCH_KEY);

    itemSkus.add(ITEM_SKU_1);

    LogAuditTrailUpdatedProductResponse logAuditTrailUpdatedProductResponse = new LogAuditTrailUpdatedProductResponse();
    logAuditTrailUpdatedProductResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(logAuditTrailUpdatedProductResponse), pageMetaData, REQUEST_ID);

    productLevel3UpdateSummaryRequest = new ProductLevel3UpdateSummaryRequest();
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setSalePrice(1000.0);
    productLevel3PriceRequest.setPrice(2000.0);
    productLevel3UpdateSummaryRequest.setPrices(Arrays.asList(productLevel3PriceRequest));
    productLevel3UpdateSummaryRequest.setAccessChannel(UpdateProductAccessChannel.MTA_WEB_UPDATE_LIST.getDesc());

    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    productLevel3SummaryResponse.setItemSku(ITEM_SKU);
    productLevel3SummaryResponse.setItemName(ITEM_NAME);
    productLevel3SummaryResponse.setSkuCode(PRODUCT_CODE);
    productLevel3SummaryResponse.setMerchantSku(BUSINESSPARTNER_CODE);
    productLevel3SummaryResponse.setAvailableStockLevel2(0);
    productLevel3SummaryResponse.setProductType(0);
    productLevel3SummaryResponse.setPickupPointCode(BUSINESSPARTNER_CODE);
    productLevel3SummaryResponse.setOff2OnActiveFlag(true);
    productLevel3SummaryResponse.setSynchronizeStock(true);
    ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
    productLevel3PriceResponse.setPrice(1D);
    productLevel3PriceResponse.setSalePrice(1D);
    ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse = new ProductLevel3ViewConfigResponse();
    productLevel3ViewConfigResponse.setBuyable(false);
    productLevel3ViewConfigResponse.setDisplay(false);
    productLevel3SummaryResponse.setPrices(new ArrayList<ProductLevel3PriceResponse>());
    productLevel3SummaryResponse.setViewConfigs(new ArrayList<ProductLevel3ViewConfigResponse>());
    productLevel3SummaryResponse.getPrices().add(productLevel3PriceResponse);
    productLevel3SummaryResponse.getViewConfigs().add(productLevel3ViewConfigResponse);

    productLevel3OrderResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(new ProductLevel3OrderResponse(), REQUEST_ID);

    productLevel3SummaryResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(productLevel3SummaryResponse, REQUEST_ID);

    bulkDeleteProductWipRequest = new BulkDeleteProductWipRequest();

    productLevel3UpdateSummaryRequestMap = new HashMap<>();
    productLevel3UpdateSummaryRequestMap.put(ITEM_SKU, productLevel3UpdateSummaryRequest);

    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");
    PickupPointDTO pickupPointDto = new PickupPointDTO();
    pickupPointDto.setCode(BUSINESSPARTNER_CODE);
    pickupPointDto.setFbbActivated(true);
    pickupPointDto.setGeolocation(new GeolocationDTO());
    PickupPointDTO pickupPointDto1 = new PickupPointDTO();
    pickupPointDto1.setCode(PICKUP_POINT_CODE);
    pickupPointDto1.setGeolocation(null);
    pickupPointDto1.setFbbActivated(false);
    List<PickupPointDTO> listPickupPointDto = new ArrayList<PickupPointDTO>();
    listPickupPointDto.add(pickupPointDto);
    listPickupPointDto.add(pickupPointDto1);
    company = new CompanyDTO();
    company.setBusinessPartnerName(BUSINESSPARTNER_CODE);
    company.setMerchantFlag(true);
    company.setInternationalFlag(false);
    company.setMerchantType("CM");
    company.setOfflineToOnlineFlag(true);
    company.setInventoryFulfillment("BL");
    company.setOfflineToOnlineFlag(true);
    company.setEmail(EMAIL);
    company.setPurchaseTerm(PURCHASE_TERM);
    profileResponse.setCompany(company);
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setAllCategory(true);

    List<AllowedAttributeValueResponse> listAllowedAttributeValueResponse =
        new ArrayList<AllowedAttributeValueResponse>();
    AllowedAttributeValueResponse allowedAttributeValues = new AllowedAttributeValueResponse();
    allowedAttributeValues.setValue(BUSINESS_PARTNER_CATEGORY);
    allowedAttributeValues.setSequence(1);
    listAllowedAttributeValueResponse.add(allowedAttributeValues);
    AllowedAttributeValueResponse allowedAttributeValues2 = new AllowedAttributeValueResponse();
    allowedAttributeValues2.setValue(BUSINESS_PARTNER_NAME);
    allowedAttributeValues2.setSequence(2);
    listAllowedAttributeValueResponse.add(allowedAttributeValues2);
    AllowedAttributeValueResponse allowedAttributeValues3 = new AllowedAttributeValueResponse();
    allowedAttributeValues3.setValue(BUSINESS_PARTNER_NAME);
    allowedAttributeValues3.setSequence(1);
    listAllowedAttributeValueResponse.add(allowedAttributeValues3);

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttribute = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttribute.setValue(BUSINESS_PARTNER_CATEGORY);
    predefinedAllowedAttribute.setUpdatedDate(new Date());

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttribute1 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttribute1.setValue("OEM");
    predefinedAllowedAttribute1.setUpdatedDate(new Date());

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttribute2 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttribute2.setValue("Brand_unapproved");
    predefinedAllowedAttribute2.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_DRAFT);
    predefinedAllowedAttribute2.setUpdatedDate(new Date());

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttribute3 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttribute2.setValue(BRAND1);
    predefinedAllowedAttribute2.setUpdatedDate(new Date());

    List<PredefinedAllowedAttributeValueResponse> listPredefinedAllowedAttribute =
        new ArrayList<PredefinedAllowedAttributeValueResponse>();
    listPredefinedAllowedAttribute.add(predefinedAllowedAttribute);
    listPredefinedAllowedAttribute.add(predefinedAllowedAttribute1);
    listPredefinedAllowedAttribute.add(predefinedAllowedAttribute2);


    attributeResponse1 = new AttributeResponse();
    attributeResponse1.setId(ATTRIBUTE_ID1);
    attributeResponse1.setName(BUSINESS_PARTNER_NAME);
    attributeResponse1.setAllowedAttributeValues(listAllowedAttributeValueResponse);
    attributeResponse1.setAttributeType("DEFINING_ATTRIBUTE");

    attributeResponse2 = new AttributeResponse();
    attributeResponse2.setId(ATTRIBUTE_ID2);
    attributeResponse2.setName(BUSINESS_PARTNER_NAME);
    attributeResponse2.setAllowedAttributeValues(listAllowedAttributeValueResponse);
    attributeResponse2.setAttributeType("DESCRIPTIVE_ATTRIBUTE");

    attributeResponse3 = new AttributeResponse();
    attributeResponse3.setId(ATTRIBUTE_ID3);
    attributeResponse3.setName(BRAND);
    attributeResponse3.setPredefinedAllowedAttributeValues(listPredefinedAllowedAttribute);
    attributeResponse3.setAttributeType("PREDEFINED_ATTRIBUTE");

    attributeResponse4 = new AttributeResponse();
    attributeResponse4.setId(ATTRIBUTE_ID4);
    attributeResponse4.setName(GARANSI);
    attributeResponse4.setPredefinedAllowedAttributeValues(listPredefinedAllowedAttribute);
    attributeResponse4.setAttributeType("PREDEFINED_ATTRIBUTE");
    attributeResponse4.setSkuValue(true);


    categoryAttributeResponse1 = new CategoryAttributeResponse();
    categoryAttributeResponse1.setAttribute(attributeResponse1);
    categoryAttributeResponse2 = new CategoryAttributeResponse();
    categoryAttributeResponse2.setAttribute(attributeResponse2);
    categoryAttributeResponse3 = new CategoryAttributeResponse();
    categoryAttributeResponse3.setAttribute(attributeResponse3);
    categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse4);

    listCategoryAttributeResponse.add(categoryAttributeResponse1);
    listCategoryAttributeResponse.add(categoryAttributeResponse2);
    listCategoryAttributeResponse.add(categoryAttributeResponse3);

    categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setName(BUSINESS_PARTNER_CATEGORY);
    categoryDetailResponse.setId(ID);
    categoryDetailResponse.setCategoryAttributes(listCategoryAttributeResponse);
    categoryDetailResponse.getCategoryAttributes().get(0).setAttribute(attributeResponse1);
    categoryDetailResponse.getCategoryAttributes().get(1).setAttribute(attributeResponse2);
    categoryDetailResponse.getCategoryAttributes().get(2).setAttribute(attributeResponse3);

    ReflectionTestUtils.setField(productService, "pickupPointNameConcat", true);
    ReflectionTestUtils.setField(productService, "ppNameDelimiter", "||");
    ReflectionTestUtils.setField(productService, "templateDirectory", "./src/test/resources/Excel-template/");
    ReflectionTestUtils.setField(productService, "categoryUploadTemplateFile", "products-upload-template.xlsx");
    ReflectionTestUtils.setField(productService, "categoryUploadTemplateFileEnglish", "products-template-upload-English.xlsx");
    ReflectionTestUtils.setField(productService, "inventoryApiBatchSize", 10);
    ReflectionTestUtils.setField(productService, "pickupPointCodeBatchSize", 100);
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", false);
    ReflectionTestUtils.setField(productService, "bopisUnsupportedMerchantTypes", "CM,CC");
    ReflectionTestUtils.setField(productService, "validateSellerItemBasicDetails", false);
    ReflectionTestUtils.setField(productService, "itemBasicDetailsValidationRelaxForSellerTypes", "TD");

    bulkRequest = new BulkRequest();
    bulkRequest.setGdnSkus(Arrays.asList(ITEM_SKU));

//    this.httpServletResponse = new HttpServletResponse();

    productLevel3WebRequest = new ProductLevel3WebRequest();
    productLevel3UpdateWebRequest = new ProductLevel3UpdateWebRequest();
    productLevel3UpdateRequest = new ProductLevel3UpdateRequest();
    productLevel3UpdateRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productLevel3UpdateRequest.setAccessChannel(UpdateProductAccessChannel.MTA_WEB_UPDATE_DETAIL.getDesc());

    productLevel3ResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(new ProductLevel3Response(), REQUEST_ID);

    predefinedAllowedAttributeValueResponse1 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse1.setValue(BRAND);
    predefinedAllowedAttributeValueResponse1.setUpdatedDate(new Date());

    predefinedAttributeValueWebResponse =  new PredefinedAttributeValueWebResponse();
    predefinedAttributeValueWebResponse.setValue(Constants.OEM);

    predefinedAttributeValueWebResponse2 =  new PredefinedAttributeValueWebResponse();
    predefinedAttributeValueWebResponse2.setValue(Constants.NO_BRAND);

    predefinedAttributeValueWebResponse1 =  new PredefinedAttributeValueWebResponse();
    predefinedAttributeValueWebResponse1.setValue(BRAND);

    predefinedAllowedAttributeValueResponse2 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse2.setValue(BRAND1);
    predefinedAllowedAttributeValueResponse2.setUpdatedDate(new Date());

    ProductItemLevel3OrderResponse productItemLevel3OrderResponse = new ProductItemLevel3OrderResponse();
    productItemLevel3OrderResponse.setPrices(Arrays.asList(productLevel3PriceResponse));

    level3OrderResponse = new ProductLevel3OrderResponse();
    level3OrderResponse.setBrand(BRAND);
    level3OrderResponse.setItems(Arrays.asList(productItemLevel3OrderResponse));
    level3OrderResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(level3OrderResponse, REQUEST_ID);

    productItemLevel3Request = new ProductItemLevel3Request();
    productItemLevel3Request.setPickupPointCode(BUSINESSPARTNER_CODE);
    productItemLevel3Request.setItemSku(ITEM_SKU);
    productItemLevel3Request.setDeltaStock(DELTA_STOCK);
    productItemLevel3Request.setPrices(Arrays.asList(productLevel3PriceRequest));

    productLevel3Request = new ProductLevel3Request();
    productLevel3Request.setProductName(PRODUCT_NAME);
    productLevel3Request.setBrand(BRAND);
    productLevel3Request.setCategoryName(CATEGORY_NAME_1);
    productLevel3Request.setDescription(PRODUCT_DESCRIPTION);
    productLevel3Request.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productLevel3Request.setItems(Arrays.asList(productItemLevel3Request));

    productLevel3Response = new ProductLevel3Response();
    productLevel3Response.setCategoryId(CATEGORY_ID);
    productResponse = new GdnRestSingleResponse<>(productLevel3Response, REQUEST_ID);

    upcCodeSearchRequest = new UPCCodeSearchRequest();
    upcCodeSearchRequest.setUpcCode(UPC_CODE);
    upcCodeSearchRequest.setCategoryCodes(Arrays.asList(CATEGORY_CODE));

    profileResponse.setCompany(company);

    activeProductRequest2 = new ActiveProductRequest();
    activeProductRequest2.setMerchantCode(BUSINESSPARTNER_CODE);
    activeProductRequest2.setSearchKey(ITEM_SKU);

    updateImageRequest = new UpdateImageRequest();
    ProductLevel3ImageRequest productLevel3ImageRequest = new ProductLevel3ImageRequest();
    productLevel3ImageRequest.setLocationPath(LOCATION_PATH);
    productLevel3ImageRequest.setSequence(0);
    productLevel3ImageRequest.setMainImage(true);
    updateImageRequest.setProductSku(PRODUCT_SKU);
    updateImageRequest.setProductCode(PRODUCT_CODE);
    updateImageRequest.setItemSku(ITEM_SKU);
    updateImageRequest.setMasterDataItemImages(Arrays.asList(productLevel3ImageRequest));

    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(ITEM_SKU);
    productCampaignAvailabilityRequest.setItemSkus(itemSkuSet);
    productCampaignAvailabilityRequest
        .setItemInfo(
            Collections.singleton(new com.gdn.x.campaign.dto.ItemInfoDto(ITEM_SKU, PICKUP_POINT_CODE, StringUtils.EMPTY)));
    productCampaignAvailabilityRequest.setMerchantCode(BUSINESSPARTNER_CODE);

    Map<String, Boolean> productCampaignAvailabilityMap = new HashMap<>();
    productCampaignAvailabilityMap.put(ITEM_SKU, true);
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(productCampaignAvailabilityMap);

    productPriceAndStockUpdateWebRequest.setItemSku(ITEM_SKU);

    priceUpdateWebRequest.setPrice(NORMAL_PRICE);
    priceUpdateWebRequest.setSalePrice(SALES_PRICE);
    productPriceAndStockUpdateWebRequest.setPrices(Arrays.asList(priceUpdateWebRequest));

    ReflectionTestUtils.setField(productService, "productDetailPageUrlPrefix", PRODUCT_DETAIL_LINK);
    ReflectionTestUtils.setField(productService, "youTubeDataApiKey", API_KEY);
    youTubeAPIWebResponse = YouTubeAPIWebResponse.builder().isValid(true).build();
    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(2);
    videoListResponse.setPageInfo(pageInfo);
    Video video = new Video();
    video.setId(ID);
    VideoSnippet videoSnippet = new VideoSnippet();
    videoSnippet.setCategoryId(ID);
    videoSnippet.setDescription(DESCRIPTION);
    ThumbnailDetails thumbnailDetails = new ThumbnailDetails();
    Thumbnail thumbnail = new Thumbnail();
    thumbnail.setUrl(URL);
    thumbnailDetails.setDefault(thumbnail);
    videoSnippet.setThumbnails(thumbnailDetails);
    VideoLocalization videoLocalization = new VideoLocalization();
    videoLocalization.setDescription(DESCRIPTION);
    videoLocalization.setTitle(TITLE);
    videoListResponse.setItems(Arrays.asList(video));
    videoSnippet.setLocalized(videoLocalization);
    videoListResponse.getItems().get(0).setSnippet(videoSnippet);

    brandPredefinedAttributeValueResponse = new BrandPredefinedAttributeValueResponse();
    brandPredefinedAttributeValueResponse.setId(ID);
    brandPredefinedAttributeValueResponse.setValue(BRAND);
    brandPredefinedAttributeValueResponse.setBrandRequestCode(BRAND_CODE);
    brandPredefinedAttributeValueResponse.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_APPROVED);
    brandPredefinedAttributeValueResponse.setPredefinedAllowedAttributeCode(BRAND_CODE_1);
    brandPredefinedAttributeValueResponse.setSequence(10);

    itemSkusSet.add(ITEM_SKU);
    wholesalePriceSkuDetailListRequest = new WholesalePriceSkuDetailListRequest();
    wholesalePriceSkuDetailListRequest.setItemSkus(itemSkuSet);
    priceChangeCompatibleRequests.add(new PriceChangeCompatibleRequest(CATEGORY_CODE, ITEM_SKU, 100000.0, PICKUP_POINT_CODE));
    priceChangeCompatibleRequests_1.add(new PriceChangeCompatibleRequest(CATEGORY_CODE, ITEM_SKU, 90000.0, PICKUP_POINT_CODE));
    minWholesaleDiscountResponse = MinWholesaleDiscountResponse.builder().price(100000.0).percentage(10.0).build();
    wholesaleConfigResponse =
        WholesaleConfigResponse.builder().minWholesaleDiscount(Arrays.asList(minWholesaleDiscountResponse)).quantity(2)
            .build();
    wholesaleMappingResponse =
        WholesaleMappingResponse.builder().wholesaleConfig(Arrays.asList(wholesaleConfigResponse))
            .configurationType("PRICE_PERCENTAGE").build();
    wholesaleRules.put(2, 10.0);
    wholesaleRules.put(3, 15.0);
    wholesalePriceSkuResponse =
        WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus("ACTIVE").wholesaleRules(wholesaleRules).pickUpPointCode(PICKUP_POINT_CODE)
            .build();
    wholesalePriceSkuResponseMap
        .put(ITEM_SKU + Constants.DASH_SEPARATOR + PICKUP_POINT_CODE, wholesalePriceSkuResponse);

    switchVariableMap.put(SWITCH_VARIABLE, Boolean.TRUE);

    systemParameterConfigResponse = new SystemParameterConfigResponse();
    systemParameterConfigResponse.setUpdatedDate(new Date());
    systemParameterConfigResponse.setVariable(Constants.CATEGORY_BRAND_SCHEDULER_RUN_TIME);

    productEditInfoWebRequest = new ProductEditInfoWebRequest();
    productEditInfoWebRequest.setProductCode(PRODUCT_CODE);
    productEditInfoWebRequest.setProductSku(PRODUCT_SKU);
    productEditInfoWebRequest.setBrand(BRAND);
    productEditInfoWebRequest.setCategoryCode(CATEGORY_CODE);
    productEditInfoWebRequest.setBusinessPartnerCode(MERCHANT_CODE);
    productEditInfoWebRequest.setDescription(DESCRIPTION);

    editProductResponse = new EditProductV2Response();
    editProductResponse.setProductReview(true);
    editProductResponse.setReviewType(REVIEW_TYPE);
    editProductResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(editProductResponse, REQUEST_ID);

    editProductWebResponse = new EditProductWebResponse();
    editProductWebResponse.setProductReview(true);
    editProductWebResponse.setReviewType(REVIEW_TYPE);

    ProductLevel3SummaryDetailsResponse productLevel3SummaryDetailsResponse = new ProductLevel3SummaryDetailsResponse();
    productLevel3SummaryDetailsResponse.setIsArchived(Boolean.TRUE);
    productLevel3SummaryDetailsResponse.setItemName(ITEM_NAME);
    productLevel3SummaryDetailsResponse.setProductCode(PRODUCT_CODE);
    productLevel3SummaryDetailsResponse.setItemSku(ITEM_SKU);
    productLevel3SummaryDetailsResponse.setWholesalePriceActivated(Boolean.TRUE);
    productLevel3SummaryDetailsResponse.setProductScore(90.0);
    productLevel3SummaryDetailsResponse.setCogs(10000.0);
    productLevel3SummaryDetailsResponse.setCogsErrorCode("error");
    productLevel3SummaryDetailsResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(productLevel3SummaryDetailsResponse), pageMetaData, REQUEST_ID);
    preOrderRequest =
        PreOrderRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();
    uniquePickupPointCodeResponse = new UniquePickupPointCodeResponse();
    uniquePickupPointCodeResponse.setPickupPointCodes(new HashSet<>());
    uniquePickupPointCodeResponse.getPickupPointCodes().add(PICKUP_POINT_CODE);
    uniquePickupPointCodeResponse.setItemSkus(new HashSet<>());
    uniquePickupPointCodeResponse.getItemSkus().add(ITEM_SKU);

    pickupPointUpdateWebRequest = new PickupPointUpdateWebRequest();
    pickupPointUpdateWebRequest.setProductSku(PRODUCT_SKU);
    pickupPointUpdateWebRequest.setDifferentLocation(true);
    pickupPointUpdateWebRequest.setMarkDefaultAddress(true);
    pickupPointUpdateWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);
    PickupPointUpdateItemsWebRequest pickupPointUpdateItemsWebRequest = new PickupPointUpdateItemsWebRequest();
    pickupPointUpdateItemsWebRequest.setItemSku(ITEM_SKU);
    pickupPointUpdateItemsWebRequest.setItemName(ITEM_NAME);
    pickupPointUpdateItemsWebRequest.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointUpdateWebRequest.setItemsPickupPoint(Arrays.asList(pickupPointUpdateItemsWebRequest));

    ReservedStockSummary reservedStockSummary = new ReservedStockSummary();
    reservedStockSummary.setReservedQuantity(10);
    ReservedStockSummary reservedStockSummary1 = new ReservedStockSummary();
    reservedStockSummary1.setReservedQuantity(7);
    reservedStockSummaryResponse.setPendingCartReserved(Arrays.asList(reservedStockSummary, reservedStockSummary1));
    reservedStockSummaryResponse.setPendingPaymentReserved(Collections.singletonList(reservedStockSummary));

    priceDTO = new PriceDTO();
    priceDTO.setCurrency("currency");
    priceDTO.setListOfDiscountPrices(Arrays
        .asList(new DiscountPriceDTO(10.0, AdjustmentTypeEnum.BLIBLI, new Date(), new Date(), DEFAULT_CLIENT_HOST)));
    productSummaryRequest =
        ProductSummaryRequest.builder().archived(true).categoryCodes(Arrays.asList(CATEGORY_CODE)).inStock(true)
            .keyword(PRODUCT_NAME).merchantCode(MERCHANT_CODE).pickupPointCodes(Arrays.asList(PICKUP_POINT_CODE))
            .promoTypes(Arrays.asList(PROMO_TYPE)).sortField("CRETAED_DATE").sortOrder("ASC").build();
    itemL4SummaryResponse = ItemL4SummaryResponse.builder()
        .activePromoBundlings(new HashSet<>(Arrays.asList("activePromoBundlings", "activePromoBundlings1")))
        .cncActivated(true).forceReview(true).createdDate(new Date()).generatedItemName(ITEM_NAME)
        .isLateFulfillment(true).itemCode(ITEM_CODE).itemSku(ITEM_SKU).wholesalePriceExists(true)
        .pickupPointCode(PICKUP_POINT_CODE).priceEditDisabled(true).productType("REGULAR").promoBundling(true)
        .updatedDate(new Date()).itemViewConfigs(Arrays.asList(
            new ItemViewConfigDTO(true, true, CHANGED_BY, new ItemDiscoverableScheduleDTO(),
                new ItemBuyableScheduleDTO()))).markForDelete(false).merchantPromoDiscount(true)
        .merchantPromoDiscountActivated(true).merchantSku(MERCHANT_CODE).promoTypes(Arrays.asList("PROMO"))
        .promoLabels(Arrays.asList("PROMO_LABEL")).version((long) 2).wholesalePriceActivated(true)
        .masterDataItemImages(Arrays.asList(new MasterDataItemImageDTO(true, IMAGE_URL, 2))).storeId(STORE_ID)
        .price(new HashSet<>(Arrays.asList(priceDTO))).build();
    productL3SummaryResponse =
        ProductL3SummaryResponse.builder().brand(BRAND).catalogCode(CATEGORY_CODE_2).categoryCode(CATEGORY_CODE)
            .isArchived(true).markForDelete(false).maxNormalPrice(100).maxSellingPrice(30).variantCount(1)
            .merchantCode(BUSINESSPARTNER_CODE).minNormalPrice(10).minSellingPrice(35).off2OnChannelActive(true)
            .productCode(PRODUCT_CODE).productMainImage(IMAGE_URL).productName(PRODUCT_NAME).productScore(
            com.gdn.x.product.rest.web.model.response.ProductScoreResponse.builder().descriptionScore(30)
                .eanUpcScore(10).imageScore(40).mandatoryAttributeScore(50).productTitleScore(30)
                .recommendedAttributeScore(56).remainingAttributeScore(4).totalScore(56).uspScore(45)
                .variantCreatingScore(6).videoUrlScore(7).build()).productSku(PRODUCT_SKU_NEW)
            .promoLabels(Arrays.asList("PROMO")).suspended(true).itemL4SummaryResponse(itemL4SummaryResponse).build();
    productL3SummaryResponse.setCreatedDate(new Date());
    productL3SummaryResponse.setUpdatedDate(new Date());
    productL3SummaryResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(productL3SummaryResponse), pageMetaData, REQUEST_ID);
    Map<String, String > categoryName = new HashMap<>();
    categoryName.put(CATEGORY_CODE, CATEGORY_NAME_1);
    categoryNamesResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(new CategoryNamesResponse(categoryName), REQUEST_ID);
    webInventoryResponseDTO = new WebInventoryResponseDTO();
    webInventoryResponseDTO.setAvailableStock(10);
    webInventoryResponseDTO.setOriginalStock(40);
    webInventoryResponseDTO.setSyncStock(true);
    warehouseInventoryResponseDTO = new WarehouseInventoryResponseDTO();
    warehouseInventoryResponseDTO.setAvailableStock(10);
    warehouseInventoryResponseDTO.setOriginalStock(90);
    inventoryStockInfoDTO = new InventoryStockInfoDTO();
    inventoryStockInfoDTO.setWarehouseTotalAvailableStock(10);
    inventoryStockInfoDTO.setWarehouseTotalOriginalStock(40);
    inventoryStockInfoDTO.setWebProductSku(PRODUCT_SKU_NEW);
    inventoryDetailInfoResponseDTO = new InventoryDetailInfoResponseDTO();
    inventoryDetailInfoResponseDTO.setWebInventoryResponse(webInventoryResponseDTO);
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(Arrays.asList(warehouseInventoryResponseDTO));
    inventoryDetailInfoResponseDTO.setWebItemSku(ITEM_SKU);
    campaignPriceSkuResponse =
        CampaignPriceSkuResponse.builder().lockPriceUpdate(true).live(true).registered(true).campaignPrice(90.0)
            .maxAllowedPrice(100).build();
    Map<String, CampaignPriceSkuResponse> campMap = new HashMap<>();
    campMap.put(ITEM_SKU, campaignPriceSkuResponse);
    campaignPriceResponse = CampaignPriceResponse.builder().itemSkuToPriceResponse(campMap).build();
    campaignPriceRequest = CampaignPriceRequest.builder().campaignPriceSkuRequestList(
      Collections.singletonList(new CampaignPriceSkuRequest(
        productL3SummaryResponse.getItemL4SummaryResponse().getItemSku(),
        productL3SummaryResponse.getCategoryCode(),
        productL3SummaryResponse.getItemL4SummaryResponse().getPickupPointCode(), 0.0))).build();
    campaignPriceRequest1 = CampaignPriceRequest.builder().campaignPriceSkuRequestList(
      Collections.singletonList(CampaignPriceSkuRequest.builder()
        .itemSku(productL3SummaryResponse.getItemL4SummaryResponse().getItemSku()).pickUpPointCode(PICKUP_POINT_CODE)
        .categoryCode(productL3SummaryResponse.getCategoryCode()).sellingPrice(0.0).build())).build();
    productSuspensionHistoryResponse =
        ProductSuspensionHistoryResponse.builder().productSku(PRODUCT_SKU_NEW).reason(SUSPENSION_REASON).build();
    productSuspensionHistoryResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(productSuspensionHistoryResponse), pageMetaData, REQUEST_ID);

    ReflectionTestUtils.setField(productService, "preOrderMaximumDays", 90);
    ReflectionTestUtils.setField(productService, "preOrderMaximumWeek", 13);
    productSummaryWebRequest =
        ProductSummaryWebRequest.builder().categoryCodes(Arrays.asList(CATEGORY_CODE)).build();
    productSystemParameterResponse1 = new ProductSystemParameterResponse();
    productSystemParameterResponse1.setVariable(Constants.SHOW_L3_STOCK);
    productSystemParameterResponse1.setValue("true");
    when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse1, REQUEST_ID));
    historyUpdateWebRequest =
      HistoryUpdateWebRequest.builder().productSku(PRODUCT_SKU).beforeOneMonths(true)
        .endDate(new Date()).keyword(KEYWORD).pickupPointCode(PICKUP_POINT_CODE).endDate(new Date())
        .startDate(new Date()).build();
    historyUpdateResponse =
      HistoryUpdateResponse.builder().activity(ACTIVITY).changedBy(CHANGED_BY).gdnName(PRODUCT_NAME)
        .gdnSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).pickupPointName(PICKUP_POINT_CODE_1)
        .oldValues(PRODUCT_NAME).newValues(PRODUCT_NAME_2).build();
    when(xProductFeign.getItemPickupPointCodeByItemSkus(any())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new ItemSkuPickupPointCodeResponse(GDN_SKU, PICKUP_POINT_CODE)),
            new PageMetaData(), REQUEST_ID));

    productVariantUpdateWebRequest = new ProductVariantUpdateWebRequest();
    productVariantUpdateWebRequest.setProductCode(PRODUCT_CODE);
    productVariantUpdateWebRequest.setProductSku(PRODUCT_SKU);
    ItemPickupPointWebRequest itemPickupPointWebRequest = new ItemPickupPointWebRequest();
    itemPickupPointWebRequest.setPickupPointId(PICKUP_POINT_CODE);
    productVariantUpdateWebRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointWebRequest));
    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        new ProductVariantPriceStockAndImagesWebRequest();
    productVariantPriceStockAndImagesWebRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointWebRequest));
    ProductLevel3CommonImageWebRequest productLevel3SummaryDetailsImageWebRequest =
        new ProductLevel3CommonImageWebRequest();
    productLevel3SummaryDetailsImageWebRequest.setLocationPath(LOCATION_PATH);
    productVariantPriceStockAndImagesWebRequest.setImages(
        Collections.singletonList(productLevel3SummaryDetailsImageWebRequest));
    productVariantUpdateWebRequest.setProductItems(
        Collections.singletonList(productVariantPriceStockAndImagesWebRequest));
    ItemPickupPointDeleteWebRequest itemPickupPointDeleteWebRequest = new ItemPickupPointDeleteWebRequest();
    itemPickupPointDeleteWebRequest.setItemSku(ITEM_SKU);
    productVariantUpdateWebRequest.setDeletePickupPoints(Collections.singletonList(itemPickupPointDeleteWebRequest));
    ProductLevel3SummaryDetailsImageWebRequest imageWebRequest = new ProductLevel3SummaryDetailsImageWebRequest();
    imageWebRequest.setLocationPath(LOCATION_PATH);
    productVariantUpdateWebRequest.setCommonImages(Collections.singletonList(imageWebRequest));

    brandInReviewResponse1 = new BrandInReviewResponse();
    brandInReviewResponse1.setBrandName(BRAND);
    brandInReviewResponse1.setBrandRequestCode(BRAND_CODE);
    brandInReviewResponse1.setState(BrandWipState.DRAFT);
    brandInReviewResponse1.setBusinessPartnerCode(INTERNAL);

    brandInReviewResponse2 = new BrandInReviewResponse();
    brandInReviewResponse2.setBrandName(BRAND1);
    brandInReviewResponse2.setBrandRequestCode(BRAND_CODE_1);
    brandInReviewResponse2.setState(BrandWipState.DRAFT);
    brandInReviewResponse2.setBusinessPartnerCode(OTHER);

    brandInReviewResponse3 = new BrandInReviewResponse();
    brandInReviewResponse3.setBrandName(BRAND_CODE);
    brandInReviewResponse3.setBrandRequestCode(BRAND1);
    brandInReviewResponse3.setState(BrandWipState.DRAFT);
    brandInReviewResponse3.setBusinessPartnerCode(BUSINESSPARTNER_CODE);

    wholeSaleDetailListWebRequests = new WholeSaleDetailListWebRequest();
    wholeSaleDetailListWebRequests.setItemSku(ITEM_SKU);
    wholeSaleDetailListWebRequests.setPickupPointCode(PICKUP_POINT_CODE);

    PickupPointOutboundResponse pickupPointResponse1 = new PickupPointOutboundResponse();
    BeanUtils.copyProperties(pickupPointDto, pickupPointResponse1);
    PickupPointOutboundResponse pickupPointResponse2 = new PickupPointOutboundResponse();
    BeanUtils.copyProperties(pickupPointDto1, pickupPointResponse2);
    pickupPointResponseList = Arrays.asList(pickupPointResponse1, pickupPointResponse2);

    when(gcsProperties.isEnabled()).thenReturn(false);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(String.valueOf(false));
    viewConfigResponseB2b = new ProductLevel3ViewConfigResponse();
    b2BResponse = new B2BResponse();
    viewConfigResponseB2b.setChannelId(Constants.B2B_CHANNEL);
  }

  private void populateProductLevel3SummaryRequest1() {
    productLevel3SummaryRequest1 = new ProductLevel3SummaryRequest();
    productLevel3SummaryRequest1.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    productLevel3SummaryRequest1.setPickupPointCodes(Arrays.asList(PICKUP_POINT_CODE));
    productLevel3SummaryRequest1.setSearchKey(SEARCH_KEY);
    productLevel3SummaryRequest1.setBuyable(true);
    productLevel3SummaryRequest1.setDisplayable(true);
    productLevel3SummaryRequest1.setInventoryFilter(ProductLevel3InventoryCriteria.AVAILABLE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(pcbFeign);
    verifyNoMoreInteractions(pbpFeign);
    verifyNoMoreInteractions(fileStorageService);
    verifyNoMoreInteractions(kafkaProducer);
    verifyNoMoreInteractions(businessPartnerService);
    verifyNoMoreInteractions(productPricingFeign);
    verifyNoMoreInteractions(productPricingService, xInventoryFeign);
    verifyNoMoreInteractions(pdtFeign);
    Mockito.verifyNoMoreInteractions(this.youTube);
    Mockito.verifyNoMoreInteractions(this.imageService);
    Mockito.verifyNoMoreInteractions(xProductFeign);
    Mockito.verifyNoMoreInteractions(productAssemblyService);
    Mockito.verifyNoMoreInteractions(userPicService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void getEstimatedPriceTest() {
    when(pbpFeign.getEstimatedPrice(ITEM_CODE, PRICE))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, estimateItemPriceResponse));
    List<EstimateItemPriceWebResponse> responseList =
        productService.getEstimatedPriceByItemCodes(Collections.singletonList(ITEM_CODE), PRICE);
    verify(pbpFeign).getEstimatedPrice(ITEM_CODE, PRICE);
    assertEquals(OFFER_PRICE, responseList.get(0).getOfferPrice(), 0);
  }

  @Test
  public void getEstimatedPriceExceptionTest() {
    Exception exception = null;
    when(pbpFeign.getEstimatedPrice(ITEM_CODE, PRICE))
        .thenReturn(new GdnRestSimpleResponse<>(null, null, false, REQUEST_ID, null));
    List<EstimateItemPriceWebResponse> responseList = null;
    try {
      responseList = productService.getEstimatedPriceByItemCodes(Collections.singletonList(ITEM_CODE), PRICE);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(exception.getClass(), ClientException.class);
      verify(pbpFeign).getEstimatedPrice(ITEM_CODE, PRICE);
      assertNull(responseList);
    }
  }

  @Test
  public void getProductDetailsByProductIdTest() {
    when(this.pcbFeign.getProductDetailsById(PRODUCT_ID)).thenReturn(response);
    ProductDetailWebResponse response = this.productService.getProductDetailsByProductId(PRODUCT_ID);
    verify(this.pcbFeign).getProductDetailsById(PRODUCT_ID);
    assertNotNull(response);
    assertEquals(response.getId(), PRODUCT_ID);
  }

  @Test
  public void getProductDetailsByProductIdClientExceptionTest() {
    Exception exception = null;
    when(this.pcbFeign.getProductDetailsById(PRODUCT_ID)).thenReturn(null);
    ProductDetailWebResponse response = null;
    try {
      response = this.productService.getProductDetailsByProductId(PRODUCT_ID);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(exception.getClass(), ClientException.class);
      verify(this.pcbFeign).getProductDetailsById(PRODUCT_ID);
      assertNull(response);
    }
  }

  @Test
  public void getProductDetailsByProductIdExceptionTest() {
    Exception exception = null;
    when(this.pcbFeign.getProductDetailsById(PRODUCT_ID)).thenThrow(ApplicationRuntimeException.class);
    ProductDetailWebResponse response = null;
    try {
      response = this.productService.getProductDetailsByProductId(PRODUCT_ID);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(exception.getClass(), ApplicationRuntimeException.class);
      verify(this.pcbFeign).getProductDetailsById(PRODUCT_ID);
      assertNull(response);
    }
  }

  @Test
  public void getProductItemsByKeywordAndCategoryCodesForProductNameTest() {
    when(this.pbpFeign
        .getProductItemsByNameAndCategoryCodes(eq(PAGE), eq(SIZE), any(ProductSearchRequest.class), eq(Boolean.TRUE)))
        .thenReturn(productItemDetailResponses);
    Page<ProductItemDetailWebResponse> response = this.productService
        .getProductItemsByKeywordAndCategoryCodes(PRODUCT_NAME, categoryCodes, PAGE, SIZE, Boolean.TRUE);
    verify(this.pbpFeign)
        .getProductItemsByNameAndCategoryCodes(eq(PAGE), eq(SIZE), any(ProductSearchRequest.class), eq(Boolean.TRUE));
    assertNotNull(response);
    assertEquals(response.getContent().get(0).getProductItemWebResponse().getId(), ID);
  }

  @Test
  public void getProductItemsByKeywordAndEmptyCategoryCodesForProductNameTest() {
    when(this.pbpFeign
        .getProductItemsByNameAndCategoryCodes(eq(PAGE), eq(SIZE), any(ProductSearchRequest.class), eq(Boolean.TRUE)))
        .thenReturn(productItemDetailResponses);
    Page<ProductItemDetailWebResponse> response = this.productService
        .getProductItemsByKeywordAndCategoryCodes(PRODUCT_NAME, null, PAGE, SIZE, Boolean.TRUE);
    verify(this.pbpFeign)
        .getProductItemsByNameAndCategoryCodes(eq(PAGE), eq(SIZE), any(ProductSearchRequest.class), eq(Boolean.TRUE));
    assertNotNull(response);
    assertEquals(response.getContent().get(0).getProductItemWebResponse().getId(), ID);
  }

  @Test
  public void getProductItemsByKeywordAndCategoryCodesExceptionTest() {
    when(this.pbpFeign
        .getProductItemsByNameAndCategoryCodes(eq(PAGE), eq(SIZE), any(ProductSearchRequest.class), eq(Boolean.TRUE)))
        .thenThrow(ClientException.class);
    Page<ProductItemDetailWebResponse> response = null;
    try {
      response =
          this.productService.getProductItemsByKeywordAndCategoryCodes(PRODUCT_NAME, categoryCodes, PAGE, SIZE, true);
    } catch (ClientException e) {
    } finally {
      verify(this.pbpFeign)
          .getProductItemsByNameAndCategoryCodes(eq(PAGE), eq(SIZE), any(ProductSearchRequest.class), eq(Boolean.TRUE));
      assertNull(response);
    }
  }


  @Test
  public void getProductItemsByKeywordAndCategoryCodesForUPCCodeTest() {
    when(this.pcbFeign
        .getProductItemsByUPCCodeAndCategory(eq(PAGE), eq(SIZE), any(UPCCodeSearchRequest.class), eq(Boolean.TRUE)))
        .thenReturn(productItemDetailResponses);
    Page<ProductItemDetailWebResponse> response = this.productService
        .getProductItemsByKeywordAndCategoryCodes(UPC_CODE, Arrays.asList(CATEGORY_CODE), PAGE, SIZE, Boolean.TRUE);
    verify(this.pcbFeign)
        .getProductItemsByUPCCodeAndCategory(eq(PAGE), eq(SIZE), eq(upcCodeSearchRequest), eq(Boolean.TRUE));
    assertNotNull(response);
    assertEquals(response.getContent().get(0).getProductItemWebResponse().getId(), ID);
  }

  @Test
  public void getProductItemsByKeywordAndCategoryCodesForUPCCodeNullCategoryCodeTest() {
    when(this.pbpFeign
        .getProductItemsByNameAndCategoryCodes(eq(PAGE), eq(SIZE), any(ProductSearchRequest.class), eq(Boolean.TRUE)))
        .thenReturn(productItemDetailResponses);
    Page<ProductItemDetailWebResponse> response = this.productService
        .getProductItemsByKeywordAndCategoryCodes(UPC_CODE, Arrays.asList(), PAGE, SIZE, Boolean.TRUE);
    verify(this.pbpFeign)
        .getProductItemsByNameAndCategoryCodes(eq(PAGE), eq(SIZE), any(ProductSearchRequest.class), eq(Boolean.TRUE));
    assertNotNull(response);
    assertEquals(response.getContent().get(0).getProductItemWebResponse().getId(), ID);
  }

  @Test
  public void getProductItemsByKeywordAndCategoryCodesForUPCCodeExceptionTest() {
    when(this.pcbFeign
        .getProductItemsByUPCCodeAndCategory(eq(PAGE), eq(SIZE), any(UPCCodeSearchRequest.class), eq(Boolean.TRUE)))
        .thenThrow(ClientException.class);
    Page<ProductItemDetailWebResponse> response = null;
    try {
      response = this.productService
          .getProductItemsByKeywordAndCategoryCodes(UPC_CODE, Arrays.asList(CATEGORY_CODE), PAGE, SIZE, true);
    } catch (ClientException e) {
    } finally {
      verify(this.pcbFeign)
          .getProductItemsByUPCCodeAndCategory(eq(PAGE), eq(SIZE), eq(upcCodeSearchRequest), eq(Boolean.TRUE));
      assertNull(response);
    }
  }


  @Test
  public void getProductSuggestionByProductIdExceptionTest() {
    when(this.pbpFeign.getProductItemSuggestions(PAGE, SIZE, PRODUCT_NAME, CATEGORY_ID))
        .thenThrow(ClientException.class);
    List<ProductItemDetailWebResponse> response = null;
    try {
      response =
          this.productService.getProductItemSuggestionsByItemNameAndCategoryId(PRODUCT_NAME, CATEGORY_ID, PAGE, SIZE);
    } catch (ClientException e) {
    } finally {
      verify(this.pcbFeign).getProductItemSuggestionsByItemNameAndCategoryId(PRODUCT_NAME, CATEGORY_ID, PAGE, SIZE);
      assertNull(response);
    }
  }

  @Test
  public void getProductSuggestionSuccessTrueTest() {
    when(this.pcbFeign.getProductItemSuggestionsByItemNameAndCategoryId(PRODUCT_NAME, CATEGORY_ID, PAGE, SIZE))
        .thenReturn(productItemDetailResponses);
    List<ProductItemDetailWebResponse> response =
        this.productService.getProductItemSuggestionsByItemNameAndCategoryId(PRODUCT_NAME, CATEGORY_ID, PAGE, SIZE);
    verify(this.pcbFeign).getProductItemSuggestionsByItemNameAndCategoryId(PRODUCT_NAME, CATEGORY_ID, PAGE, SIZE);
    assertNotNull(response);
    assertEquals(ID, response.get(0).getProductItemWebResponse().getId());
  }

  @Test
  public void testGetCategorySuggestions() {
    mockPBPCategoryHierarchyProductCountResponse();
    Pageable pageable = PageRequest.of(0, 100);
    ListBaseResponse<CategorySuggestionWebResponse> response =
        productService.getCategorySuggestions(KEYWORD, pageable, REQUEST_ID, false);
    verify(pbpFeign).getCategoryHierarchyByKeywordWithProductCount(pageable.getPageNumber(),
      pageable.getPageSize(), KEYWORD, null);
    List<CategorySuggestionWebResponse> responseContent = response.getContent();
    assertEquals(CATEGORY_CODE_4, responseContent.get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_3, responseContent.get(0).getChildCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_9, responseContent.get(0).getChildCategories().get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_2,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_5,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_1,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(0)
            .getCategoryCode());
    assertEquals(CATEGORY_CODE_6,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(1)
            .getCategoryCode());
    assertEquals(CATEGORY_CODE_8,
        responseContent.get(0).getChildCategories().get(1).getChildCategories().get(0).getCategoryCode());
    assertEquals(32, responseContent.get(0).getProductCount());
    assertEquals(31, responseContent.get(0).getChildCategories().get(0).getProductCount());
    assertEquals(1, responseContent.get(0).getChildCategories().get(1).getProductCount());
    assertEquals(21, responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getProductCount());
    assertEquals(8, responseContent.get(0).getChildCategories().get(0).getChildCategories().get(1).getProductCount());
    assertEquals(16,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(0)
            .getProductCount());
    assertEquals(4,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(1)
            .getProductCount());
    assertEquals(1, responseContent.get(0).getChildCategories().get(1).getChildCategories().get(0).getProductCount());
  }

  @Test
  public void testGetCategorySuggestionsNotUPCTest() {
    mockPBPCategoryHierarchyProductCountResponse();
    Pageable pageable = PageRequest.of(0, 100);
    ListBaseResponse<CategorySuggestionWebResponse> response =
        productService.getCategorySuggestions(KEYWORDS, pageable, REQUEST_ID, false);
    verify(pbpFeign)
        .getCategoryHierarchyByKeywordWithProductCount(pageable.getPageNumber(),
          pageable.getPageSize(), KEYWORDS, null);
    List<CategorySuggestionWebResponse> responseContent = response.getContent();
    assertEquals(CATEGORY_CODE_4, responseContent.get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_3, responseContent.get(0).getChildCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_9, responseContent.get(0).getChildCategories().get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_2,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_5,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_1,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(0)
            .getCategoryCode());
    assertEquals(CATEGORY_CODE_6,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(1)
            .getCategoryCode());
    assertEquals(CATEGORY_CODE_8,
        responseContent.get(0).getChildCategories().get(1).getChildCategories().get(0).getCategoryCode());
    assertEquals(32, responseContent.get(0).getProductCount());
    assertEquals(31, responseContent.get(0).getChildCategories().get(0).getProductCount());
    assertEquals(1, responseContent.get(0).getChildCategories().get(1).getProductCount());
    assertEquals(21, responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getProductCount());
    assertEquals(8, responseContent.get(0).getChildCategories().get(0).getChildCategories().get(1).getProductCount());
    assertEquals(16,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(0)
            .getProductCount());
    assertEquals(4,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(1)
            .getProductCount());
    assertEquals(1, responseContent.get(0).getChildCategories().get(1).getChildCategories().get(0).getProductCount());
  }

  @Test
  public void testGetCategorySuggestionsForUpcCode() {
    mockCategoryHierarchyResponse();
    Pageable pageable = PageRequest.of(0, 100);
    ListBaseResponse<CategorySuggestionWebResponse> response =
        productService.getCategorySuggestions(UPC_CODE, pageable, REQUEST_ID, false);
    verify(pcbFeign).getCategoryHierarchyByUPCCodeWithProductCount(UPC_CODE, false);
    List<CategorySuggestionWebResponse> responseContent = response.getContent();
    assertEquals(CATEGORY_CODE_4, responseContent.get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_3, responseContent.get(0).getChildCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_9, responseContent.get(0).getChildCategories().get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_2,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_5,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_1,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(0)
            .getCategoryCode());
    assertEquals(CATEGORY_CODE_6,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(1)
            .getCategoryCode());
    assertEquals(CATEGORY_CODE_8,
        responseContent.get(0).getChildCategories().get(1).getChildCategories().get(0).getCategoryCode());
    assertEquals(32, responseContent.get(0).getProductCount());
    assertEquals(31, responseContent.get(0).getChildCategories().get(0).getProductCount());
    assertEquals(1, responseContent.get(0).getChildCategories().get(1).getProductCount());
    assertEquals(21, responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getProductCount());
    assertEquals(8, responseContent.get(0).getChildCategories().get(0).getChildCategories().get(1).getProductCount());
    assertEquals(16,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(0)
            .getProductCount());
    assertEquals(4,
        responseContent.get(0).getChildCategories().get(0).getChildCategories().get(0).getChildCategories().get(1)
            .getProductCount());
    assertEquals(1, responseContent.get(0).getChildCategories().get(1).getChildCategories().get(0).getProductCount());
  }

  @Test
  public void getTopCategorySuggestionsTest() {
    mockPBPCategoryHierarchyProductCountResponse();
    Pageable pageable = PageRequest.of(0, 5);
    List<List<SimpleCategoryWebResponse>> response =
        productService.getTopCategorySuggestions(KEYWORD, pageable, REQUEST_ID);
    verify(pbpFeign).getCategoryHierarchyByKeywordWithProductCount(pageable.getPageNumber(),
      pageable.getPageSize(), KEYWORD, null);
    assertEquals(5, response.size());
    assertEquals(CATEGORY_CODE_1, response.get(0).get(0).getCategoryCode());
    assertEquals(CATEGORY_NAME_ENGLISH_1, response.get(0).get(0).getNameEnglish());
    assertEquals(CATEGORY_CODE_5, response.get(1).get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_6, response.get(2).get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_3, response.get(3).get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_8, response.get(4).get(0).getCategoryCode());
    assertEquals(CATEGORY_CODE_2, response.get(0).get(1).getCategoryCode());
    assertEquals(CATEGORY_NAME_ENGLISH_2, response.get(0).get(1).getNameEnglish());
    assertEquals(CATEGORY_CODE_3, response.get(1).get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_2, response.get(2).get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_4, response.get(3).get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_9, response.get(4).get(1).getCategoryCode());
    assertEquals(CATEGORY_CODE_3, response.get(0).get(2).getCategoryCode());
    assertEquals(CATEGORY_NAME_ENGLISH_3, response.get(0).get(2).getNameEnglish());
    assertEquals(CATEGORY_CODE_4, response.get(1).get(2).getCategoryCode());
    assertEquals(CATEGORY_CODE_3, response.get(2).get(2).getCategoryCode());
    assertEquals(CATEGORY_CODE_4, response.get(4).get(2).getCategoryCode());
    assertEquals(CATEGORY_CODE_4, response.get(0).get(3).getCategoryCode());
    assertEquals(CATEGORY_NAME_ENGLISH_4, response.get(0).get(3).getNameEnglish());
    assertEquals(CATEGORY_CODE_4, response.get(2).get(3).getCategoryCode());
  }

  @Test
  public void getTopCategorySuggestionsTest_1() {
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setId(CATEGORY_ID_1);
    categoryResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryResponse1.setName(CATEGORY_NAME_1);
    categoryResponse1.setParentCategoryId(CATEGORY_ID_2);
    categoryResponse1.setNameEnglish(CATEGORY_NAME_ENGLISH_1);
    CategoryHierarchyProductCountResponse categoryHierarchyProductCountResponse1 =
        new CategoryHierarchyProductCountResponse();
    categoryHierarchyProductCountResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryHierarchyProductCountResponse1.setCategoryId(CATEGORY_ID_1);
    categoryHierarchyProductCountResponse1.setProductCount(PRODUCT_COUNT_1);
    categoryHierarchyProductCountResponse1.setCategoryHierarchy(Collections.singletonList(categoryResponse1));
    List<CategoryHierarchyProductCountResponse> categoryHierarchyProductCountResponseList =
        Collections.singletonList(categoryHierarchyProductCountResponse1);

    GdnRestListResponse<CategoryHierarchyProductCountResponse> pbpCategoryHierarchyProductCountResponse =
        new GdnRestListResponse<>(categoryHierarchyProductCountResponseList, pageMetaData, REQUEST_ID);

    when(pbpFeign.getCategoryHierarchyByKeywordWithProductCount(anyInt(), anyInt(), any(),
      Mockito.isNull()))
        .thenReturn(pbpCategoryHierarchyProductCountResponse);
    Pageable pageable = PageRequest.of(0, 5);
    List<List<SimpleCategoryWebResponse>> response =
        productService.getTopCategorySuggestions(KEYWORD, pageable, REQUEST_ID);
    verify(pbpFeign)
        .getCategoryHierarchyByKeywordWithProductCount(pageable.getPageNumber(),
          pageable.getPageSize(), KEYWORD, null);
    assertEquals(0, response.size());
  }

  private void mockPBPCategoryHierarchyProductCountResponse() {
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setId(CATEGORY_ID_1);
    categoryResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryResponse1.setName(CATEGORY_NAME_1);
    categoryResponse1.setParentCategoryId(CATEGORY_ID_2);
    categoryResponse1.setNameEnglish(CATEGORY_NAME_ENGLISH_1);
    categoryResponse1.setActivated(true);

    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setId(CATEGORY_ID_2);
    categoryResponse2.setCategoryCode(CATEGORY_CODE_2);
    categoryResponse2.setName(CATEGORY_NAME_2);
    categoryResponse2.setParentCategoryId(CATEGORY_ID_3);
    categoryResponse2.setNameEnglish(CATEGORY_NAME_ENGLISH_2);
    categoryResponse2.setActivated(true);

    CategoryResponse categoryResponse3 = new CategoryResponse();
    categoryResponse3.setId(CATEGORY_ID_3);
    categoryResponse3.setCategoryCode(CATEGORY_CODE_3);
    categoryResponse3.setName(CATEGORY_NAME_3);
    categoryResponse3.setParentCategoryId(CATEGORY_ID_4);
    categoryResponse3.setNameEnglish(CATEGORY_NAME_ENGLISH_3);
    categoryResponse3.setActivated(true);

    CategoryResponse categoryResponse4 = new CategoryResponse();
    categoryResponse4.setId(CATEGORY_ID_4);
    categoryResponse4.setCategoryCode(CATEGORY_CODE_4);
    categoryResponse4.setName(CATEGORY_NAME_4);
    categoryResponse4.setParentCategoryId(null);
    categoryResponse4.setNameEnglish(CATEGORY_NAME_ENGLISH_4);
    categoryResponse4.setActivated(true);

    CategoryResponse categoryResponse5 = new CategoryResponse();
    categoryResponse5.setId(CATEGORY_ID_5);
    categoryResponse5.setCategoryCode(CATEGORY_CODE_5);
    categoryResponse5.setName(CATEGORY_NAME_5);
    categoryResponse5.setParentCategoryId(CATEGORY_ID_3);
    categoryResponse5.setActivated(true);

    CategoryResponse categoryResponse6 = new CategoryResponse();
    categoryResponse6.setId(CATEGORY_ID_6);
    categoryResponse6.setCategoryCode(CATEGORY_CODE_6);
    categoryResponse6.setName(CATEGORY_NAME_6);
    categoryResponse6.setParentCategoryId(CATEGORY_ID_2);
    categoryResponse6.setActivated(true);

    CategoryResponse categoryResponse7 = new CategoryResponse();
    categoryResponse7.setId(CATEGORY_ID_7);
    categoryResponse7.setCategoryCode(CATEGORY_CODE_7);
    categoryResponse7.setName(CATEGORY_NAME_7);
    categoryResponse7.setParentCategoryId(CATEGORY_ID_3);

    CategoryResponse categoryResponse8 = new CategoryResponse();
    categoryResponse8.setId(CATEGORY_ID_8);
    categoryResponse8.setCategoryCode(CATEGORY_CODE_8);
    categoryResponse8.setName(CATEGORY_NAME_8);
    categoryResponse8.setParentCategoryId(CATEGORY_ID_9);
    categoryResponse8.setActivated(true);

    CategoryResponse categoryResponse9 = new CategoryResponse();
    categoryResponse9.setId(CATEGORY_ID_9);
    categoryResponse9.setCategoryCode(CATEGORY_CODE_9);
    categoryResponse9.setName(CATEGORY_NAME_9);
    categoryResponse9.setParentCategoryId(CATEGORY_ID_4);
    categoryResponse9.setActivated(true);

    CategoryHierarchyProductCountResponse categoryHierarchyProductCountResponse1 =
        new CategoryHierarchyProductCountResponse();
    categoryHierarchyProductCountResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryHierarchyProductCountResponse1.setCategoryId(CATEGORY_ID_1);
    categoryHierarchyProductCountResponse1.setProductCount(PRODUCT_COUNT_1);
    categoryHierarchyProductCountResponse1.setCategoryHierarchy(
        Arrays.asList(categoryResponse1, categoryResponse2, categoryResponse3, categoryResponse4));

    CategoryHierarchyProductCountResponse categoryHierarchyProductCountResponse2 =
        new CategoryHierarchyProductCountResponse();
    categoryHierarchyProductCountResponse2.setCategoryCode(CATEGORY_CODE_5);
    categoryHierarchyProductCountResponse2.setCategoryId(CATEGORY_ID_5);
    categoryHierarchyProductCountResponse2.setProductCount(PRODUCT_COUNT_2);
    categoryHierarchyProductCountResponse2
        .setCategoryHierarchy(Arrays.asList(categoryResponse5, categoryResponse3, categoryResponse4));

    CategoryHierarchyProductCountResponse categoryHierarchyProductCountResponse3 =
        new CategoryHierarchyProductCountResponse();
    categoryHierarchyProductCountResponse3.setCategoryCode(CATEGORY_CODE_6);
    categoryHierarchyProductCountResponse3.setCategoryId(CATEGORY_ID_6);
    categoryHierarchyProductCountResponse3.setProductCount(PRODUCT_COUNT_3);
    categoryHierarchyProductCountResponse3.setCategoryHierarchy(
        Arrays.asList(categoryResponse6, categoryResponse2, categoryResponse3, categoryResponse4));

    CategoryHierarchyProductCountResponse categoryHierarchyProductCountResponse4 =
        new CategoryHierarchyProductCountResponse();
    categoryHierarchyProductCountResponse4.setCategoryCode(CATEGORY_CODE_7);
    categoryHierarchyProductCountResponse4.setCategoryId(CATEGORY_ID_7);
    categoryHierarchyProductCountResponse4.setProductCount(PRODUCT_COUNT_4);
    categoryHierarchyProductCountResponse4
        .setCategoryHierarchy(Arrays.asList(categoryResponse7, categoryResponse3, categoryResponse4));

    CategoryHierarchyProductCountResponse categoryHierarchyProductCountResponse5 =
        new CategoryHierarchyProductCountResponse();
    categoryHierarchyProductCountResponse5.setCategoryCode(CATEGORY_CODE_2);
    categoryHierarchyProductCountResponse5.setCategoryId(CATEGORY_ID_2);
    categoryHierarchyProductCountResponse5.setProductCount(PRODUCT_COUNT_5);
    categoryHierarchyProductCountResponse5
        .setCategoryHierarchy(Arrays.asList(categoryResponse2, categoryResponse3, categoryResponse4));

    CategoryHierarchyProductCountResponse categoryHierarchyProductCountResponse6 =
        new CategoryHierarchyProductCountResponse();
    categoryHierarchyProductCountResponse6.setCategoryCode(CATEGORY_CODE_8);
    categoryHierarchyProductCountResponse6.setCategoryId(CATEGORY_ID_8);
    categoryHierarchyProductCountResponse6.setProductCount(PRODUCT_COUNT_6);
    categoryHierarchyProductCountResponse6
        .setCategoryHierarchy(Arrays.asList(categoryResponse8, categoryResponse9, categoryResponse4));

    List<CategoryHierarchyProductCountResponse> categoryHierarchyProductCountResponseList = Arrays
        .asList(categoryHierarchyProductCountResponse2, categoryHierarchyProductCountResponse3,
            categoryHierarchyProductCountResponse1, categoryHierarchyProductCountResponse6,
            categoryHierarchyProductCountResponse5, categoryHierarchyProductCountResponse4);

    GdnRestListResponse<CategoryHierarchyProductCountResponse> pbpCategoryHierarchyProductCountResponse =
        new GdnRestListResponse<>(categoryHierarchyProductCountResponseList, pageMetaData, REQUEST_ID);

    when(pbpFeign.getCategoryHierarchyByKeywordWithProductCount(anyInt(), anyInt(), any(), Mockito.isNull()))
        .thenReturn(pbpCategoryHierarchyProductCountResponse);
  }

  private void mockCategoryHierarchyResponse() {
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setId(CATEGORY_ID_1);
    categoryResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryResponse1.setName(CATEGORY_NAME_1);
    categoryResponse1.setParentCategoryId(CATEGORY_ID_2);
    categoryResponse1.setNameEnglish(CATEGORY_NAME_ENGLISH_1);
    categoryResponse1.setActivated(true);

    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setId(CATEGORY_ID_2);
    categoryResponse2.setCategoryCode(CATEGORY_CODE_2);
    categoryResponse2.setName(CATEGORY_NAME_2);
    categoryResponse2.setParentCategoryId(CATEGORY_ID_3);
    categoryResponse2.setNameEnglish(CATEGORY_NAME_ENGLISH_2);
    categoryResponse2.setActivated(true);

    CategoryResponse categoryResponse3 = new CategoryResponse();
    categoryResponse3.setId(CATEGORY_ID_3);
    categoryResponse3.setCategoryCode(CATEGORY_CODE_3);
    categoryResponse3.setName(CATEGORY_NAME_3);
    categoryResponse3.setParentCategoryId(CATEGORY_ID_4);
    categoryResponse3.setNameEnglish(CATEGORY_NAME_ENGLISH_3);
    categoryResponse3.setActivated(true);

    CategoryResponse categoryResponse4 = new CategoryResponse();
    categoryResponse4.setId(CATEGORY_ID_4);
    categoryResponse4.setCategoryCode(CATEGORY_CODE_4);
    categoryResponse4.setName(CATEGORY_NAME_4);
    categoryResponse4.setParentCategoryId(null);
    categoryResponse4.setNameEnglish(CATEGORY_NAME_ENGLISH_4);
    categoryResponse4.setActivated(true);

    CategoryResponse categoryResponse5 = new CategoryResponse();
    categoryResponse5.setId(CATEGORY_ID_5);
    categoryResponse5.setCategoryCode(CATEGORY_CODE_5);
    categoryResponse5.setName(CATEGORY_NAME_5);
    categoryResponse5.setParentCategoryId(CATEGORY_ID_3);
    categoryResponse5.setActivated(true);

    CategoryResponse categoryResponse6 = new CategoryResponse();
    categoryResponse6.setId(CATEGORY_ID_6);
    categoryResponse6.setCategoryCode(CATEGORY_CODE_6);
    categoryResponse6.setName(CATEGORY_NAME_6);
    categoryResponse6.setParentCategoryId(CATEGORY_ID_2);
    categoryResponse6.setActivated(true);

    CategoryResponse categoryResponse7 = new CategoryResponse();
    categoryResponse7.setId(CATEGORY_ID_7);
    categoryResponse7.setCategoryCode(CATEGORY_CODE_7);
    categoryResponse7.setName(CATEGORY_NAME_7);
    categoryResponse7.setParentCategoryId(CATEGORY_ID_3);

    CategoryResponse categoryResponse8 = new CategoryResponse();
    categoryResponse8.setId(CATEGORY_ID_8);
    categoryResponse8.setCategoryCode(CATEGORY_CODE_8);
    categoryResponse8.setName(CATEGORY_NAME_8);
    categoryResponse8.setParentCategoryId(CATEGORY_ID_9);
    categoryResponse8.setActivated(true);

    CategoryResponse categoryResponse9 = new CategoryResponse();
    categoryResponse9.setId(CATEGORY_ID_9);
    categoryResponse9.setCategoryCode(CATEGORY_CODE_9);
    categoryResponse9.setName(CATEGORY_NAME_9);
    categoryResponse9.setParentCategoryId(CATEGORY_ID_4);
    categoryResponse9.setActivated(true);

    CategoryHierarchyResponse categoryHierarchyResponse1 = new CategoryHierarchyResponse();
    categoryHierarchyResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryHierarchyResponse1.setCategoryId(CATEGORY_ID_1);
    categoryHierarchyResponse1.setProductCount(PRODUCT_COUNT_1);
    categoryHierarchyResponse1.setCategoryHierarchy(
        Arrays.asList(categoryResponse1, categoryResponse2, categoryResponse3, categoryResponse4));

    CategoryHierarchyResponse categoryHierarchyResponse2 = new CategoryHierarchyResponse();
    categoryHierarchyResponse2.setCategoryCode(CATEGORY_CODE_5);
    categoryHierarchyResponse2.setCategoryId(CATEGORY_ID_5);
    categoryHierarchyResponse2.setProductCount(PRODUCT_COUNT_2);
    categoryHierarchyResponse2
        .setCategoryHierarchy(Arrays.asList(categoryResponse5, categoryResponse3, categoryResponse4));

    CategoryHierarchyResponse categoryHierarchyResponse3 = new CategoryHierarchyResponse();
    categoryHierarchyResponse3.setCategoryCode(CATEGORY_CODE_6);
    categoryHierarchyResponse3.setCategoryId(CATEGORY_ID_6);
    categoryHierarchyResponse3.setProductCount(PRODUCT_COUNT_3);
    categoryHierarchyResponse3.setCategoryHierarchy(
        Arrays.asList(categoryResponse6, categoryResponse2, categoryResponse3, categoryResponse4));

    CategoryHierarchyResponse categoryHierarchyResponse4 = new CategoryHierarchyResponse();
    categoryHierarchyResponse4.setCategoryCode(CATEGORY_CODE_7);
    categoryHierarchyResponse4.setCategoryId(CATEGORY_ID_7);
    categoryHierarchyResponse4.setProductCount(PRODUCT_COUNT_4);
    categoryHierarchyResponse4
        .setCategoryHierarchy(Arrays.asList(categoryResponse7, categoryResponse3, categoryResponse4));

    CategoryHierarchyResponse categoryHierarchyResponse5 = new CategoryHierarchyResponse();
    categoryHierarchyResponse5.setCategoryCode(CATEGORY_CODE_2);
    categoryHierarchyResponse5.setCategoryId(CATEGORY_ID_2);
    categoryHierarchyResponse5.setProductCount(PRODUCT_COUNT_5);
    categoryHierarchyResponse5
        .setCategoryHierarchy(Arrays.asList(categoryResponse2, categoryResponse3, categoryResponse4));

    CategoryHierarchyResponse categoryHierarchyResponse6 = new CategoryHierarchyResponse();
    categoryHierarchyResponse6.setCategoryCode(CATEGORY_CODE_8);
    categoryHierarchyResponse6.setCategoryId(CATEGORY_ID_8);
    categoryHierarchyResponse6.setProductCount(PRODUCT_COUNT_6);
    categoryHierarchyResponse6
        .setCategoryHierarchy(Arrays.asList(categoryResponse8, categoryResponse9, categoryResponse4));

    List<CategoryHierarchyResponse> categoryHierarchyProductCountResponseList = Arrays
        .asList(categoryHierarchyResponse2, categoryHierarchyResponse3, categoryHierarchyResponse1,
            categoryHierarchyResponse6, categoryHierarchyResponse5, categoryHierarchyResponse4);

    GdnRestListResponse<CategoryHierarchyResponse> categoryHierarchyResponse =
        new GdnRestListResponse<>(categoryHierarchyProductCountResponseList, pageMetaData, REQUEST_ID);

    when(pcbFeign.getCategoryHierarchyByUPCCodeWithProductCount(any(), anyBoolean()))
        .thenReturn(categoryHierarchyResponse);
  }

  @Test
  public void getProductsCountFromRedisTest() {
    when(this.boundValueOperations.get()).thenReturn(PRODUCTS_COUNT);
    productService.getProductsCount(true);
    verify(productRedisTemplate).boundValueOps(VIEWABLE_PRODUCT_COUNT);
    verify(boundValueOperations).get();
  }

  @Test
  public void getProductsCountTest() {
    when(pbpFeign.getProductsCountByViewable(true)).thenReturn(new GdnRestSimpleResponse<>(null, 1));
    int productsCount = productService.getProductsCount(true);
    verify(pbpFeign).getProductsCountByViewable(true);
    verify(productRedisTemplate, Mockito.times(2)).boundValueOps(VIEWABLE_PRODUCT_COUNT);
    verify(boundValueOperations).get();
    verify(boundValueOperations).set(String.valueOf(productsCount), REDIS_PRODUCT_COUNT_TIMEOUT, TimeUnit.HOURS);
    Assertions.assertEquals(1, productsCount);
  }

  @Test
  public void getProductsCountClientExceptionTest() {
    when(pbpFeign.getProductsCountByViewable(false)).thenReturn(null);
    try {
      productService.getProductsCount(false);
    } catch (ClientException e) {
    } finally {
      verify(pbpFeign).getProductsCountByViewable(false);
    }
  }

  @Test
  public void createProductTest_whenBrandApproved() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
      PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
      productItemCreationRequest.setPickupPoints(
          Collections.singletonList(pickupPointCreateRequest));
      productCreationRequest.setProductItemRequests(
          Collections.singletonList(productItemCreationRequest));
      productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_APPROVED);
      productCreationRequest.setPreOrder(preOrderRequest);
      Mockito.when(pbpFeign.createProduct(productCreationRequest, FLOW_TYPE))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
          .thenReturn(PRODUCT_CODE);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
          .thenReturn(profileResponse);
      CreateProductResponse response =
          this.productService.createProduct(USERNAME, productCreationRequest, BUSINESSPARTNER_CODE,
              FLOW_TYPE);
      Mockito.verify(pbpFeign).createProduct(productCreationRequest, FLOW_TYPE);
      Mockito.verify(pickupPointService)
          .validateAndSaveDefaultPickupPoint(USERNAME, productCreationRequest);
      Mockito.verify(categoryService)
          .findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(userPicService).validateUserPicPickupPointsForBusinessPartner(
          Collections.singleton(pickupPointCreateRequest.getPickupPointId()), profileResponse);
      Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    }
  }

  @Test
  public void createProductTest_flow3() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
      PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
      productItemCreationRequest.setPickupPoints(
          Collections.singletonList(pickupPointCreateRequest));
      productCreationRequest.setProductItemRequests(
          Collections.singletonList(productItemCreationRequest));
      productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_APPROVED);
      productCreationRequest.setPreOrder(preOrderRequest);
      Mockito.when(pbpFeign.createProduct(productCreationRequest, FLOW_TYPE_3))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
          .thenReturn(PRODUCT_CODE);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
          .thenReturn(profileResponse);
      CreateProductResponse response =
          this.productService.createProduct(USERNAME, productCreationRequest, BUSINESSPARTNER_CODE,
              FLOW_TYPE_3);
      Mockito.verify(pbpFeign).createProduct(productCreationRequest, FLOW_TYPE_3);
      Mockito.verify(pickupPointService)
          .validateAndSaveDefaultPickupPoint(USERNAME, productCreationRequest);
      Mockito.verify(categoryService)
          .findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    }
  }

  @Test
  public void createProductV2Test() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
      PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
      productItemCreationRequest.setPickupPoints(
          Collections.singletonList(pickupPointCreateRequest));
      productCreationRequest.setProductItemRequests(
          Collections.singletonList(productItemCreationRequest));
      productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_APPROVED);
      productCreationRequest.setPreOrder(preOrderRequest);
      Mockito.when(pbpFeign.createNewProduct(productCreationRequest, FLOW_TYPE))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
          .thenReturn(PRODUCT_CODE);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
          .thenReturn(profileResponse);
      CreateProductResponse response =
          this.productService.createProductV2(USERNAME, productCreationRequest,
              BUSINESSPARTNER_CODE, FLOW_TYPE);
      Mockito.verify(pbpFeign).createNewProduct(productCreationRequest, FLOW_TYPE);
      Mockito.verify(categoryService)
          .findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(userPicService).validateUserPicPickupPointsForBusinessPartner(
          productCreationRequest.getProductItemRequests().stream().flatMap(
                  productItemCreationRequest1 -> productItemCreationRequest1.getPickupPoints().stream())
              .map(PickupPointCreateRequest::getPickupPointId).collect(Collectors.toSet()),
          profileResponse);
      Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    }
  }
  @Test
  public void createProductV2SanitizeImagePathTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
      PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
      productItemCreationRequest.setPickupPoints(
          Collections.singletonList(pickupPointCreateRequest));
      productCreationRequest.setProductItemRequests(
          Collections.singletonList(productItemCreationRequest));
      productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_APPROVED);
      productCreationRequest.setPreOrder(preOrderRequest);
      productCreationRequest.setCommonImages(List.of(new Image()));
      Mockito.when(pbpFeign.createNewProduct(productCreationRequest, FLOW_TYPE))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
          .thenReturn(PRODUCT_CODE);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
          .thenReturn(profileResponse);
      CreateProductResponse response =
          this.productService.createProductV2(USERNAME, productCreationRequest,
              BUSINESSPARTNER_CODE, FLOW_TYPE);
      Mockito.verify(pbpFeign).createNewProduct(productCreationRequest, FLOW_TYPE);
      Mockito.verify(categoryService)
          .findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(userPicService).validateUserPicPickupPointsForBusinessPartner(
          productCreationRequest.getProductItemRequests().stream().flatMap(
                  productItemCreationRequest1 -> productItemCreationRequest1.getPickupPoints().stream())
              .map(PickupPointCreateRequest::getPickupPointId).collect(Collectors.toSet()),
          profileResponse);
      Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    }
  }

  @Test
  public void createProductTest_PreOrderException() throws Exception {
    profileResponse.setMerchantStatus(INACTIVE);
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      preOrderRequest.setPreOrderType(PROMO_TYPE);
      productCreationRequest.setPreOrder(preOrderRequest);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
          .thenReturn(profileResponse);
      try {
        Assertions.assertThrows(ApiIncorrectInputDataException.class,
            () -> this.productService.createProduct(USERNAME, productCreationRequest,
                BUSINESSPARTNER_CODE, FLOW_TYPE));
      } finally {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      }
    }
  }

  @Test
  public void createProductTest_whenBrandInDraft() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_DRAFT);
      gdnBrandWipResponse.getValue().setState(BRAND_APPROVAL_STATUS_APPROVED);
      Mockito.when(pcbFeign.getBrandDetail(BRAND_CODE)).thenReturn(gdnBrandWipResponse);
      Mockito.when(pbpFeign.createProduct(productCreationRequest, null))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
          .thenReturn(PRODUCT_CODE);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
          .thenReturn(profileResponse);

      CreateProductResponse response =
          this.productService.createProduct(USERNAME, productCreationRequest, BUSINESSPARTNER_CODE,
              null);
      Mockito.verify(pcbFeign).getBrandDetail(BRAND_CODE);
      Mockito.verify(pbpFeign).createProduct(productCreationRequest, null);
      Mockito.verify(pickupPointService)
          .validateAndSaveDefaultPickupPoint(USERNAME, productCreationRequest);
      Mockito.verify(categoryService)
          .findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
      Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(userPicService).validateUserPicPickupPointsForBusinessPartner(
          productCreationRequest.getProductItemRequests().stream().flatMap(
                  productItemCreationRequest -> productItemCreationRequest.getPickupPoints().stream())
              .map(PickupPointCreateRequest::getPickupPointId).collect(Collectors.toSet()),
          profileResponse);
      Assertions.assertEquals(productCreationRequest.getBrandApprovalStatus(),
          gdnBrandWipResponse.getValue().getState());
    }
  }

  @Test
  public void createProduct_whenBrandApprovedAndPBPClientExceptionTest() throws Exception {
    ReflectionTestUtils.setField(productService, "productCreationResponseValidationSwitch", true);
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      CreateProductResponse response = null;
      productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_APPROVED);
      Mockito.when(pbpFeign.createProduct(productCreationRequest, FLOW_TYPE)).thenReturn(null);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
          .thenReturn(profileResponse);
      try {
        response = this.productService.createProduct(USERNAME, productCreationRequest,
            BUSINESSPARTNER_CODE, FLOW_TYPE);
      } catch (ClientException ex) {

      } finally {
        Mockito.verify(userPicService).validateUserPicPickupPointsForBusinessPartner(
            productCreationRequest.getProductItemRequests().stream().flatMap(
                    productItemCreationRequest -> productItemCreationRequest.getPickupPoints().stream())
                .map(PickupPointCreateRequest::getPickupPointId).collect(Collectors.toSet()),
            profileResponse);
        Mockito.verify(pbpFeign).createProduct(productCreationRequest, FLOW_TYPE);
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
        assertNull(response);
      }
    }
  }

  @Test
  public void createProduct_whenBrandInDraftAndPCBClientExceptionTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_DRAFT);
      CreateProductResponse response = null;
      Mockito.when(pcbFeign.getBrandDetail(BRAND_CODE)).thenReturn(null);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
          .thenReturn(profileResponse);
      try {
        response = this.productService.createProduct(USERNAME, productCreationRequest,
            BUSINESSPARTNER_CODE, FLOW_TYPE);
      } catch (ClientException ex) {

      } finally {
        Mockito.verify(userPicService).validateUserPicPickupPointsForBusinessPartner(
            productCreationRequest.getProductItemRequests().stream().flatMap(
                    productItemCreationRequest -> productItemCreationRequest.getPickupPoints().stream())
                .map(PickupPointCreateRequest::getPickupPointId).collect(Collectors.toSet()),
            profileResponse);
        Mockito.verify(pcbFeign).getBrandDetail(BRAND_CODE);
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
        assertNull(response);
      }
    }
  }

  @Test
  public void createProduct_whenBusinessPartnerCodeNullTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_APPROVED);
      productCreationRequest.setBusinessPartnerCode(null);
      Mockito.when(pbpFeign.createProduct(productCreationRequest, FLOW_TYPE))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
          .thenReturn(PRODUCT_CODE);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
          .thenReturn(profileResponse);
      CreateProductResponse response =
          this.productService.createProduct(USERNAME, productCreationRequest, BUSINESSPARTNER_CODE,
              FLOW_TYPE);
      Mockito.verify(pbpFeign).createProduct(productCreationRequest, FLOW_TYPE);
      Mockito.verify(categoryService)
          .findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(userPicService).validateUserPicPickupPointsForBusinessPartner(
          productCreationRequest.getProductItemRequests().stream().flatMap(
                  productItemCreationRequest -> productItemCreationRequest.getPickupPoints().stream())
              .map(PickupPointCreateRequest::getPickupPointId).collect(Collectors.toSet()),
          profileResponse);
      Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    }
  }

  @Test
  public void createProduct_withNullBusinessPartnerCodeTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_APPROVED);
      productCreationRequest.setBusinessPartnerCode(null);
      Mockito.when(pbpFeign.createProduct(productCreationRequest, FLOW_TYPE))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
          .thenReturn(PRODUCT_CODE);
      CreateProductResponse response =
          this.productService.createProduct(USERNAME, productCreationRequest, null, FLOW_TYPE);
      Mockito.verify(pbpFeign).createProduct(productCreationRequest, FLOW_TYPE);
      Mockito.verify(categoryService)
          .findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
      Mockito.verify(businessPartnerService, times(0))
          .filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(userPicService).validateUserPicPickupPointsForBusinessPartner(
          productCreationRequest.getProductItemRequests().stream().flatMap(
                  productItemCreationRequest -> productItemCreationRequest.getPickupPoints().stream())
              .map(PickupPointCreateRequest::getPickupPointId).collect(Collectors.toSet()), null);
      Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    }
  }

  @Test
  public void createProduct_isLogisticsSaveSuccessFalse() throws Exception {
    ReflectionTestUtils.setField(productService, "productCreationResponseValidationSwitch", true);
    GdnBaseRestResponse pbpCreateResponse =
      new GdnBaseRestResponse("ERR-PBP400008", "ERR-PBP400008", true, null);
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_APPROVED);
      productCreationRequest.setBusinessPartnerCode(null);
      Mockito.when(pbpFeign.createProduct(productCreationRequest, FLOW_TYPE))
          .thenReturn(pbpCreateResponse);
      Mockito.when(
              categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
          .thenReturn(PRODUCT_CODE);
      try {
        Assertions.assertThrows(ApiIncorrectInputDataException.class,
            () -> this.productService.createProduct(USERNAME, productCreationRequest, null,
                FLOW_TYPE));
      } finally {
        Mockito.verify(pbpFeign).createProduct(productCreationRequest, FLOW_TYPE);
        Mockito.verify(businessPartnerService, times(0))
            .filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
        Mockito.verify(userPicService).validateUserPicPickupPointsForBusinessPartner(
            productCreationRequest.getProductItemRequests().stream().flatMap(
                    productItemCreationRequest -> productItemCreationRequest.getPickupPoints().stream())
                .map(PickupPointCreateRequest::getPickupPointId).collect(Collectors.toSet()), null);
      }
    }
  }

  @Test
  public void createProduct_ExceptionUnauthorizeErrorTest() throws Exception {
    String[] accessibility = {};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      Assertions.assertThrows(UnauthorizedException.class,
          () -> this.productService.createProduct(USERNAME, productCreationRequest,
              BUSINESSPARTNER_CODE, FLOW_TYPE));
    }
  }

  @Test
  public void createProductApiIncorrectInputDataExceptionTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      preOrderRequest.setPreOrderType(PROMO_TYPE);
      productCreationRequest.setPreOrder(preOrderRequest);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
          .thenReturn(null);
      try {
        Assertions.assertThrows(ApiIncorrectInputDataException.class,
            () -> this.productService.createProduct(USERNAME, productCreationRequest,
                BUSINESSPARTNER_CODE, FLOW_TYPE));
      } finally {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      }
    }
  }

  @Test
  public void getProductListByMerchantAndCategoryCodeTest() {
    when(this.pcbFeign.getAllChildCategoryCodesByC1CategoryCode(any(CategoryCodeRequest.class)))
        .thenReturn(categoryCodeResponseGdnRestSingleResponse);
    when(this.xProductFeign.getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequest))
        .thenReturn(activeProductResponseGdnRestListResponse);
    Page<ActiveProductWebResponse> response =
        this.productService.getProductListByMerchantAndCategoryCode(activeProductWebRequest, MERCHANT_CODE);
    verify(this.pcbFeign).getAllChildCategoryCodesByC1CategoryCode(any(CategoryCodeRequest.class));
    verify(this.xProductFeign).getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequest);
    assertNotNull(response);
    List<ItemDetailWebResponse> response1 = response.getContent().get(0).getItemDetailWebResponse();
    assertEquals(response1.iterator().next().getItemName(), ITEM_NAME);
    assertEquals(response1.iterator().next().getItemSku(), ITEM_SKU);
  }

  @Test
  public void getProductListByMerchantAndCategoryCodeWithoutCategoryCodeTest() {
    activeProductWebRequest.setCategoryCode(null);
    activeProductRequest.setCategoryCodes(Collections.emptyList());
    List<ActiveProductResponse> activeProductResponses = new ArrayList<>();
    activeProductResponses.add(activeProductResponse);
    activeProductResponseGdnRestListResponse =
        new GdnRestListResponse<>(activeProductResponses, pageMetaData, REQUEST_ID);
    when(this.xProductFeign.getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequest))
        .thenReturn(activeProductResponseGdnRestListResponse);
    Page<ActiveProductWebResponse> response =
        this.productService.getProductListByMerchantAndCategoryCode(activeProductWebRequest, MERCHANT_CODE);
    verify(this.xProductFeign).getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequest);
    assertNotNull(response);
    List<ItemDetailWebResponse> response1 = response.getContent().get(0).getItemDetailWebResponse();
    assertEquals(response1.iterator().next().getItemName(), ITEM_NAME);
    assertEquals(response1.iterator().next().getItemSku(), ITEM_SKU);
  }

  @Test
  public void getProductListByMerchantAndCategoryCodeWithCategoryCodeEmptyTest() {
    activeProductWebRequest.setCategoryCode(StringUtils.EMPTY);
    activeProductRequest.setCategoryCodes(Collections.emptyList());
    List<ActiveProductResponse> activeProductResponses = new ArrayList<>();
    activeProductResponses.add(activeProductResponse);
    activeProductResponseGdnRestListResponse =
        new GdnRestListResponse<>(activeProductResponses, pageMetaData, REQUEST_ID);
    when(this.xProductFeign.getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequest))
        .thenReturn(activeProductResponseGdnRestListResponse);
    Page<ActiveProductWebResponse> response =
        this.productService.getProductListByMerchantAndCategoryCode(activeProductWebRequest, MERCHANT_CODE);
    verify(this.xProductFeign).getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequest);
    assertNotNull(response);
    List<ItemDetailWebResponse> response1 = response.getContent().get(0).getItemDetailWebResponse();
    assertEquals(response1.iterator().next().getItemName(), ITEM_NAME);
    assertEquals(response1.iterator().next().getItemSku(), ITEM_SKU);
  }

  @Test
  public void getProductListByMerchantAndCategoryCodeWithEmptyRequestAndResponseTest() {
    when(this.xProductFeign.getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequest1))
        .thenReturn(gdnRestListResponse1);
    Page<ActiveProductWebResponse> response =
        this.productService.getProductListByMerchantAndCategoryCode(activeProductWebRequest1, MERCHANT_CODE);
    verify(this.xProductFeign).getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequest1);
    assertNotNull(response);
    List<ItemDetailWebResponse> response1 = response.getContent().get(0).getItemDetailWebResponse();
    assertEquals(response1, Collections.EMPTY_LIST);
  }

  @Test
  public void getProductListByMerchantAndCategoryCodeWithEmptyCategoryCodeResponseTest() {
    categoryCodeResponseGdnRestSingleResponse.getValue().setCategoryCodes(Collections.EMPTY_LIST);
    when(this.pcbFeign.getAllChildCategoryCodesByC1CategoryCode(any(CategoryCodeRequest.class)))
        .thenReturn(categoryCodeResponseGdnRestSingleResponse);
    when(this.xProductFeign.getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequest))
        .thenReturn(activeProductResponseGdnRestListResponse);
    Page<ActiveProductWebResponse> response =
        this.productService.getProductListByMerchantAndCategoryCode(activeProductWebRequest, MERCHANT_CODE);
    verify(this.pcbFeign).getAllChildCategoryCodesByC1CategoryCode(any(CategoryCodeRequest.class));
    verify(this.xProductFeign).getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequest);
    assertNotNull(response);
    assertEquals(activeProductRequest.getCategoryCodes(), Collections.singletonList(CATEGORY_CODE));
  }

  @Test
  public void getProductListByMerchantAndCategoryCodeV2Success() throws Exception {
    ActiveProductRequest activeProductRequestForQR = new ActiveProductRequest();
    activeProductRequestForQR.setMerchantCode(MERCHANT_CODE);
    activeProductRequestForQR.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    activeProductRequestForQR.setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    activeProductRequestForQR.setDiscoverable(false);
    activeProductRequestForQR.setBuyable(false);
    activeProductRequestForQR.setSearchKey(SEARCH_KEY);
    activeProductRequestForQR.setCncActivated(true);
    ActiveProductResponse activeProductResponseForQr = new ActiveProductResponse();
    activeProductResponseForQr.setProductCode(PRODUCT_CODE);
    activeProductResponseForQr.setProductName(PRODUCT_NAME);
    activeProductResponseForQr.setProductSku(PRODUCT_SKU);
    activeProductResponseForQr.setItemCount(1);
    activeProductResponseForQr.setMasterCatalog(CATEGORY_CODE);
    GdnRestListResponse<ActiveProductResponse> activeProductResponseGdnRestListResponseForQR =
        new GdnRestListResponse<>(Collections.singletonList(activeProductResponseForQr),
            pageMetaData, REQUEST_ID);
    when(categoryService.getCategoriesByCategoryCodes(Collections.singletonList(CATEGORY_CODE)))
        .thenReturn(Collections.singletonList(categoryWebResponse));
    when(this.xProductFeign.getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequestForQR))
        .thenReturn(activeProductResponseGdnRestListResponseForQR);
    Page<ProductL3ListingWebResponse> response =
        this.productService.getProductListByMerchantAndCategoryCodeV2(productL3ListingWebRequest,
            MERCHANT_CODE, PAGE, SIZE);
    verify(categoryService).getCategoriesByCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    verify(this.xProductFeign).getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequestForQR);
  }

  @Test
  public void getProductListByMerchantAndCategoryCodeV2NonnullSuccess() throws Exception {
    ActiveProductRequest activeProductRequestForQR = new ActiveProductRequest();
    activeProductRequestForQR.setMerchantCode(MERCHANT_CODE);
    activeProductRequestForQR.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    activeProductRequestForQR.setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    activeProductRequestForQR.setDiscoverable(false);
    activeProductRequestForQR.setBuyable(false);
    activeProductRequestForQR.setSearchKey(SEARCH_KEY);
    activeProductRequestForQR.setCncActivated(true);
    ActiveProductResponse activeProductResponseForQr = new ActiveProductResponse();
    activeProductResponseForQr.setProductCode(PRODUCT_CODE);
    activeProductResponseForQr.setProductName(PRODUCT_NAME);
    activeProductResponseForQr.setProductSku(PRODUCT_SKU);
    activeProductResponseForQr.setItemCount(1);
    activeProductResponseForQr.setMasterCatalog(CATEGORY_CODE);
    activeProductResponseForQr.setItemAndPickupPointBasicDetailResponse(new ItemAndPickupPointBasicDetailResponse());
    GdnRestListResponse<ActiveProductResponse> activeProductResponseGdnRestListResponseForQR =
        new GdnRestListResponse<>(Collections.singletonList(activeProductResponseForQr),
            pageMetaData, REQUEST_ID);
    when(categoryService.getCategoriesByCategoryCodes(Collections.singletonList(CATEGORY_CODE)))
        .thenReturn(Collections.singletonList(categoryWebResponse));
    when(this.xProductFeign.getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequestForQR))
        .thenReturn(activeProductResponseGdnRestListResponseForQR);
    Page<ProductL3ListingWebResponse> response =
        this.productService.getProductListByMerchantAndCategoryCodeV2(productL3ListingWebRequest,
            MERCHANT_CODE, PAGE, SIZE);
    verify(categoryService).getCategoriesByCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    verify(this.xProductFeign).getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE, activeProductRequestForQR);
  }

  @Test
  public void getSuspendedItemListByMerchantAndCategoryCodeExceptionTest() {
    when(this.pbpFeign.getSuspendedItem(PAGE, SIZE, summaryFilterRequest)).thenThrow(ClientException.class);
    Page<SuspensionWebResponse> response = null;
    try {
      response = this.productService
          .getSuspendedItemListByMerchantAndCategoryCode(new SuspensionWebRequest(), PageRequest.of(PAGE, SIZE));
    } catch (ClientException e) {
    } finally {
      verify(this.pbpFeign).getSuspendedItem(PAGE, SIZE, summaryFilterRequest);
      assertNull(response);
    }
  }

  @Test
  public void getSuspendedItemListByMerchantAndCategoryCodeTest() {
    when(this.pbpFeign.getSuspendedItem(PAGE, SIZE, summaryFilterRequest))
        .thenReturn(suspensionItemResponseGdnRestListResponse);
    Page<SuspensionWebResponse> response = this.productService
        .getSuspendedItemListByMerchantAndCategoryCode(new SuspensionWebRequest(), PageRequest.of(PAGE, SIZE));
    verify(this.pbpFeign).getSuspendedItem(PAGE, SIZE, summaryFilterRequest);
    assertNotNull(response);
  }

  @Test
  public void getProductCounts_forActiveProductTest() {
    when(this.pbpFeign.getActiveProductStockCount(MERCHANT_CODE))
        .thenReturn(productLevel3SummaryCountResponseGdnRestSingleResponse);
    ProductCountWebResponse response = this.productService.getProductCounts(ACTIVE, MERCHANT_CODE);
    verify(this.pbpFeign).getActiveProductStockCount(MERCHANT_CODE);
    assertNotNull(response);
    assertEquals(10L, response.getAvailable().longValue());
    assertEquals(20L, response.getMinimumStock().longValue());
    assertEquals(30L, response.getOutOfStock().longValue());
    assertEquals(40L, response.getTotalCounts().longValue());
  }

  @Test
  public void getCogsValueTest() {
    ProfileResponse profileResponse;
    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");
    profileResponse.setMultiDefaultAddressFlag(true);
    PickupPointDTO pickupPointDto = new PickupPointDTO();
    pickupPointDto.setCode(BUSINESSPARTNER_CODE);
    pickupPointDto.setGeolocation(new GeolocationDTO());
    PickupPointDTO pickupPointDto1 = new PickupPointDTO();
    pickupPointDto1.setCode(PICKUP_POINT_CODE);
    pickupPointDto1.setGeolocation(null);
    List<PickupPointDTO> listPickupPointDto = new ArrayList<PickupPointDTO>();
    listPickupPointDto.add(pickupPointDto);
    listPickupPointDto.add(pickupPointDto1);
    company = new CompanyDTO();
    company.setBusinessPartnerName(BUSINESSPARTNER_CODE);
    company.setMerchantFlag(true);
    company.setInternationalFlag(false);
    company.setMerchantType("Test");
    company.setOfflineToOnlineFlag(true);
    company.setInventoryFulfillment("BL");
    company.setOfflineToOnlineFlag(true);
    company.setEmail(EMAIL);
    company.setPurchaseTerm(PURCHASE_TERM);
    profileResponse.setCompany(company);
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setAllCategory(true);

    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    GdnRestSimpleResponse<CogsValueResponse> response = new GdnRestSimpleResponse<>();
    CogsValueResponse cogsValueResponse = new CogsValueResponse();
    cogsValueResponse.setMovingPrice(TEST_PRICE);
    response.setValue(cogsValueResponse);
    response.setSuccess(Boolean.TRUE);
    when(this.pbpFeign.getCogsValue(MATERIAL_CODE)).thenReturn(response);
    double cogsValue = this.productService.getCogsValue(MATERIAL_CODE, BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(any());
    verify(this.pbpFeign).getCogsValue(any());
    Assertions.assertEquals(53.53, cogsValue, 0.0);
  }

  @Test
  public void getCogsValueTest_MerchantTypeCM() {
    ProfileResponse profileResponse;
    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");
    PickupPointDTO pickupPointDto = new PickupPointDTO();
    pickupPointDto.setCode(BUSINESSPARTNER_CODE);
    pickupPointDto.setGeolocation(new GeolocationDTO());
    PickupPointDTO pickupPointDto1 = new PickupPointDTO();
    pickupPointDto1.setCode(PICKUP_POINT_CODE);
    pickupPointDto1.setGeolocation(null);
    List<PickupPointDTO> listPickupPointDto = new ArrayList<PickupPointDTO>();
    listPickupPointDto.add(pickupPointDto);
    listPickupPointDto.add(pickupPointDto1);
    company = new CompanyDTO();
    company.setBusinessPartnerName(BUSINESSPARTNER_CODE);
    company.setMerchantFlag(true);
    company.setInternationalFlag(false);
    company.setMerchantType("CM");
    company.setOfflineToOnlineFlag(true);
    company.setInventoryFulfillment("BL");
    company.setOfflineToOnlineFlag(true);
    company.setEmail(EMAIL);
    company.setPurchaseTerm(PURCHASE_TERM);
    profileResponse.setCompany(company);
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setAllCategory(true);

    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    GdnRestSimpleResponse<CogsValueResponse> response = new GdnRestSimpleResponse<>();
    CogsValueResponse cogsValueResponse = new CogsValueResponse();
    cogsValueResponse.setMovingPrice(TEST_PRICE);
    response.setValue(cogsValueResponse);
    response.setSuccess(Boolean.TRUE);
    when(this.pbpFeign.getCogsValue(MATERIAL_CODE)).thenReturn(response);
    Double cogsValue = this.productService.getCogsValue(MATERIAL_CODE, BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(any());
    Assertions.assertEquals(null, cogsValue);
  }

  @Test
  public void getCogsValueTest_CompanyNull() {
    ProfileResponse profileResponse;
    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");
    PickupPointDTO pickupPointDto = new PickupPointDTO();
    pickupPointDto.setCode(BUSINESSPARTNER_CODE);
    pickupPointDto.setGeolocation(new GeolocationDTO());
    PickupPointDTO pickupPointDto1 = new PickupPointDTO();
    pickupPointDto1.setCode(PICKUP_POINT_CODE);
    pickupPointDto1.setGeolocation(null);
    List<PickupPointDTO> listPickupPointDto = new ArrayList<PickupPointDTO>();
    listPickupPointDto.add(pickupPointDto);
    listPickupPointDto.add(pickupPointDto1);
    profileResponse.setCompany(null);
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setAllCategory(true);

    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    GdnRestSimpleResponse<CogsValueResponse> response = new GdnRestSimpleResponse<>();
    CogsValueResponse cogsValueResponse = new CogsValueResponse();
    cogsValueResponse.setMovingPrice(TEST_PRICE);
    response.setValue(cogsValueResponse);
    response.setSuccess(Boolean.TRUE);
    when(this.pbpFeign.getCogsValue(MATERIAL_CODE)).thenReturn(response);
    Double cogsValue = this.productService.getCogsValue(MATERIAL_CODE, BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(any());
    Assertions.assertEquals(null, cogsValue);
  }

  @Test
  public void getCogsValueTest_BusinessPartnerNull() {
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(null);
    GdnRestSimpleResponse<CogsValueResponse> response = new GdnRestSimpleResponse<>();
    CogsValueResponse cogsValueResponse = new CogsValueResponse();
    cogsValueResponse.setMovingPrice(TEST_PRICE);
    response.setValue(cogsValueResponse);
    response.setSuccess(Boolean.TRUE);
    when(this.pbpFeign.getCogsValue(MATERIAL_CODE)).thenReturn(response);
    Double cogsValue = this.productService.getCogsValue(MATERIAL_CODE, BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(any());
    Assertions.assertEquals(null, cogsValue);
  }

  @Test
  public void getProductCounts_forInProcessProductTest() {
    when(this.pbpFeign.getInProgressProductCount(MERCHANT_CODE))
        .thenReturn(countProductLevel3WipResponseGdnRestSingleResponse);
    ProductCountWebResponse response = this.productService.getProductCounts(INPROCESS, MERCHANT_CODE);
    verify(this.pbpFeign).getInProgressProductCount(MERCHANT_CODE);
    assertNotNull(response);
    assertEquals(10L, response.getNeedAction().longValue());
    assertEquals(20L, response.getWaitingForApproval().longValue());
    assertEquals(30L, response.getNeedCorrection().longValue());
    assertEquals(60L, response.getTotalCounts().longValue());
  }

  @Test
  public void getProductCounts_forInActiveProductTest() {
    when(this.pbpFeign.getInActiveProductCount(MERCHANT_CODE))
        .thenReturn(countProductLevel3InactiveResponseGdnRestSingleResponse);
    ProductCountWebResponse response = this.productService.getProductCounts(INACTIVE, MERCHANT_CODE);
    verify(this.pbpFeign).getInActiveProductCount(MERCHANT_CODE);
    assertNotNull(response);
    assertEquals(10L, response.getArchived().longValue());
    assertEquals(20L, response.getRejected().longValue());
    assertEquals(30L, response.getSuspended().longValue());
    assertEquals(60L, response.getTotalCounts().longValue());
  }

  @Test
  public void getProductCountsEmptyTypeExceptionTest() {
    try {
      this.productService.getProductCounts(StringUtils.EMPTY, MERCHANT_CODE);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
    }
  }

  @Test
  public void getProductCountsInvalidTypeExceptionTest() {
    try {
      this.productService.getProductCounts(KEYWORD, MERCHANT_CODE);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
    }
  }

  @Test
  public void getProductCountsEmptyMerchantCodeExceptionTest() {
    try {
      this.productService.getProductCounts(ACTIVE, StringUtils.EMPTY);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
    }
  }

  @Test
  public void findSummaryByFilterArchivedTest() {
    when(this.pbpFeign
        .filterSummary(eq(inActiveProductWebRequest.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(sort.getSortType()), eq(sort.getSortBy()),
            any(ProductLevel3SummaryRequest.class))).thenReturn(productLevel3SummaryResponseGdnRestListResponse);
    Page<ProductLevel3SummaryResponse> result = this.productService
        .findSummaryByFilter(inActiveProductWebRequest, pageRequest, sort, InActiveProductStatus.ARCHIVED.toString());
    verify(this.pbpFeign)
        .filterSummary(eq(inActiveProductWebRequest.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(sort.getSortType()), eq(sort.getSortBy()),
            productLevel3SummaryRequestArgumentCaptor.capture());
    assertTrue(productLevel3SummaryRequestArgumentCaptor.getValue().getArchived());
    assertEquals(DATA_LINK, result.getContent().get(0).getProductDetailPageLink());
    assertEquals(ITEM_NAME, result.getContent().get(0).getItemName());
    assertEquals(PRODUCT_CODE, result.getContent().get(0).getProductCode());
  }

  @Test
  public void findSummaryByFilterSuspendedTest() {
    when(this.pbpFeign.getSuspendedItem(PAGE, SIZE, summaryFilterRequest))
        .thenReturn(suspensionItemResponseGdnRestListResponse);
    Page<ProductLevel3SummaryResponse> result = this.productService
        .findSummaryByFilter(new InActiveProductWebRequest(), pageRequest, sort,
            InActiveProductStatus.SUSPENDED.toString());
    verify(this.pbpFeign).getSuspendedItem(PAGE, SIZE, summaryFilterRequest);
    assertNotNull(result);
  }

  @Test
  public void findSummaryByFilterRejectedTest() {
    when(this.pbpFeign
        .filterProductBusinessPartnerSummaryByBusinessPartnerId(eq(PAGE), eq(SIZE), eq(BUSINESSPARTNER_CODE),
            eq(StringUtils.EMPTY), eq(ORDER_BY), eq(SORT_BY)))
        .thenReturn(rejectedSkuProductResponseGdnRestListResponse);
    Page<ProductLevel3SummaryResponse> result = this.productService
        .findSummaryByFilter(inActiveProductWebRequest, pageRequest, sort, InActiveProductStatus.REJECTED.toString());
    verify(this.pbpFeign)
        .filterProductBusinessPartnerSummaryByBusinessPartnerId(PAGE, SIZE, BUSINESSPARTNER_CODE, StringUtils.EMPTY,
            ORDER_BY, SORT_BY);
    assertNotNull(result);
  }

  @Test
  void findSummaryByFilterExceptionTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.productService.findSummaryByFilter(inActiveProductWebRequest, pageRequest, sort,
            TYPE));
  }

  @Test
  public void getActiveProductListTest() {
    when(this.xCampaignFeign.getCampaignAvailability(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.pbpFeign
        .filterSummary(eq(activeProductWebRequest2.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(ORDERBY), eq(SORTBY), any(ProductLevel3SummaryRequest.class)))
        .thenReturn(productLevel3SummaryResponseGdnRestListResponse);
    Page<ProductLevel3SummaryWebResponse> result =
        this.productService.getActiveProductList(activeProductWebRequest2, pageRequest);
    verify(this.pbpFeign)
        .filterSummary(eq(activeProductWebRequest2.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(ORDERBY), eq(SORTBY),
            productLevel3SummaryRequestArgumentCaptor.capture());
    verify(this.xCampaignFeign).getCampaignAvailability(productCampaignAvailabilityRequest);
    assertTrue(productLevel3SummaryRequestArgumentCaptor.getValue().getBuyable());
    assertTrue(productLevel3SummaryRequestArgumentCaptor.getValue().getDisplayable());
    assertEquals(ProductLevel3InventoryCriteria.AVAILABLE,
        productLevel3SummaryRequestArgumentCaptor.getValue().getInventoryFilter());
    assertEquals(CATEGORY_CODE, productLevel3SummaryRequestArgumentCaptor.getValue().getCategoryCodes().get(0));
    assertEquals(PICKUP_POINT_CODE, productLevel3SummaryRequestArgumentCaptor.getValue().getPickupPointCodes().get(0));
    assertNotNull(result);
    assertTrue(result.getContent().get(0).isItemCampaignMapped());
    assertEquals(90.0, result.getContent().get(0).getProductScore(), 0);
  }

  @Test
  public void getActiveProductListEmptyTest() {
    when(this.pbpFeign
        .filterSummary(eq(activeProductWebRequest2.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(ORDERBY), eq(SORTBY), any(ProductLevel3SummaryRequest.class)))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID));
    Page<ProductLevel3SummaryWebResponse> result =
        this.productService.getActiveProductList(activeProductWebRequest2, pageRequest);
    verify(this.pbpFeign)
        .filterSummary(eq(activeProductWebRequest2.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(ORDERBY), eq(SORTBY),
            productLevel3SummaryRequestArgumentCaptor.capture());
    assertEquals(0, result.getContent().size());
    assertNotNull(result);
  }

  @Test
  public void filterSummaryWithStateTest() {
    when(this.pbpFeign.filterSummaryWithState(eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
        any(ProductLevel3WipSummaryRequest.class))).thenReturn(responseGdnRestListResponse);
    Page<InProcessWebResponse> result =
        this.productService.getInprocessProductList(inProcessProductWebRequest, pageRequest);
    verify(this.pbpFeign).filterSummaryWithState(eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
        productLevel3WipSummaryRequestArgumentCaptor.capture());
    Mockito.verify(mandatoryParameterHelper).isExternal();
    assertEquals(CRITERIA, productLevel3WipSummaryRequestArgumentCaptor.getValue().getCriteria().toString());
    assertEquals(PRODUCT_NAME, result.getContent().get(0).getItemName());
    assertEquals(BRAND, result.getContent().get(0).getBrand());
  }

  @Test
  public void getActiveProductNameListTest() {
    when(this.xProductFeign.getActiveProductNamesByMerchantCode(PAGE, SIZE, itemSummaryRequest))
        .thenReturn(itemSummaryResponseGdnRestListResponse);
    Page<ItemDetailWebResponse> result =
        this.productService.getActiveProductNameList(SEARCH_KEY, MERCHANT_CODE, false, PageRequest.of(PAGE, SIZE), false);
    verify(this.xProductFeign).getActiveProductNamesByMerchantCode(PAGE, SIZE, itemSummaryRequest);
    assertNotNull(result);
    assertEquals(1, result.getContent().size());
    assertEquals(ITEM_NAME, result.getContent().get(0).getItemName());
    assertEquals(ITEM_SKU, result.getContent().get(0).getItemSku());
  }

  @Test
  public void getActiveProductNameListTest_isProductName() {
    productSummaryRequest =
        ProductSummaryRequest.builder().archived(false).keyword(SEARCH_KEY).merchantCode(MERCHANT_CODE).inStock(true).build();
    when(this.xProductFeign.getProductNamesByFilter(PAGE, SIZE, productSummaryRequest))
        .thenReturn(productNameSuggestionResponseGdnRestListResponse);
    Page<ItemDetailWebResponse> result =
        this.productService.getActiveProductNameList(SEARCH_KEY, MERCHANT_CODE, true, PageRequest.of(PAGE, SIZE), true);
    verify(this.userPicService).fetchAccessiblePickupPointCodes(null);
    verify(this.xProductFeign).getProductNamesByFilter(PAGE, SIZE, productSummaryRequest);
    assertNotNull(result);
    assertEquals(1, result.getContent().size());
    assertEquals(PRODUCT_NAME, result.getContent().get(0).getProductName());
    assertNull(result.getContent().get(0).getItemSku());
  }

  @Test
  public void getActiveProductNameListTest_isProductName_withPickupPoints() {
    productSummaryRequest = ProductSummaryRequest.builder().archived(false).keyword(SEARCH_KEY)
      .merchantCode(MERCHANT_CODE).inStock(true)
      .pickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)).build();
    when(this.xProductFeign.getProductNamesByFilter(eq(PAGE), eq(SIZE), eq(productSummaryRequest)))
      .thenReturn(productNameSuggestionResponseGdnRestListResponse);
    when(userPicService.fetchAccessiblePickupPointCodes(null)).thenReturn(
      Collections.singleton(PICKUP_POINT_CODE));
    Page<ItemDetailWebResponse> result =
      this.productService.getActiveProductNameList(SEARCH_KEY, MERCHANT_CODE, true, PageRequest.of(PAGE, SIZE), true);
    verify(this.userPicService).fetchAccessiblePickupPointCodes(null);
    verify(this.xProductFeign).getProductNamesByFilter(eq(PAGE), eq(SIZE),
      eq(productSummaryRequest));
    assertNotNull(result);
    assertEquals(1, result.getContent().size());
    assertEquals(PRODUCT_NAME, result.getContent().get(0).getProductName());
    assertNull(result.getContent().get(0).getItemSku());
  }

  @Test
  void getActiveProductNameListWithNullSearchKeyExceptionTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.productService.getActiveProductNameList(null, MERCHANT_CODE, true,
            PageRequest.of(PAGE, SIZE), false));
  }

  @Test
  void getActiveProductNameListWithNullMerchantCodeExceptionTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.productService.getActiveProductNameList(SEARCH_KEY, null, true,
            PageRequest.of(PAGE, SIZE), false));
  }

  @Test
  public void toggleArchiveItemsTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_ARCHIVE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      when(this.pbpFeign.toggleArchiveItem(DEFAULT_CLIENT_HOST, ITEM_SKU_1, DO_ARCHIVE)).thenReturn(
          new GdnBaseRestResponse(true));
      this.productService.toggleArchiveItems(itemSkus, DO_ARCHIVE, BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).toggleArchiveItem(DEFAULT_CLIENT_HOST, ITEM_SKU_1, DO_ARCHIVE);
    }
  }

  @Test
  public void toggleArchiveItemsForEmptyItemSkuTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_ARCHIVE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      itemSkus.clear();
      itemSkus.add(StringUtils.EMPTY);
      this.productService.toggleArchiveItems(itemSkus, DO_ARCHIVE, BUSINESSPARTNER_CODE);
      verify(this.pbpFeign, times(0)).toggleArchiveItem(Mockito.eq(DEFAULT_CLIENT_HOST), any(),
          Mockito.eq(DO_ARCHIVE));
    }
  }

  @Test
  void toggleArchiveItemsExceptionTest() {
      itemSkus.clear();
      itemSkus.add(ITEM_SKU);
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.toggleArchiveItems(itemSkus, DO_ARCHIVE, BUSINESSPARTNER_CODE));
    }


  @Test
  public void toggleArchiveItemsExceptionUnauthorizeErrorTest() {
    String errorMessage = StringUtils.EMPTY;
    String[] accessibility = {};
    itemSkus.clear();
    itemSkus.add(ITEM_SKU);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
        Assertions.assertThrows(ApplicationRuntimeException.class,
            () -> this.productService.toggleArchiveItems(itemSkus, DO_ARCHIVE,
                BUSINESSPARTNER_CODE));
    }
  }

  @Test
  public void getProductUpdateLogsTest() {
    when(this.pbpFeign.getProductUpdateLogs(PAGE, SIZE, ITEM_SKU_1))
        .thenReturn(logAuditTrailUpdatedProductResponseGdnRestListResponse);
    Page<LogAuditTrailUpdatedProductResponse> result =
        this.productService.getProductUpdateLogs(ITEM_SKU_1, PageRequest.of(PAGE, SIZE));
    verify(this.pbpFeign).getProductUpdateLogs(PAGE, SIZE, ITEM_SKU_1);
    assertNotNull(result);
    assertEquals(1, result.getContent().size());
  }

  @Test
  public void updateSummaryTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_LISTING_ALLOW_UPDATE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      when(this.pbpFeign.filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU)).thenReturn(
          level3OrderResponseGdnRestSingleResponse);
      when(this.pbpFeign.updateSummary(BUSINESSPARTNER_CODE, ITEM_SKU, DEFAULT_CLIENT_HOST,
          productLevel3UpdateSummaryRequest)).thenReturn(
          productLevel3SummaryResponseGdnRestSingleResponse);
      when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      ProductLevel3SummaryResponse result =
          this.productService.updateSummary(ITEM_SKU, BUSINESSPARTNER_CODE,
              productLevel3UpdateSummaryRequest);
      verify(this.pbpFeign).filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU);
      verify(this.pbpFeign).updateSummary(BUSINESSPARTNER_CODE, ITEM_SKU, DEFAULT_CLIENT_HOST,
          productLevel3UpdateSummaryRequest);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      assertNotNull(result);
    }
  }

  @Test
  public void updateSummaryMerchantSKUChangeExceptionTest() {
    String errorMessage = StringUtils.EMPTY;
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_LISTING_ALLOW_UPDATE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      when(this.pbpFeign.filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU)).thenReturn(
          level3OrderResponseGdnRestSingleResponse);
      when(this.pbpFeign.updateSummary(BUSINESSPARTNER_CODE, ITEM_SKU, DEFAULT_CLIENT_HOST,
          productLevel3UpdateSummaryRequest)).thenReturn(
          productLevel3SummaryResponseGdnRestSingleResponse);
      when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      productLevel3UpdateSummaryRequest.setMerchantSku("MERCHANTSKUCHANGE");
      try {
        ProductLevel3SummaryResponse result =
            this.productService.updateSummary(ITEM_SKU, BUSINESSPARTNER_CODE,
                productLevel3UpdateSummaryRequest);
      } catch (ApplicationRuntimeException e) {
        errorMessage = e.getErrorMessage();
      } finally {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
        Mockito.verify(pbpFeign).filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU);
        Assertions.assertEquals("Can not save, update or delete:You are not authorized",
            errorMessage);
      }
    }
  }

  @Test
  public void updateSummary_ExceptionUnauthorizeErrorTest() {
    String[] accessibility = {};
    String errorMessage = StringUtils.EMPTY;
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      try {
        ProductLevel3SummaryResponse result =
            this.productService.updateSummary(ITEM_SKU, BUSINESSPARTNER_CODE,
                productLevel3UpdateSummaryRequest);
      } catch (ApplicationRuntimeException e) {
        errorMessage = e.getErrorMessage();
      } finally {
        assertEquals(UNAUTHORIZE_ERROR, errorMessage);
      }
    }
  }

  @Test
  public void updateProductDetailsTest() throws Exception {
    AgpSimpleQueryResponse response1 =
        AgpSimpleQueryResponse.builder().hits(HitsResponse.builder().total(4).build()).build();
    Mockito.when(pbpFeign.getProductSkusByProductCode(any()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, new ProductSkuResponseList(itemSkus)));
    when(agpQueryFeign
        .findNumberOfOrder(any(), any(), any(), any()))
        .thenReturn(response1);
    when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.API_CLIENT_ID);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
        .thenReturn(pickupPointResponseList);
    when(this.pbpFeign.filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU))
        .thenReturn(level3OrderResponseGdnRestSingleResponse);
    when(this.pbpFeign
        .updateAndReturn(DEFAULT_CLIENT_HOST, Boolean.valueOf(Constants.IS_EXTERNAL_ONLY), true, false, productLevel3Request))
        .thenReturn(productLevel3ResponseGdnRestSingleResponse);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class);
        MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(
          () -> RequestHelper.toProductLevel3RequestFromProductLevel3WebRequest(
              productLevel3WebRequest)).thenReturn(productLevel3Request);
      this.productService.updateProductDetails(productLevel3WebRequest, BUSINESSPARTNER_CODE,
          Constants.IS_EXTERNAL_ONLY);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(mandatoryParameterHelper).getClientId();
      Mockito.verify(pbpFeign).filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU);
      Mockito.verify(pbpFeign)
          .updateAndReturn(DEFAULT_CLIENT_HOST, Boolean.valueOf(Constants.IS_EXTERNAL_ONLY), true,
              false, productLevel3Request);
      Mockito.verify(agpQueryFeign).findNumberOfOrder(any(), any(), any(), any());
      Assertions.assertEquals(true, gdnBaseRestResponse.isSuccess());
    }
  }

  @Test
  public void updateProductDetailsBussinessPartnerNotActiveExceptionTest() {
    String errorMessage = StringUtils.EMPTY;
    ProfileResponse response = new ProfileResponse();
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        response);
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedRequestHelper.when(
          () -> RequestHelper.toProductLevel3RequestFromProductLevel3WebRequest(
              productLevel3WebRequest)).thenReturn(productLevel3Request);
      try {
        this.productService.updateProductDetails(productLevel3WebRequest, BUSINESSPARTNER_CODE,
            Constants.IS_EXTERNAL_ONLY);
      } catch (Exception e) {
        errorMessage = e.getMessage();
      } finally {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
        Assertions.assertEquals("System/data are in invalid state :business partner not active",
            errorMessage);
      }
    }
  }

  @Test
  public void updateProductDetailsEditStockAccessibilityExceptionTest() {
    String errorMessage = StringUtils.EMPTY;
    String[] accessibility = new String[1];
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      try {
        Method method = productService.getClass()
            .getDeclaredMethod("editStockAccessibilityCheck", ProductLevel3Request.class);
        method.setAccessible(true);
        method.invoke(productService, productLevel3Request);
      } catch (Exception e) {
        errorMessage = e.getCause().getMessage();
      } finally {
        Assertions.assertEquals("Can not save, update or delete:No access to edit stock",
            errorMessage);
      }
    }
  }

  @Test
  public void updateProductDetailsBrandChangeExceptionTest() {
    String[] accessibility = {Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA};
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
        BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
    when(this.pbpFeign.filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU)).thenReturn(
        level3OrderResponseGdnRestSingleResponse);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class);
        MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      mockedRequestHelper.when(
          () -> RequestHelper.toProductLevel3RequestFromProductLevel3WebRequest(
              productLevel3WebRequest)).thenReturn(productLevel3Request);
      productLevel3Request.setBrand(BRAND1);
      when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.APP_CLIENT_ID);
      try {
        Assertions.assertThrows(ApplicationRuntimeException.class,
            () -> this.productService.updateProductDetails(productLevel3WebRequest,
                BUSINESSPARTNER_CODE, Constants.IS_EXTERNAL_ONLY));
      }
      finally {
        verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
        verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
            BUSINESSPARTNER_CODE);
        Mockito.verify(pbpFeign).filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU);
        verify(mandatoryParameterHelper).getClientId();
      }
    }
  }

  @Test
  public void updateProductDetailsMerchantSkuChangeExceptionTest() {
    String errorMessage = StringUtils.EMPTY;
    String[] accessibility = {Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA};
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
        BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
    when(this.pbpFeign.filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU)).thenReturn(
        level3OrderResponseGdnRestSingleResponse);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class);
        MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      mockedRequestHelper.when(
          () -> RequestHelper.toProductLevel3RequestFromProductLevel3WebRequest(
              productLevel3WebRequest)).thenReturn(productLevel3Request);
      productLevel3Request.getItems().get(0).setMerchantSku("SKUCHANGED");
      when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.APP_CLIENT_ID);
      try {
        this.productService.updateProductDetails(productLevel3WebRequest, BUSINESSPARTNER_CODE,
            Constants.IS_EXTERNAL_ONLY);
      } catch (Exception e) {
        errorMessage = e.getMessage();
      } finally {
        verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
        verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
            BUSINESSPARTNER_CODE);
        Mockito.verify(pbpFeign).filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU);
        verify(mandatoryParameterHelper).getClientId();
        Assertions.assertEquals("Can not save, update or delete:You are not authorized",
            errorMessage);
      }
    }
  }

  @Test
  public void updateProductDetailsMerchantSkuChange1ExceptionTest() {
    String errorMessage = StringUtils.EMPTY;
    String[] accessibility = {Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA, Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA};
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
        .thenReturn(pickupPointResponseList);
    when(this.pbpFeign.filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU))
        .thenReturn(level3OrderResponseGdnRestSingleResponse);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class);
        MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      mockedRequestHelper.when(
          () -> RequestHelper.toProductLevel3RequestFromProductLevel3WebRequest(
              productLevel3WebRequest)).thenReturn(productLevel3Request);
      productLevel3Request.getItems().get(0).setMerchantSku("SKUCHANGED");
      when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.APP_CLIENT_ID);
      try {
        this.productService.updateProductDetails(productLevel3WebRequest, BUSINESSPARTNER_CODE,
            Constants.IS_EXTERNAL_ONLY);
      } catch (Exception e) {
        errorMessage = e.getMessage();
      } finally {
        verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
        verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
            BUSINESSPARTNER_CODE);
        Mockito.verify(pbpFeign).filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU);
        verify(mandatoryParameterHelper).getClientId();
        Assertions.assertEquals("Can not save, update or delete:You are not authorized",
            errorMessage);
      }
    }
  }

  @Test
  public void setProductOff2OnFlagNullAndRoundOffSalePriceTest() throws Exception {
    Method method = ProductServiceImpl.class
        .getDeclaredMethod("setProductOff2OnFlagNullAndRoundOffSalePrice", ProductLevel3Request.class,
            ProductLevel3OrderResponse.class);
    method.setAccessible(true);
    method.invoke(productService, productLevel3Request, level3OrderResponse);
    assertNull(productLevel3Request.getItems().get(0).getOff2OnActiveFlag());
    Assertions.assertEquals(new Double(Math.ceil(productLevel3Request.getItems().get(0).getPrices().get(0).getSalePrice())),
        new Double(productLevel3Request.getItems().get(0).getPrices().get(0).getSalePrice()));
  }

  @Test
  public void bulkDeleteProductWipTest() {
    when(this.pbpFeign.bulkDeleteProductWip(BUSINESSPARTNER_CODE, bulkDeleteProductWipRequest)).thenReturn(
        new GdnRestSingleResponse<PostLiveProductCountResponse>(null, null, true,
            new PostLiveProductCountResponse(5, new HashMap<>()), REQUEST_ID));
    PostLiveProductCountResponse postLiveProductCountResponse = this.productService.bulkDeleteProductWip(BUSINESSPARTNER_CODE, bulkDeleteProductWipRequest);
    verify(this.pbpFeign).bulkDeleteProductWip(BUSINESSPARTNER_CODE, bulkDeleteProductWipRequest);
    assertEquals(new Integer(5), postLiveProductCountResponse.getPostLiveProductCount());
  }

  @Test
  public void updateBulkSummaryTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE, Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_LISTING_ALLOW_UPDATE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      when(this.pbpFeign.filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU)).thenReturn(
          level3OrderResponseGdnRestSingleResponse);
      when(this.pbpFeign.updateSummary(BUSINESSPARTNER_CODE, ITEM_SKU, DEFAULT_CLIENT_HOST,
          productLevel3UpdateSummaryRequest)).thenReturn(
          productLevel3SummaryResponseGdnRestSingleResponse);
      when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      Map<String, ProductLevel3SummaryResponse> result =
          this.productService.updateBulkSummary(BUSINESSPARTNER_CODE,
              productLevel3UpdateSummaryRequestMap);
      verify(this.pbpFeign).filterDetailOrderByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU);
      verify(this.pbpFeign).updateSummary(BUSINESSPARTNER_CODE, ITEM_SKU, DEFAULT_CLIENT_HOST,
          productLevel3UpdateSummaryRequest);
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      assertNotNull(result);
      assertNotNull(result.get(ITEM_SKU));
    }
  }

  @Test
  public void updateBulkSummaryExceptionUnauthorizeErrorTest() {
    String errorMessage = StringUtils.EMPTY;
    String[] accessibility = {};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      try {
        Map<String, ProductLevel3SummaryResponse> result =
            this.productService.updateBulkSummary(BUSINESSPARTNER_CODE,
                productLevel3UpdateSummaryRequestMap);
      } catch (ApplicationRuntimeException e) {
        errorMessage = e.getErrorMessage();
      } finally {
        assertEquals(UNAUTHORIZE_ERROR, errorMessage);
      }
    }
  }

  @Test
  void updateBulkSummaryExceptionTest() {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.updateBulkSummary(BUSINESSPARTNER_CODE,
              productLevel3UpdateSummaryRequestMap));
  }

  @Test
  public void retryCreateTest() {
    when(this.pbpFeign.retryCreate(PRODUCT_BUSINESS_PARTNER_ID)).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    this.productService.retryCreate(Arrays.asList(PRODUCT_BUSINESS_PARTNER_ID));
    verify(this.pbpFeign).retryCreate(PRODUCT_BUSINESS_PARTNER_ID);
  }

  @Test
  void retryCreateTestNullResponse() {
    when(this.pbpFeign.retryCreate(PRODUCT_BUSINESS_PARTNER_ID)).thenReturn(null);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> this.productService.retryCreate(Arrays.asList(PRODUCT_BUSINESS_PARTNER_ID)));
    }finally {
      verify(this.pbpFeign).retryCreate(PRODUCT_BUSINESS_PARTNER_ID);
    }
  }

  @Test
  void retryCreateTestFalseResponse() {
    when(this.pbpFeign.retryCreate(PRODUCT_BUSINESS_PARTNER_ID)).thenReturn(
        new GdnBaseRestResponse(Boolean.FALSE));
    try {
      Assertions.assertThrows(ClientException.class,
          () -> this.productService.retryCreate(Arrays.asList(PRODUCT_BUSINESS_PARTNER_ID)));
    } finally {
      verify(this.pbpFeign).retryCreate(PRODUCT_BUSINESS_PARTNER_ID);
    }
  }

  @Test
  public void downloadAllProductTest() throws Exception {
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(kafkaTopicProperties.getBulkDownloadAllEvent()).thenReturn(BULK_DOWNLOAD_ALL_EVENT);
      when(this.userPicService.filterInaccessiblePickupPoints(profileResponse,
          new HashSet<>())).thenReturn(new HashSet<String>());
      this.productService.downloadAllProduct(USERNAME, true, BUSINESSPARTNER_CODE,
          productSummaryWebRequest);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(userPicService).filterInaccessiblePickupPoints(profileResponse, new HashSet<>());
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
      verify(kafkaProducer).send(Mockito.eq(kafkaTopicProperties.getBulkDownloadAllEvent()),
          productDownloadRequestArgumentCaptor.capture());
      verify(kafkaTopicProperties, times(2)).getBulkDownloadAllEvent();
      assertEquals(EMAIL, productDownloadRequestArgumentCaptor.getValue().getEmailTo());
      assertEquals(USERNAME, productDownloadRequestArgumentCaptor.getValue().getEmailCc());
      assertEquals(BulkProcessEntity.PRODUCT,
          productDownloadRequestArgumentCaptor.getValue().getBulkProcessEntity());
    }
  }

  @Test
  public void downloadAllProductWithFiltersTest() throws Exception {
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productSummaryWebRequest.setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.userPicService.filterInaccessiblePickupPoints(profileResponse,
          new HashSet<>(Collections.singletonList(PICKUP_POINT_CODE)))).thenReturn(
          new HashSet<>(Collections.singletonList(PICKUP_POINT_CODE)));
      this.productService.downloadAllProduct(USERNAME, true, BUSINESSPARTNER_CODE,
          productSummaryWebRequest);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(kafkaProducer).send(Mockito.eq(kafkaTopicProperties.getBulkDownloadAllEvent()),
          productDownloadRequestArgumentCaptor.capture());
      verify(userPicService).filterInaccessiblePickupPoints(profileResponse,
          new HashSet<>(Collections.singletonList(PICKUP_POINT_CODE)));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
      verify(kafkaTopicProperties, times(2)).getBulkDownloadAllEvent();
      assertEquals(EMAIL, productDownloadRequestArgumentCaptor.getValue().getEmailTo());
      assertEquals(USERNAME, productDownloadRequestArgumentCaptor.getValue().getEmailCc());
      assertEquals(BulkProcessEntity.PRODUCT,
          productDownloadRequestArgumentCaptor.getValue().getBulkProcessEntity());
      assertEquals(Collections.singletonList(PICKUP_POINT_CODE),
          productDownloadRequestArgumentCaptor.getValue().getProductSummaryRequest()
              .getPickupPointCodes());
    }
  }

  @Test
  public void downloadAllProductExceptionTest() throws Exception {
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(null);
    try {
      this.productService.downloadAllProduct(USERNAME, true, BUSINESSPARTNER_CODE,
          productSummaryWebRequest);
    } catch (ApplicationRuntimeException e) {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      assertEquals(ErrorCategory.VALIDATION.getMessage(), e.getErrorCodes().getMessage());
    }
  }

  @Test
  public void downloadAllProductExceptionForDifferentBPCodeTest() throws Exception {
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", true);
    productSummaryWebRequest.setMerchantCode("merchantCode");
    try {
      this.productService.downloadAllProduct(USERNAME, true, BUSINESSPARTNER_CODE,
          productSummaryWebRequest);
    } catch (ValidationException validationException) {
      assertEquals(ErrorMessages.INVALID_GDN_SKU, validationException.getMessage());
    }
  }


  @Test
  public void  downloadAllProductExceptionForSameBPCodeTest() throws Exception {
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", true);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(null);
    productSummaryWebRequest.setMerchantCode(BUSINESSPARTNER_CODE);
    try {
      this.productService.downloadAllProduct(USERNAME, true, BUSINESSPARTNER_CODE,
          productSummaryWebRequest);
    } catch (ApplicationRuntimeException e) {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      assertEquals(ErrorCategory.VALIDATION.getMessage(), e.getErrorCodes().getMessage());
    }
  }

  @Test
  public void getStockInfoWebSiteTest() throws Exception {
    when(this.pbpFeign.getStockInfoWebSite(BUSINESSPARTNER_CODE, ITEM_SKU))
        .thenReturn(new GdnRestSingleResponse<>(new ProductLevel3StockInfoWebSiteResponse(), REQUEST_ID));
    this.productService.getStockInfoWebSite(BUSINESSPARTNER_CODE, ITEM_SKU);
    verify(this.pbpFeign).getStockInfoWebSite(BUSINESSPARTNER_CODE, ITEM_SKU);
  }

  @Test
  public void getStockInfoWebSiteExceptionTest() throws Exception {
    when(this.pbpFeign.getStockInfoWebSite(BUSINESSPARTNER_CODE, ITEM_SKU))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID));
    try {
      this.productService.getStockInfoWebSite(BUSINESSPARTNER_CODE, ITEM_SKU);
    } catch (Exception e) {
      verify(this.pbpFeign).getStockInfoWebSite(BUSINESSPARTNER_CODE, ITEM_SKU);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateTest() throws Exception {
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      profileResponse.getCompany().setCncActivated(true);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummaryByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU)).thenReturn(
          productLevel3SummaryResponseGdnRestSingleResponse);
      productService.downloadTemplateBulkUpdate(USERNAME, BUSINESSPARTNER_CODE, true, bulkRequest,
          httpServletResponse);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(pbpFeign).filterSummaryByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateForAmphiUserTest() throws Exception {
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummaryByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU)).thenReturn(
          productLevel3SummaryResponseGdnRestSingleResponse);
      productService.downloadTemplateBulkUpdate(USERNAME, BUSINESSPARTNER_CODE, false, bulkRequest,
          httpServletResponse);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(pbpFeign).filterSummaryByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateForAmphiUserTest_2() throws Exception {
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
          new ProductLevel3ViewConfigResponse();
      productLevel3ViewConfigResponse.setBuyable(false);
      productLevel3ViewConfigResponse.setDisplay(false);
      productLevel3SummaryResponseGdnRestSingleResponse.getValue().getViewConfigs().get(0)
          .setDisplay(true);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummaryByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU)).thenReturn(
          productLevel3SummaryResponseGdnRestSingleResponse);
      productService.downloadTemplateBulkUpdate(USERNAME, BUSINESSPARTNER_CODE, false, bulkRequest,
          httpServletResponse);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(pbpFeign).filterSummaryByGdnSku(BUSINESSPARTNER_CODE, ITEM_SKU);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateExceptionTest() throws Exception {
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(null);
    try {
      productService.downloadTemplateBulkUpdate(USERNAME, BUSINESSPARTNER_CODE, true, bulkRequest, httpServletResponse);
    } catch (ApplicationRuntimeException e) {
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      assertEquals(ErrorCategory.VALIDATION.getMessage(), e.getErrorCodes().getMessage());
    }
  }

  @Test
  public void checkSyncStockModeAndProductPermissionTest() {
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      boolean response =
          this.productService.checkSyncStockModeAndProductPermission(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      assertTrue(response);
    }
  }

  @Test
  public void checkSyncStockModeAndProductPermissionExceptionTest() {
    profileResponse.setCompany(null);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    try {
      this.productService.checkSyncStockModeAndProductPermission(BUSINESSPARTNER_CODE);
    } catch (ApplicationRuntimeException e) {
      assertEquals(ErrorCategory.DATA_NOT_FOUND, e.getErrorCodes());
      assertTrue(e.getErrorMessage().contains(ErrorMessages.DATA_NOT_COMPLETE));
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  public void getProductItemsByUPCCodeAndCategoryIdsTest() {
    upcCodeSearchRequest.setCategoryIds(Arrays.asList(CATEGORY_ID));
    upcCodeSearchRequest.setCategoryCodes(null);
    when(this.pcbFeign
        .getProductItemsByUPCCodeAndCategory(eq(PAGE), eq(SIZE), any(UPCCodeSearchRequest.class), eq(Boolean.TRUE)))
        .thenReturn(productItemDetailResponses);
    Page<ProductItemDetailWebResponse> response = this.productService
        .getProductItemsByUPCCodeAndCategoryIds(UPC_CODE, Arrays.asList(CATEGORY_ID), PAGE, SIZE, Boolean.TRUE);
    verify(this.pcbFeign)
        .getProductItemsByUPCCodeAndCategory(eq(PAGE), eq(SIZE), eq(upcCodeSearchRequest), eq(Boolean.TRUE));
    assertNotNull(response);
    assertEquals(response.getContent().get(0).getProductItemWebResponse().getId(), ID);
  }

  @Test
  public void getProductItemsByUPCCodeAndCategoryIdsExceptionTest() {
    upcCodeSearchRequest.setCategoryIds(Arrays.asList(CATEGORY_ID));
    upcCodeSearchRequest.setCategoryCodes(null);
    when(this.pcbFeign
        .getProductItemsByUPCCodeAndCategory(eq(PAGE), eq(SIZE), any(UPCCodeSearchRequest.class), eq(Boolean.TRUE)))
        .thenThrow(ClientException.class);
    Page<ProductItemDetailWebResponse> response = null;
    try {
      response = this.productService
          .getProductItemsByUPCCodeAndCategoryIds(UPC_CODE, Arrays.asList(CATEGORY_ID), PAGE, SIZE, true);
    } catch (ClientException e) {
    } finally {
      verify(this.pcbFeign)
          .getProductItemsByUPCCodeAndCategory(eq(PAGE), eq(SIZE), eq(upcCodeSearchRequest), eq(Boolean.TRUE));
      assertNull(response);
    }
  }

  @Test
  public void findDetailByGdnSkuTest() {
    productResponse.getValue().setItems(new ArrayList<>());
    productResponse.getValue().getItems().add(new ProductItemLevel3Response());
    productResponse.getValue().getItems().get(0).setItemSku(ITEM_SKU);
    productResponse.getValue().getItems().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    productResponse.getValue().setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productResponse.getValue().setVersion(VERSION);
    productResponse.getValue().setProductScore(new ProductScoreResponse(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));
    productResponse.getValue().setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse<SimpleLongResponse> simpleLongResponseGdnRestSingleResponse = new GdnRestSingleResponse<>();
    simpleLongResponseGdnRestSingleResponse.setValue(new SimpleLongResponse());
    simpleLongResponseGdnRestSingleResponse.setSuccess(true);
    simpleLongResponseGdnRestSingleResponse.getValue().setValue(1L);
    productResponse.getValue().setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    PickupPointDTO pickupPointDTO = new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    GeolocationDTO geolocationDTO = new GeolocationDTO();
    geolocationDTO.setLatitude(67.765);
    geolocationDTO.setLongitude(76.32);
    PickupPointDTO pickupPointDTO2 = new PickupPointDTO();
    pickupPointDTO2.setCode(PRODUCT_CODE);
    pickupPointDTO.setGeolocation(geolocationDTO);
    List<PickupPointDTO> pickupPointDTOList = new ArrayList<>();
    pickupPointDTOList.add(pickupPointDTO);
    pickupPointDTOList.add(pickupPointDTO2);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
        .thenReturn(pickupPointResponseList);
    when(pbpFeign.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU)).thenReturn(productResponse);
    when(pbpFeign.isPristineCategory(CATEGORY_ID)).thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, true));
    when(this.xCampaignFeign.getCampaignAvailability(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.xProductFeign.getL3CountByProductCode(STORE_ID, REQUEST_ID, PRODUCT_CODE))
        .thenReturn(simpleLongResponseGdnRestSingleResponse);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductLevel3MasterWebResponse productLevel3MasterWebResponse =
        this.productService.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.pbpFeign).isPristineCategory(CATEGORY_ID);
    verify(this.xProductFeign).getL3CountByProductCode(STORE_ID, REQUEST_ID, PRODUCT_CODE);
    verify(this.xCampaignFeign).getCampaignAvailability(productCampaignAvailabilityRequest);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    assertNotNull(productResponse);
    assertNotNull(profileResponse);
    assertTrue(productLevel3MasterWebResponse.getProductLevel3().getItems().get(0).isItemCampaignMapped());
    assertEquals(VERSION, productLevel3MasterWebResponse.getProductLevel3().getVersion());
    assertEquals(true, productLevel3MasterWebResponse.getProductLevel3().isProductEditable());
    assertEquals(PICKUP_POINT_CODE,
        productLevel3MasterWebResponse.getProductLevel3().getDistinctPickUpPoints().get(0).getPickupPointCode());
  }

  @Test
  public void findDetailByGdnSkuSharedProductTrueTest() {
    ReflectionTestUtils.setField(productService, "sharedProductDetailsForEditableFlag", true);
    productResponse.getValue().setItems(new ArrayList<>());
    productResponse.getValue().getItems().add(new ProductItemLevel3Response());
    productResponse.getValue().getItems().get(0).setItemSku(ITEM_SKU);
    productResponse.getValue().getItems().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    productResponse.getValue().setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productResponse.getValue().setVersion(VERSION);
    productResponse.getValue().setProductScore(new ProductScoreResponse(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));
    productResponse.getValue().setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse<SimpleBooleanResponse> simpleBooleanResponseGdnRestSingleResponse = new GdnRestSingleResponse<>();
    simpleBooleanResponseGdnRestSingleResponse.setValue(new SimpleBooleanResponse());
    simpleBooleanResponseGdnRestSingleResponse.setSuccess(true);
    simpleBooleanResponseGdnRestSingleResponse.getValue().setResult(false);
    productResponse.getValue().setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    PickupPointDTO pickupPointDTO = new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    GeolocationDTO geolocationDTO = new GeolocationDTO();
    geolocationDTO.setLatitude(67.765);
    geolocationDTO.setLongitude(76.32);
    PickupPointDTO pickupPointDTO2 = new PickupPointDTO();
    pickupPointDTO2.setCode(PRODUCT_CODE);
    pickupPointDTO.setGeolocation(geolocationDTO);
    List<PickupPointDTO> pickupPointDTOList = new ArrayList<>();
    pickupPointDTOList.add(pickupPointDTO);
    pickupPointDTOList.add(pickupPointDTO2);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
        .thenReturn(pickupPointResponseList);
    when(pbpFeign.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU)).thenReturn(productResponse);
    when(pbpFeign.isPristineCategory(CATEGORY_ID)).thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, true));
    when(this.xCampaignFeign.getCampaignAvailability(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.xProductFeign.sharedProductByProductCode(PRODUCT_CODE))
        .thenReturn(simpleBooleanResponseGdnRestSingleResponse);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductLevel3MasterWebResponse productLevel3MasterWebResponse =
        this.productService.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.pbpFeign).isPristineCategory(CATEGORY_ID);
    verify(this.xProductFeign).sharedProductByProductCode(PRODUCT_CODE);
    verify(this.xCampaignFeign).getCampaignAvailability(productCampaignAvailabilityRequest);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    assertNotNull(productResponse);
    assertNotNull(profileResponse);
    assertTrue(productLevel3MasterWebResponse.getProductLevel3().getItems().get(0).isItemCampaignMapped());
    assertEquals(VERSION, productLevel3MasterWebResponse.getProductLevel3().getVersion());
    assertEquals(true, productLevel3MasterWebResponse.getProductLevel3().isProductEditable());
    assertEquals(PICKUP_POINT_CODE,
        productLevel3MasterWebResponse.getProductLevel3().getDistinctPickUpPoints().get(0).getPickupPointCode());
  }

  @Test
  public void findDetailByGdnSkuSharedProductFalseTest() {
    ReflectionTestUtils.setField(productService, "sharedProductDetailsForEditableFlag", true);
    productResponse.getValue().setItems(new ArrayList<>());
    productResponse.getValue().getItems().add(new ProductItemLevel3Response());
    productResponse.getValue().getItems().get(0).setItemSku(ITEM_SKU);
    productResponse.getValue().getItems().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    productResponse.getValue().setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productResponse.getValue().setVersion(VERSION);
    productResponse.getValue().setProductScore(new ProductScoreResponse(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));
    productResponse.getValue().setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse<SimpleBooleanResponse> simpleBooleanResponseGdnRestSingleResponse = new GdnRestSingleResponse<>();
    simpleBooleanResponseGdnRestSingleResponse.setValue(new SimpleBooleanResponse());
    simpleBooleanResponseGdnRestSingleResponse.setSuccess(true);
    simpleBooleanResponseGdnRestSingleResponse.getValue().setResult(true);
    productResponse.getValue().setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    PickupPointDTO pickupPointDTO = new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    GeolocationDTO geolocationDTO = new GeolocationDTO();
    geolocationDTO.setLatitude(67.765);
    geolocationDTO.setLongitude(76.32);
    PickupPointDTO pickupPointDTO2 = new PickupPointDTO();
    pickupPointDTO2.setCode(PRODUCT_CODE);
    pickupPointDTO.setGeolocation(geolocationDTO);
    List<PickupPointDTO> pickupPointDTOList = new ArrayList<>();
    pickupPointDTOList.add(pickupPointDTO);
    pickupPointDTOList.add(pickupPointDTO2);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
        .thenReturn(pickupPointResponseList);
    when(pbpFeign.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU)).thenReturn(productResponse);
    when(pbpFeign.isPristineCategory(CATEGORY_ID)).thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, true));
    when(this.xCampaignFeign.getCampaignAvailability(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.xProductFeign.sharedProductByProductCode(PRODUCT_CODE))
        .thenReturn(simpleBooleanResponseGdnRestSingleResponse);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductLevel3MasterWebResponse productLevel3MasterWebResponse =
        this.productService.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.pbpFeign).isPristineCategory(CATEGORY_ID);
    verify(this.xProductFeign).sharedProductByProductCode(PRODUCT_CODE);
    verify(this.xCampaignFeign).getCampaignAvailability(productCampaignAvailabilityRequest);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    assertNotNull(productResponse);
    assertNotNull(profileResponse);
    assertTrue(productLevel3MasterWebResponse.getProductLevel3().getItems().get(0).isItemCampaignMapped());
    assertEquals(VERSION, productLevel3MasterWebResponse.getProductLevel3().getVersion());
    assertEquals(false, productLevel3MasterWebResponse.getProductLevel3().isProductEditable());
    assertEquals(PICKUP_POINT_CODE,
        productLevel3MasterWebResponse.getProductLevel3().getDistinctPickUpPoints().get(0).getPickupPointCode());
  }

  @Test
  public void findDetailByGdnSkuProductNotEditableTest() {
    productResponse.getValue().setItems(new ArrayList<>());
    productResponse.getValue().getItems().add(new ProductItemLevel3Response());
    productResponse.getValue().getItems().get(0).setItemSku(ITEM_SKU);
    productResponse.getValue().getItems().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    productResponse.getValue().setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productResponse.getValue().setVersion(VERSION);
    productResponse.getValue().setProductScore(new ProductScoreResponse(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));
    productResponse.getValue().setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse<SimpleLongResponse> simpleLongResponseGdnRestSingleResponse = new GdnRestSingleResponse<>();
    simpleLongResponseGdnRestSingleResponse.setValue(new SimpleLongResponse());
    simpleLongResponseGdnRestSingleResponse.setSuccess(true);
    simpleLongResponseGdnRestSingleResponse.getValue().setValue(3L);
    productResponse.getValue().setPickupPointCodes(new ArrayList<>());
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
        .thenReturn(pickupPointResponseList);
    when(pbpFeign.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU)).thenReturn(productResponse);
    when(pbpFeign.isPristineCategory(CATEGORY_ID)).thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, true));
    when(this.xCampaignFeign.getCampaignAvailability(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.xProductFeign.getL3CountByProductCode(STORE_ID, REQUEST_ID, PRODUCT_CODE))
        .thenReturn(simpleLongResponseGdnRestSingleResponse);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    ProductLevel3MasterWebResponse productLevel3MasterWebResponse =
        this.productService.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.pbpFeign).isPristineCategory(CATEGORY_ID);
    verify(this.xCampaignFeign).getCampaignAvailability(productCampaignAvailabilityRequest);
    verify(this.xProductFeign).getL3CountByProductCode(STORE_ID, REQUEST_ID, PRODUCT_CODE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    assertNotNull(productResponse);
    assertNotNull(profileResponse);
    assertTrue(productLevel3MasterWebResponse.getProductLevel3().getItems().get(0).isItemCampaignMapped());
    assertEquals(VERSION, productLevel3MasterWebResponse.getProductLevel3().getVersion());
    assertEquals(false, productLevel3MasterWebResponse.getProductLevel3().isProductEditable());
  }

  @Test
  public void findDetailByGdnSkuProductNotEditableMppTest() {
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productResponse.getValue().setItems(new ArrayList<>());
    productResponse.getValue().getItems().add(new ProductItemLevel3Response());
    productResponse.getValue().getItems().get(0).setItemSku(ITEM_SKU);
    productResponse.getValue().getItems().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    productResponse.getValue().setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productResponse.getValue().setVersion(VERSION);
    productResponse.getValue().setProductScore(new ProductScoreResponse(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));
    productResponse.getValue().setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse<SimpleLongResponse> simpleLongResponseGdnRestSingleResponse = new GdnRestSingleResponse<>();
    simpleLongResponseGdnRestSingleResponse.setValue(new SimpleLongResponse());
    simpleLongResponseGdnRestSingleResponse.setSuccess(true);
    simpleLongResponseGdnRestSingleResponse.getValue().setValue(3L);
    productResponse.getValue().setPickupPointCodes(new ArrayList<>());
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
        .thenReturn(pickupPointResponseList);
    when(pbpFeign.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU)).thenReturn(productResponse);
    when(pbpFeign.isPristineCategory(CATEGORY_ID)).thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, true));
    when(this.xCampaignFeign.getCampaignAvailabilityV2(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.xProductFeign.getL3CountByProductCode(STORE_ID, REQUEST_ID, PRODUCT_CODE))
        .thenReturn(simpleLongResponseGdnRestSingleResponse);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    ProductLevel3MasterWebResponse productLevel3MasterWebResponse =
        this.productService.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.pbpFeign).isPristineCategory(CATEGORY_ID);
    verify(this.xCampaignFeign).getCampaignAvailabilityV2(productCampaignAvailabilityRequest);
    verify(this.xProductFeign).getL3CountByProductCode(STORE_ID, REQUEST_ID, PRODUCT_CODE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    assertNotNull(productResponse);
    assertNotNull(profileResponse);
    assertTrue(productLevel3MasterWebResponse.getProductLevel3().getItems().get(0).isItemCampaignMapped());
    assertEquals(VERSION, productLevel3MasterWebResponse.getProductLevel3().getVersion());
    assertFalse(productLevel3MasterWebResponse.getProductLevel3().isProductEditable());
  }

  @Test
  public void findDetailByGdnSkuProductNotEditablePricingMppTestTest() {
    ReflectionTestUtils.setField(productService, "pricingMultiPickupPointEnabled", true);
    productResponse.getValue().setItems(new ArrayList<>());
    productResponse.getValue().getItems().add(new ProductItemLevel3Response());
    productResponse.getValue().getItems().get(0).setItemSku(ITEM_SKU);
    productResponse.getValue().getItems().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    productResponse.getValue().setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productResponse.getValue().setVersion(VERSION);
    productResponse.getValue().setProductScore(new ProductScoreResponse(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));
    productResponse.getValue().setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse<SimpleLongResponse> simpleLongResponseGdnRestSingleResponse = new GdnRestSingleResponse<>();
    simpleLongResponseGdnRestSingleResponse.setValue(new SimpleLongResponse());
    simpleLongResponseGdnRestSingleResponse.setSuccess(true);
    simpleLongResponseGdnRestSingleResponse.getValue().setValue(3L);
    productResponse.getValue().setPickupPointCodes(new ArrayList<>());
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
        .thenReturn(pickupPointResponseList);
    when(pbpFeign.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU)).thenReturn(productResponse);
    when(pbpFeign.isPristineCategory(CATEGORY_ID)).thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, true));
    when(this.xCampaignFeign.getCampaignAvailabilityV2(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.xProductFeign.getL3CountByProductCode(STORE_ID, REQUEST_ID, PRODUCT_CODE))
        .thenReturn(simpleLongResponseGdnRestSingleResponse);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    ProductLevel3MasterWebResponse productLevel3MasterWebResponse =
        this.productService.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.pbpFeign).isPristineCategory(CATEGORY_ID);
    verify(this.xCampaignFeign).getCampaignAvailabilityV2(productCampaignAvailabilityRequest);
    verify(this.xProductFeign).getL3CountByProductCode(STORE_ID, REQUEST_ID, PRODUCT_CODE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    assertNotNull(productResponse);
    assertNotNull(profileResponse);
    assertTrue(productLevel3MasterWebResponse.getProductLevel3().getItems().get(0).isItemCampaignMapped());
    assertEquals(VERSION, productLevel3MasterWebResponse.getProductLevel3().getVersion());
    assertFalse(productLevel3MasterWebResponse.getProductLevel3().isProductEditable());
  }

  @Test
  public void findDetailByGdnSkuWithEmptyProductCodeResponseTest() {
    productResponse.getValue().setItems(new ArrayList<>());
    productResponse.getValue().getItems().add(new ProductItemLevel3Response());
    productResponse.getValue().getItems().get(0).setItemSku(ITEM_SKU);
    productResponse.getValue().getItems().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    productResponse.getValue().setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productResponse.getValue().setVersion(VERSION);
    productResponse.getValue().setProductScore(new ProductScoreResponse(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));
    productResponse.getValue().setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse<SimpleLongResponse> simpleLongResponseGdnRestSingleResponse = new GdnRestSingleResponse<>();
    simpleLongResponseGdnRestSingleResponse.setValue(new SimpleLongResponse());
    simpleLongResponseGdnRestSingleResponse.setSuccess(true);
    simpleLongResponseGdnRestSingleResponse.getValue().setValue(3L);
    productResponse.getValue().setPickupPointCodes(new ArrayList<>());
    productResponse.getValue().setProductCode(StringUtils.EMPTY);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
        .thenReturn(pickupPointResponseList);
    when(pbpFeign.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU)).thenReturn(productResponse);
    when(pbpFeign.isPristineCategory(CATEGORY_ID)).thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, true));
    when(this.xCampaignFeign.getCampaignAvailability(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductLevel3MasterWebResponse productLevel3MasterWebResponse =
        this.productService.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
    verify(this.pbpFeign).isPristineCategory(CATEGORY_ID);
    verify(this.xCampaignFeign).getCampaignAvailability(productCampaignAvailabilityRequest);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    assertNotNull(productResponse);
    assertNotNull(profileResponse);
    assertTrue(productLevel3MasterWebResponse.getProductLevel3().getItems().get(0).isItemCampaignMapped());
    assertEquals(VERSION, productLevel3MasterWebResponse.getProductLevel3().getVersion());
    assertEquals(false, productLevel3MasterWebResponse.getProductLevel3().isProductEditable());
  }

  @Test
  void findDetailByGdnSkuTest_whenPBPClientExceptionTest() {
    ProductLevel3MasterWebResponse productResponse = null;
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
        .thenReturn(pickupPointResponseList);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> productService.findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU));
    } finally {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).findDetailByGdnSku(BUSINESSPARTNER_CODE, GDN_SKU);
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getStoreId();
    }
  }

  @Test
  public void unsynchronizeProductTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_CHANGE_PRODUCT_SYNCHRONIZATION_CHANGE_DATA};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      profileResponse.setCompany(null);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.pbpFeign.unsynchronizeProduct(DEFAULT_CLIENT_HOST, GDN_SKU, ITEM_SKU)).thenReturn(
          new GdnBaseRestResponse(Boolean.TRUE));
      this.productService.unsynchronizeProduct(BUSINESSPARTNER_CODE, GDN_SKU, ITEM_SKU);
      verify(this.pbpFeign).unsynchronizeProduct(DEFAULT_CLIENT_HOST, GDN_SKU, ITEM_SKU);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  public void unsynchronizeProduct_ExceptionTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_CHANGE_PRODUCT_SYNCHRONIZATION_CHANGE_DATA};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.pbpFeign.unsynchronizeProduct(DEFAULT_CLIENT_HOST, GDN_SKU, ITEM_SKU))
        .thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    try {
      this.productService.unsynchronizeProduct(BUSINESSPARTNER_CODE, GDN_SKU, ITEM_SKU);
    } catch (Exception e) {
      throw e;
    } finally {
      verify(this.pbpFeign).unsynchronizeProduct(DEFAULT_CLIENT_HOST, GDN_SKU, ITEM_SKU);
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
    }
  }

  @Test
  public void unsynchronizeProduct_ExceptionUnauthorizedErrorTest() {
    String errorMessage = StringUtils.EMPTY;
    String[] accessibility = {};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
    try {
      this.productService.unsynchronizeProduct(BUSINESSPARTNER_CODE, GDN_SKU, ITEM_SKU);
    } catch (ApplicationRuntimeException e) {
      errorMessage = e.getErrorMessage();
    } finally {
      assertEquals(UNAUTHORIZE_ERROR, errorMessage);
    }
    }
  }

  @Test
  public void synchronizeProductTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_CHANGE_PRODUCT_SYNCHRONIZATION_CHANGE_DATA};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      profileResponse.setCompany(null);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.pbpFeign.synchronizeProduct(DEFAULT_CLIENT_HOST, GDN_SKU, ITEM_SKU)).thenReturn(
          new GdnBaseRestResponse(Boolean.TRUE));
      this.productService.synchronizeProduct(BUSINESSPARTNER_CODE, GDN_SKU, ITEM_SKU);
      verify(this.pbpFeign).synchronizeProduct(DEFAULT_CLIENT_HOST, GDN_SKU, ITEM_SKU);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  public void synchronizeProduct_ExceptionTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_CHANGE_PRODUCT_SYNCHRONIZATION_CHANGE_DATA};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.pbpFeign.synchronizeProduct(DEFAULT_CLIENT_HOST, GDN_SKU, ITEM_SKU))
        .thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    try {
      this.productService.synchronizeProduct(BUSINESSPARTNER_CODE, GDN_SKU, ITEM_SKU);
    } catch (Exception e) {
      throw e;
    } finally {
      verify(this.pbpFeign).synchronizeProduct(DEFAULT_CLIENT_HOST, GDN_SKU, ITEM_SKU);
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
    }
  }

  @Test
  public void synchronizeProduct_ExceptionUnauthorizedErrorTest() {
    String errorMessage = StringUtils.EMPTY;
    String[] accessibility = {};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
    try {
      this.productService.synchronizeProduct(BUSINESSPARTNER_CODE, GDN_SKU, ITEM_SKU);
    } catch (ApplicationRuntimeException e) {
      errorMessage = e.getErrorMessage();
    } finally {
      assertEquals(UNAUTHORIZE_ERROR, errorMessage);
    }
    }
  }

  @Test
  public void updateProductImageTest() throws Exception {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_UPLOAD_IMAGE});
      Mockito.when(this.pbpFeign.updateProductImage(anyBoolean(), any(UpdateImageRequest.class)))
          .thenReturn(gdnBaseRestResponse);
      this.productService.updateProductImage(updateImageRequest, Boolean.TRUE);
      Mockito.verify(this.pbpFeign)
          .updateProductImage(eq(true), updateImageRequestArgumentCaptor.capture());
      assertEquals(PRODUCT_CODE, updateImageRequestArgumentCaptor.getValue().getProductCode());
      assertEquals(PRODUCT_SKU, updateImageRequestArgumentCaptor.getValue().getProductSku());
      assertEquals(ITEM_SKU, updateImageRequestArgumentCaptor.getValue().getItemSku());
    }
  }

  @Test
  public void updateProductImage_ExceptionTest() throws Exception {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.FUNCTION_MAINTAIN_PRODUK_SKU_SYNC_STOCK});
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.updateProductImage(updateImageRequest, Boolean.TRUE));
    }
  }

  @Test
  public void validateYouTubeUrlTest() throws Exception {
    when(this.pbpFeign.findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH))
        .thenReturn(systemParameterResponse);
    ReflectionTestUtils.setField(productService,"youtubeRegex",VIDEO_REGEX_WITH_SHORTS);
    when(youTube.videos()).thenReturn(videos);
    when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    when(list.execute()).thenReturn(videoListResponse);
    YouTubeAPIWebResponse response = this.productService.validateYouTubeUrl(YOUTUBE_URL);
    verify(this.pbpFeign).findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH);
    verify(youTube, Mockito.times(2)).videos();
    verify(list).execute();
    Assertions.assertTrue(response.isValid());
    Assertions.assertTrue(response.isValidationSwitch());
  }

  @Test
  public void validateYouTubeUrlFlagFalseTest() throws Exception {
    ReflectionTestUtils.setField(productService,"youtubeRegex",VIDEO_REGEX_WITH_SHORTS);
    systemParameterResponse.getValue().setValue("false");
    when(this.pbpFeign.findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH))
        .thenReturn(systemParameterResponse);
    when(youTube.videos()).thenReturn(videos);
    when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    when(list.execute()).thenReturn(videoListResponse);
    YouTubeAPIWebResponse response = this.productService.validateYouTubeUrl(YOUTUBE_URL);
    verify(this.pbpFeign).findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH);
    verify(youTube, Mockito.times(1)).videos();
    Assertions.assertTrue(response.isValid());
    assertFalse(response.isValidationSwitch());
  }

  @Test
  public void validateYouTubeUrlWithInvalidURLTest() throws Exception {
    ReflectionTestUtils.setField(productService,"youtubeRegex",VIDEO_REGEX_WITH_SHORTS);
    when(this.pbpFeign.findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH))
        .thenReturn(systemParameterResponse);
    YouTubeAPIWebResponse response = this.productService.validateYouTubeUrl(YOUTUBE_INVALID_URL);
    verify(this.pbpFeign).findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH);
    assertFalse(response.isValid());
    Assertions.assertTrue(response.isValidationSwitch());
  }

  @Test
  public void validateYouTubeUrlWithFalseParamTest() throws Exception {
    ReflectionTestUtils.setField(productService,"youtubeRegex",VIDEO_REGEX_WITH_SHORTS);
    systemParameterResponse.getValue().setValue(String.valueOf(Boolean.FALSE));
    when(this.pbpFeign.findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH))
        .thenReturn(systemParameterResponse);
    YouTubeAPIWebResponse response = this.productService.validateYouTubeUrl(YOUTUBE_INVALID_URL);
    verify(this.pbpFeign).findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH);
    assertFalse(response.isValid());
    assertFalse(response.isValidationSwitch());
  }

  @Test
  void validateYouTubeUrlWithPBPCallFailTest() throws Exception {
    when(this.pbpFeign.findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH)).thenReturn(null);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> this.productService.validateYouTubeUrl(YOUTUBE_INVALID_URL));
    } finally {
      verify(this.pbpFeign).findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH);
    }
  }

  @Test
  public void getMinimumPriceTest() {
    when(this.pbpFeign.getMinimumPrice()).thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, MINIMUM_PRICE));
    Integer value = this.productService.getMinimumPrice();
    verify(this.pbpFeign).getMinimumPrice();
    assertEquals(MINIMUM_PRICE, value);
  }

  @Test
  public void getMinimumPriceExceptionTest() {
    when(this.pbpFeign.getMinimumPrice()).thenReturn(null);
    try {
      Assertions.assertThrows(ClientException.class, () -> this.productService.getMinimumPrice());
    } finally {
      verify(this.pbpFeign).getMinimumPrice();
    }
  }

  private String[] accessibilities() {
    String[] accessibilities =
        {Accessibilty.MAINTAIN_PRODUK_SKU_DISPLAY_BUYABLE_CHANGE_DATA, Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA,
            Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA, Accessibilty.MAINTAIN_PRODUK_SKU_O2O_CHANGE_DATA,
            Accessibilty.MAINTAIN_PRODUK_SKU_PICKUP_POINT_CHANGE_DATA, Accessibilty.FUNCTION_MAINTAIN_PRODUK_SKU_SYNC_STOCK,
            Accessibilty.IS_PRIVILEGED_TO_EDIT_PRICE, Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK};
    return accessibilities;
  }

  private String[] accessibilities_withoutViewConfigEdit() {
    String[] accessibilities = {Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_O2O_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_PICKUP_POINT_CHANGE_DATA,
        Accessibilty.FUNCTION_MAINTAIN_PRODUK_SKU_SYNC_STOCK,
        Accessibilty.IS_PRIVILEGED_TO_EDIT_PRICE,
        Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK};
    return accessibilities;
  }

  private String[] accessibilities_withoutO2OEdit() {
    String[] accessibilities = {Accessibilty.MAINTAIN_PRODUK_SKU_DISPLAY_BUYABLE_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_PICKUP_POINT_CHANGE_DATA,
        Accessibilty.FUNCTION_MAINTAIN_PRODUK_SKU_SYNC_STOCK,
        Accessibilty.IS_PRIVILEGED_TO_EDIT_PRICE,
        Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK};
    return accessibilities;
  }

  @Test
  public void getBrandPredefinedAllowedAttributeValueDetailTest() {
    when(pcbFeign.getBrandPredefinedValueDetail(BRAND_CODE, BRAND_APPROVAL_STATUS_APPROVED))
        .thenReturn(new GdnRestSingleResponse<>(brandPredefinedAttributeValueResponse, REQUEST_ID));
    List<BrandPredefinedAttributeValueWebResponse> response =
        productService.getBrandPredefinedAllowedAttributeValueDetail(BRAND_CODE, BRAND_APPROVAL_STATUS_APPROVED);
    verify(pcbFeign).getBrandPredefinedValueDetail(BRAND_CODE, BRAND_APPROVAL_STATUS_APPROVED);
    Assertions.assertEquals(ID, response.get(0).getId());
    Assertions.assertEquals(BRAND, response.get(0).getValue());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS_APPROVED, response.get(0).getBrandApprovalStatus());
    Assertions.assertEquals(BRAND_CODE_1, response.get(0).getPredefinedAllowedAttributeCode());
    Assertions.assertEquals(10, response.get(0).getSequence(), 0);
    Assertions.assertEquals(BRAND_CODE, response.get(0).getBrandRequestCode());
  }

  @Test
  public void getBrandPredefinedAllowedAttributeValueDetailNullTestTest() {
    Exception exception = new Exception();
    when(pcbFeign.getBrandPredefinedValueDetail(BRAND_CODE, BRAND_APPROVAL_STATUS_APPROVED))
        .thenReturn(new GdnRestSingleResponse<>(ErrorMessages.BRAND_REJECTED_ERR_MESSAGE, null, true, null, null));
    try {
      productService.getBrandPredefinedAllowedAttributeValueDetail(BRAND_CODE, BRAND_APPROVAL_STATUS_APPROVED);
    } catch (Exception e) {
      exception = e;
    }
    verify(pcbFeign).getBrandPredefinedValueDetail(BRAND_CODE, BRAND_APPROVAL_STATUS_APPROVED);
    assertEquals(InvalidStateException.class, exception.getClass());
    assertEquals(ErrorMessages.BRAND_REJECTED_ERR_MESSAGE, exception.getMessage());
  }

  @Test
  public void updatePriceAndStockTest() {
    wholesalePriceSkuResponse.setPromoActive(Boolean.FALSE);
    Mockito.when(this.pbpFeign.updateItemStock(eq(DEFAULT_CLIENT_HOST), eq(BUSINESSPARTNER_CODE), any(ProductLevel3StockRequest.class)))
        .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    Mockito.when(this.pbpFeign.updateItemPrice(eq(DEFAULT_CLIENT_HOST), eq(ITEM_SKU), any(ProductPriceAndWholesaleRequest.class)))
        .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    when(this.productPricingFeign.getWholesalePriceDetail(ITEM_SKU))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePriceSkuResponse, REQUEST_ID));
    boolean result = this.productService.updatePriceAndStock(productPriceAndStockUpdateWebRequest, BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).updateItemStock(eq(DEFAULT_CLIENT_HOST), eq(BUSINESSPARTNER_CODE), any(ProductLevel3StockRequest.class));
    verify(this.pbpFeign).updateItemPrice(eq(DEFAULT_CLIENT_HOST), eq(ITEM_SKU), any(ProductPriceAndWholesaleRequest.class));
    verify(this.productPricingFeign).getWholesalePriceDetail(ITEM_SKU);
    assertTrue(result);
  }

  @Test
  public void updatePriceAndStockMppOnTest() {
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    wholesalePriceSkuResponse.setPromoActive(Boolean.FALSE);
    Mockito.when(this.pbpFeign.updateItemStock(eq(DEFAULT_CLIENT_HOST), eq(BUSINESSPARTNER_CODE), any(ProductLevel3StockRequest.class)))
        .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    Mockito.when(this.pbpFeign.updateItemPrice(eq(DEFAULT_CLIENT_HOST), eq(ITEM_SKU), any(ProductPriceAndWholesaleRequest.class)))
        .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    when(productPricingFeign.getWholesalePriceSkuDetailV2(eq(STORE_ID), eq(REQUEST_ID), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Collections.singletonList(wholesalePriceSkuResponse), null, null));
    productPriceAndStockUpdateWebRequest.setPickupPointCode(PICKUP_POINT_CODE);
    boolean result = this.productService.updatePriceAndStock(productPriceAndStockUpdateWebRequest, BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).updateItemStock(eq(DEFAULT_CLIENT_HOST), eq(BUSINESSPARTNER_CODE), any(ProductLevel3StockRequest.class));
    verify(this.pbpFeign).updateItemPrice(eq(DEFAULT_CLIENT_HOST), eq(ITEM_SKU), any(ProductPriceAndWholesaleRequest.class));
    verify(this.productPricingFeign).getWholesalePriceSkuDetailV2(eq(STORE_ID), eq(REQUEST_ID), any());
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getRequestId();
    assertTrue(result);
  }

  @Test
  public void updatePriceAndStockPricingMppOnTest() {
    ReflectionTestUtils.setField(productService, "pricingMultiPickupPointEnabled", true);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    wholesalePriceSkuResponse.setPromoActive(Boolean.FALSE);
    Mockito.when(this.pbpFeign.updateItemStock(eq(DEFAULT_CLIENT_HOST), eq(BUSINESSPARTNER_CODE), any(ProductLevel3StockRequest.class)))
        .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    Mockito.when(this.pbpFeign.updateItemPrice(eq(DEFAULT_CLIENT_HOST), eq(ITEM_SKU), any(ProductPriceAndWholesaleRequest.class)))
        .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    when(productPricingFeign.getWholesalePriceSkuDetailV2(eq(STORE_ID), eq(REQUEST_ID), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Collections.singletonList(wholesalePriceSkuResponse), null, null));
    productPriceAndStockUpdateWebRequest.setPickupPointCode(PICKUP_POINT_CODE);
    boolean result = this.productService.updatePriceAndStock(productPriceAndStockUpdateWebRequest, BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).updateItemStock(eq(DEFAULT_CLIENT_HOST), eq(BUSINESSPARTNER_CODE), any(ProductLevel3StockRequest.class));
    verify(this.pbpFeign).updateItemPrice(eq(DEFAULT_CLIENT_HOST), eq(ITEM_SKU), any(ProductPriceAndWholesaleRequest.class));
    verify(this.productPricingFeign).getWholesalePriceSkuDetailV2(eq(STORE_ID), eq(REQUEST_ID), any());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    assertTrue(result);
  }

  @Test
  public void updatePriceAndStockActiveWholesaleTest() {
    productPriceAndStockUpdateWebRequest.setWholesalePriceActivated(true);
    wholesalePriceSkuResponse.setPromoActive(Boolean.FALSE);
    Mockito.when(this.pbpFeign.updateItemStock(eq(DEFAULT_CLIENT_HOST), eq(BUSINESSPARTNER_CODE), any(ProductLevel3StockRequest.class)))
      .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    Mockito.when(this.pbpFeign.updateItemPrice(eq(DEFAULT_CLIENT_HOST), eq(ITEM_SKU), any(ProductPriceAndWholesaleRequest.class)))
      .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    when(this.productPricingFeign.getWholesalePriceDetail(ITEM_SKU))
      .thenReturn(new GdnRestSingleResponse<>(wholesalePriceSkuResponse, REQUEST_ID));
    boolean result = this.productService.updatePriceAndStock(productPriceAndStockUpdateWebRequest, BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).updateItemStock(eq(DEFAULT_CLIENT_HOST), eq(BUSINESSPARTNER_CODE), any(ProductLevel3StockRequest.class));
    verify(this.pbpFeign).updateItemPrice(eq(DEFAULT_CLIENT_HOST), eq(ITEM_SKU), any(ProductPriceAndWholesaleRequest.class));
    verify(this.productPricingFeign).getWholesalePriceDetail(ITEM_SKU);
    assertTrue(result);
  }

  @Test
  public void updatePriceAndStockActivePromoTest() {
    productPriceAndStockUpdateWebRequest.setWholesalePriceActivated(false);
    wholesalePriceSkuResponse.setPromoActive(Boolean.TRUE);
    Mockito.when(this.pbpFeign.updateItemStock(eq(DEFAULT_CLIENT_HOST), eq(BUSINESSPARTNER_CODE), any(ProductLevel3StockRequest.class)))
      .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    Mockito.when(this.pbpFeign.updateItemPrice(eq(DEFAULT_CLIENT_HOST), eq(ITEM_SKU), any(ProductPriceAndWholesaleRequest.class)))
      .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    when(this.productPricingFeign.getWholesalePriceDetail(ITEM_SKU))
      .thenReturn(new GdnRestSingleResponse<>(wholesalePriceSkuResponse, REQUEST_ID));
    boolean result = this.productService.updatePriceAndStock(productPriceAndStockUpdateWebRequest, BUSINESSPARTNER_CODE);
    verify(this.pbpFeign).updateItemStock(eq(DEFAULT_CLIENT_HOST), eq(BUSINESSPARTNER_CODE), any(ProductLevel3StockRequest.class));
    verify(this.pbpFeign).updateItemPrice(eq(DEFAULT_CLIENT_HOST), eq(ITEM_SKU), any(ProductPriceAndWholesaleRequest.class));
    verify(this.productPricingFeign).getWholesalePriceDetail(ITEM_SKU);
    assertTrue(result);
  }

  @Test
  void updatePriceAndStock_wholesalePromoTest() {
    wholesalePriceSkuResponse.setPromoActive(Boolean.TRUE);
    productPriceAndStockUpdateWebRequest.setWholesalePriceActivated(Boolean.TRUE);
    when(this.productPricingFeign.getWholesalePriceDetail(ITEM_SKU))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePriceSkuResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.updatePriceAndStock(productPriceAndStockUpdateWebRequest,
              BUSINESSPARTNER_CODE));
    } finally {
      verify(this.productPricingFeign).getWholesalePriceDetail(ITEM_SKU);
    }
  }

  @Test
  public void getPriceChangeCompatibilityTest() throws Exception {
    WholesalePriceSkuDetailListRequest request = new WholesalePriceSkuDetailListRequest();
    List<ItemInfoDto> itemInfo = new ArrayList<>();
    PriceChangeCompatibleRequest priceChangeCompatibleRequest = new PriceChangeCompatibleRequest();
    ItemInfoDto itemInfoDto = new ItemInfoDto();
    itemInfoDto.setItemSku(ITEM_SKU);
    itemInfoDto.setPickupPointCode(PICKUP_POINT_CODE);
    itemInfo.add(itemInfoDto);
    request.setItemInfo(itemInfo);
    priceChangeCompatibleRequest.setItemSku(ITEM_SKU);
    priceChangeCompatibleRequest.setPickUpPointCode(PICKUP_POINT_CODE);
    priceChangeCompatibleRequests.add(priceChangeCompatibleRequest);
    Mockito.when(this.productPricingService.getItemWholesaleConfig(any(), any(),
        any(WholesalePriceSkuDetailListRequest.class))).thenReturn(wholesalePriceSkuResponseMap);
    Mockito.when(this.pcbFeign.getCategoryWholesaleConfiguration(any(), any()))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    List<PriceChangeCompatibleResponse> priceChangeCompatibility =
        productService.getPriceChangeCompatibility(STORE_ID, REQUEST_ID, priceChangeCompatibleRequests);
    Mockito.verify(this.productPricingService).getItemWholesaleConfig(any(), any(),
        any(WholesalePriceSkuDetailListRequest.class));
    Mockito.verify(this.pcbFeign, times(2)).getCategoryWholesaleConfiguration(any(), any());
    assertTrue(priceChangeCompatibility.get(0).isUpdateAllowed());
  }

  @Test
  public void getPriceChangeCompatibility_MinPriceTest() throws Exception {
    Mockito.when(this.productPricingService.getItemWholesaleConfig(any(), any(),
        any(WholesalePriceSkuDetailListRequest.class))).thenReturn(wholesalePriceSkuResponseMap);
    Mockito.when(this.pcbFeign.getCategoryWholesaleConfiguration(any(), any()))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    List<PriceChangeCompatibleResponse> priceChangeCompatibility =
        productService.getPriceChangeCompatibility(STORE_ID, REQUEST_ID, priceChangeCompatibleRequests_1);
    Mockito.verify(this.productPricingService).getItemWholesaleConfig(any(), any(),
        any(WholesalePriceSkuDetailListRequest.class));
    Mockito.verify(this.pcbFeign).getCategoryWholesaleConfiguration(any(), any());
    assertTrue(priceChangeCompatibility.get(0).isUpdateAllowed());
  }

  @Test
  public void getWholesalePromoStatus() {
    wholesalePriceSkuResponse.setPromoActive(true);
    when(productPricingFeign.getWholesalePriceSkuDetail(STORE_ID, REQUEST_ID,
      new WholesalePriceSkuDetailListRequest(Collections.singleton(ITEM_SKU), null))).thenReturn(
      new GdnRestListResponse<>(null, null, true, Arrays.asList(wholesalePriceSkuResponse), null,
        null));
    List<WholesalePromoResponse> wholesalePromoStatus =
      productService.getWholesalePromoStatus(STORE_ID, REQUEST_ID, Arrays.asList(wholeSaleDetailListWebRequests));
    verify(this.productPricingFeign).getWholesalePriceSkuDetail(STORE_ID, REQUEST_ID,
      new WholesalePriceSkuDetailListRequest(Collections.singleton(ITEM_SKU), null));
    assertNotNull(wholesalePromoStatus);
    Assertions.assertEquals(1, wholesalePromoStatus.size());
    Assertions.assertTrue(wholesalePromoStatus.get(0).isWholesalePromo());
  }

  @Test
  public void getWholesalePromoStatus_setWholesalePromoNull() {
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    wholesalePriceSkuResponse.setPromoActive(null);
    when(productPricingFeign.getWholesalePriceSkuDetailV2(eq(STORE_ID), eq(REQUEST_ID), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(wholesalePriceSkuResponse), null, null));
    List<WholesalePromoResponse> wholesalePromoStatus =
        productService.getWholesalePromoStatus(STORE_ID, REQUEST_ID, Arrays.asList(wholeSaleDetailListWebRequests));
    verify(this.productPricingFeign).getWholesalePriceSkuDetailV2(eq(STORE_ID), eq(REQUEST_ID), any());
    assertNotNull(wholesalePromoStatus);
    Assertions.assertEquals(1, wholesalePromoStatus.size());
    assertFalse(wholesalePromoStatus.get(0).isWholesalePromo());
  }

  @Test
  public void getWholesalePromoStatusPricingMppOn() {
    ReflectionTestUtils.setField(productService, "pricingMultiPickupPointEnabled", true);
    wholesalePriceSkuResponse.setPromoActive(null);
    when(productPricingFeign.getWholesalePriceSkuDetailV2(eq(STORE_ID), eq(REQUEST_ID), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(wholesalePriceSkuResponse), null, null));
    List<WholesalePromoResponse> wholesalePromoStatus =
        productService.getWholesalePromoStatus(STORE_ID, REQUEST_ID, Arrays.asList(wholeSaleDetailListWebRequests));
    verify(this.productPricingFeign).getWholesalePriceSkuDetailV2(eq(STORE_ID), eq(REQUEST_ID), any());
    assertNotNull(wholesalePromoStatus);
    Assertions.assertEquals(1, wholesalePromoStatus.size());
    assertFalse(wholesalePromoStatus.get(0).isWholesalePromo());
  }

  @Test
  public void getPriceChangeCompatibilityNoCategoryWholesaleConfTest() throws Exception {
    Mockito.when(this.productPricingService.getItemWholesaleConfig(any(), any(),
        any(WholesalePriceSkuDetailListRequest.class))).thenReturn(wholesalePriceSkuResponseMap);
    wholesaleMappingResponse.setWholesaleConfig(new ArrayList<>());
    wholesaleMappingResponse.setConfigurationType(Constants.PERCENTAGE);
    Mockito.when(this.pcbFeign.getCategoryWholesaleConfiguration(any(), any()))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    List<PriceChangeCompatibleResponse> priceChangeCompatibility =
        productService.getPriceChangeCompatibility(STORE_ID, REQUEST_ID, priceChangeCompatibleRequests);
    Mockito.verify(this.productPricingService).getItemWholesaleConfig(any(), any(),
        any(WholesalePriceSkuDetailListRequest.class));
    Mockito.verify(this.pcbFeign).getCategoryWholesaleConfiguration(any(), any());
    assertTrue(priceChangeCompatibility.get(0).isUpdateAllowed());
  }

  @Test
  public void getProductScoreRuleTest() {
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setCategoryCode(CATEGORY_CODE);
    productScoreRuleResponse.setProductScoreRules(new HashMap<>());
    when(xProductFeign.getProductScoreRule(CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productScoreRuleResponse, REQUEST_ID));
    ProductScoreRuleWebResponse response = productService.getProductScoreRule(CATEGORY_CODE);
    verify(xProductFeign).getProductScoreRule(CATEGORY_CODE);
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
  }

  @Test
  void getProductScoreRuleExceptionTest() {
    when(xProductFeign.getProductScoreRule(CATEGORY_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> productService.getProductScoreRule(CATEGORY_CODE));
    } finally {
      verify(xProductFeign).getProductScoreRule(CATEGORY_CODE);
    }
  }

  @Test
  public void getSystemParameterSwitchesTest() {
    wholesalePriceSkuResponse.setPromoActive(true);
    when(pbpFeign.getSystemParameterSwitch()).thenReturn(
        new GdnRestSingleResponse<>(new ProductSystemParameterSwitchResponse(switchVariableMap), REQUEST_ID));
    ProductSystemParameterSwitchWebResponse productSystemParameterSwitchWebResponse =
        this.productService.getSystemParameterSwitches();
    verify(pbpFeign).getSystemParameterSwitch();
    Assertions.assertTrue((Boolean)
        productSystemParameterSwitchWebResponse.getProductSystemParameterSwitchValues().get(SWITCH_VARIABLE));
  }

  @Test
  public void getUpcCodeAndImagesTest() {
    when(xProductFeign.getProductAndItems(STORE_ID, REQUEST_ID, PRODUCT_SKU, false))
        .thenReturn(new GdnRestSingleResponse<>(new ProductAndItemsResponse(), REQUEST_ID));
    UpcCodeAndImagesWebResponse upcCodeAndImagesWebResponse =
        this.productService.getUpcCodeAndImages(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    verify(xProductFeign).getProductAndItems(STORE_ID, REQUEST_ID, PRODUCT_SKU, false);
    assertNotNull(upcCodeAndImagesWebResponse);
  }


  @Test
  public void checkSuccessfullOrderPlacedForProductSkuTest1() throws Exception {
    HitsResponse hitsResponse = new HitsResponse();
    hitsResponse.setTotal(0);
    AgpSimpleQueryResponse response = new AgpSimpleQueryResponse();
    response.setHits(hitsResponse);
    when(agpQueryFeign.findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X")).thenReturn(response);
    OrderPlacedWebResponse orderPlacedWebResponse =
        this.productService.checkSuccessfullOrderPlacedForProductSku(PRODUCT_SKU);
    verify(agpQueryFeign).findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X");
    assertFalse(orderPlacedWebResponse.isSuccessfulOrderPlaced());
  }

  @Test
  public void checkSuccessfullOrderPlacedForProductSkuTest2() throws Exception {
    HitsResponse hitsResponse = new HitsResponse();
    hitsResponse.setTotal(1);
    AgpSimpleQueryResponse response = new AgpSimpleQueryResponse();
    response.setHits(hitsResponse);
    when(agpQueryFeign.findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X")).thenReturn(response);
    OrderPlacedWebResponse orderPlacedWebResponse =
        this.productService.checkSuccessfullOrderPlacedForProductSku(PRODUCT_SKU);
    verify(agpQueryFeign).findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X");
    Assertions.assertTrue(orderPlacedWebResponse.isSuccessfulOrderPlaced());
  }

  @Test
  void checkSuccessfullOrderPlacedForProductSkuExceptionTest() throws Exception {
    when(agpQueryFeign.findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X")).thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.productService.checkSuccessfullOrderPlacedForProductSku(PRODUCT_SKU));
    } finally {
      verify(agpQueryFeign).findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X");
    }
  }

  @Test
  public void getOrderStatusByProductCodeTest() throws Exception {
    itemSkus.add(PRODUCT_SKU);
    AgpSimpleQueryResponse response =
        AgpSimpleQueryResponse.builder().hits(HitsResponse.builder().total(0).build()).build();
    AgpSimpleQueryResponse response1 =
        AgpSimpleQueryResponse.builder().hits(HitsResponse.builder().total(4).build()).build();
    when(pbpFeign.getProductSkusByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, new ProductSkuResponseList(itemSkus)));
    when(agpQueryFeign.findNumberOfOrder(ITEM_SKU_1, "0", "0", "DF,X")).thenReturn(response);
    when(agpQueryFeign.findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X")).thenReturn(response1);
    OrderStatusWebResponse orderStatusWebResponse = this.productService.getOrderStatusByProductCode(PRODUCT_CODE);
    verify(agpQueryFeign).findNumberOfOrder(ITEM_SKU_1, "0", "0", "DF,X");
    verify(agpQueryFeign).findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X");
    verify(pbpFeign).getProductSkusByProductCode(PRODUCT_CODE);
    Assertions.assertTrue(orderStatusWebResponse.isHasOrder());
  }

  @Test
  public void getOrderStatusByProductCode_ExceptionTest() throws Exception {
    when(pbpFeign.getProductSkusByProductCode(PRODUCT_CODE)).thenReturn(null);
    when(agpQueryFeign.findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X")).thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.productService.getOrderStatusByProductCode(PRODUCT_CODE));
    } finally {
      verify(pbpFeign).getProductSkusByProductCode(PRODUCT_CODE);
    }
  }

  @Test
  void getOrderStatusByProductCode_ExceptionTest2() throws Exception {
    when(pbpFeign.getProductSkusByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, new ProductSkuResponseList()));
    when(agpQueryFeign.findNumberOfOrder(PRODUCT_SKU, "0", "0", "DF,X")).thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.productService.getOrderStatusByProductCode(PRODUCT_CODE));
    } finally {
      verify(pbpFeign).getProductSkusByProductCode(PRODUCT_CODE);
    }
  }

  @Test
  public void editProductInfoTest() {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.editProductInfo(productLevel3Request, productLevel3Request.getProductSku(), false) )
        .thenReturn(editProductResponseGdnRestSingleResponse);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class);
        MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(
          () -> RequestHelper.toProductLevel3RequestFromProductEditInfoWebRequest(
              productEditInfoWebRequest)).thenReturn(productLevel3Request);
      this.productService.editProductInfo(productEditInfoWebRequest, BUSINESSPARTNER_CODE, false);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign)
          .editProductInfo(productLevel3Request, productLevel3Request.getProductSku(), false);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductInfoExceptionTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.editProductInfo(productLevel3Request, productLevel3Request.getProductSku(), true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID));
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class);
        MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(
          () -> RequestHelper.toProductLevel3RequestFromProductEditInfoWebRequest(
              productEditInfoWebRequest)).thenReturn(productLevel3Request);
      try {
        Assertions.assertThrows(ClientException.class,
            () -> this.productService.editProductInfo(productEditInfoWebRequest,
                BUSINESSPARTNER_CODE, true));
      } finally {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
        Mockito.verify(pbpFeign)
            .editProductInfo(productLevel3Request, productLevel3Request.getProductSku(), true);
      }
    }
  }

  @Test
  public void getProductLevel3VariantListTest() {
    ProductLevel3VariantsWebRequest productLevel3VariantsWebRequest = new ProductLevel3VariantsWebRequest();
    productLevel3VariantsWebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productLevel3VariantsWebRequest.setProductSku(PRODUCT_SKU);
    productLevel3VariantsWebRequest.setNeedCorrection(true);
    productLevel3SummaryDetailsResponseGdnRestListResponse.getContent().get(0).setItemCampaignMapped(true);
    when(this.pbpFeign.filterSummaryDetails(eq(productLevel3VariantsWebRequest.getBusinessPartnerCode()),
        eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()), any(ProductLevel3SummaryDetailsRequest.class)))
        .thenReturn(productLevel3SummaryDetailsResponseGdnRestListResponse);
    Page<ProductLevel3SummaryDetailsWebResponse> result =
        this.productService.getProductLevel3VariantList(productLevel3VariantsWebRequest, pageRequest);
    verify(this.pbpFeign).filterSummaryDetails(eq(productLevel3VariantsWebRequest.getBusinessPartnerCode()),
        eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
        productLevel3SummaryDetailsRequestArgumentCaptor.capture());
    assertEquals(PRODUCT_SKU, productLevel3SummaryDetailsRequestArgumentCaptor.getValue().getProductSku());
    assertEquals(BUSINESSPARTNER_CODE,
        productLevel3SummaryDetailsRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertNotNull(result);
    assertTrue(result.getContent().get(0).isItemCampaignMapped());
    assertEquals(90.0, result.getContent().get(0).getProductScore(), 0);
    assertEquals(10000.0, result.getContent().get(0).getCogs(), 0);
    assertEquals("error", result.getContent().get(0).getCogsErrorCode());
  }

  @Test
  public void getProductLevel3VariantListEmptyResponseTest() {
    ProductLevel3VariantsWebRequest productLevel3VariantsWebRequest = new ProductLevel3VariantsWebRequest();
    productLevel3VariantsWebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productLevel3VariantsWebRequest.setProductSku(PRODUCT_SKU);
    when(this.pbpFeign.filterSummaryDetails(eq(productLevel3VariantsWebRequest.getBusinessPartnerCode()),
        eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()), any(ProductLevel3SummaryDetailsRequest.class)))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), pageMetaData, REQUEST_ID));
    Page<ProductLevel3SummaryDetailsWebResponse> result =
        this.productService.getProductLevel3VariantList(productLevel3VariantsWebRequest, pageRequest);
    verify(this.pbpFeign).filterSummaryDetails(eq(productLevel3VariantsWebRequest.getBusinessPartnerCode()),
        eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
        productLevel3SummaryDetailsRequestArgumentCaptor.capture());
    assertNotNull(result);
    assertEquals(0, result.getContent().size());
  }

  @Test
  public void getProductLevel3VariantListNoCampaignMappingTest() {
    ProductLevel3VariantsWebRequest productLevel3VariantsWebRequest = new ProductLevel3VariantsWebRequest();
    productLevel3VariantsWebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    productLevel3VariantsWebRequest.setProductSku(PRODUCT_SKU);
    when(this.pbpFeign.filterSummaryDetails(eq(productLevel3VariantsWebRequest.getBusinessPartnerCode()),
        eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()), any(ProductLevel3SummaryDetailsRequest.class)))
        .thenReturn(productLevel3SummaryDetailsResponseGdnRestListResponse);
    Page<ProductLevel3SummaryDetailsWebResponse> result =
        this.productService.getProductLevel3VariantList(productLevel3VariantsWebRequest, pageRequest);
    verify(this.pbpFeign).filterSummaryDetails(eq(productLevel3VariantsWebRequest.getBusinessPartnerCode()),
        eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
        productLevel3SummaryDetailsRequestArgumentCaptor.capture());
    assertEquals(PRODUCT_SKU, productLevel3SummaryDetailsRequestArgumentCaptor.getValue().getProductSku());
    assertEquals(BUSINESSPARTNER_CODE,
        productLevel3SummaryDetailsRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertNotNull(result);
    assertFalse(result.getContent().get(0).isItemCampaignMapped());
    assertEquals(90.0, result.getContent().get(0).getProductScore(), 0);
  }

  @Test
  public void getPinpointStatusByProductSkuTest() {
    when(this.xProductFeign.getPickupPointCodesByProductSku(PRODUCT_SKU)).thenReturn(
        new GdnRestSingleResponse(null, null, true, ProductPickupPointListResponse.builder()
            .pickupPointCodes(new HashSet<>(Arrays.asList(BUSINESSPARTNER_CODE, PICKUP_POINT_CODE_1)))
            .build(), REQUEST_ID));
    when(this.businessPartnerService.filterByBusinessPartnerCode(PRODUCT_SKU))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(anyString()))
        .thenReturn(pickupPointResponseList);
    String response = productService.getPinpointStatusByProductSku(PRODUCT_SKU, PRODUCT_SKU);
    verify(this.xProductFeign).getPickupPointCodesByProductSku(PRODUCT_SKU);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(PRODUCT_SKU);
    Mockito.verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(anyString());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, response);
  }

  @Test
  public void getPickupPointCodesByProductSkuTest() throws Exception {
    when(pbpFeign.getPickupPointCodes(0, 1, PRODUCT_SKU, false, BUSINESSPARTNER_CODE, false)).thenReturn(
        new GdnRestListResponse<>(
            Arrays.asList(new PickupPointCodeResponse(ITEM_SKU, ITEM_NAME, PICKUP_POINT_CODE, PICKUP_POINT_CODE_1)),
            new PageMetaData(0, 1, 1), REQUEST_ID));
    Page<PickupPointCodeWebResponse> pickupPointCodeWebResponses =
        this.productService.getPickupPointCodesByProductSku(0, 1, PRODUCT_SKU, false, BUSINESSPARTNER_CODE, false);
    verify(pbpFeign).getPickupPointCodes(0, 1, PRODUCT_SKU, false, BUSINESSPARTNER_CODE, false);
    assertEquals(1, pickupPointCodeWebResponses.getContent().size());
    assertEquals(ITEM_SKU, pickupPointCodeWebResponses.getContent().get(0).getItemSku());
    assertEquals(ITEM_NAME, pickupPointCodeWebResponses.getContent().get(0).getItemName());
    assertEquals(PICKUP_POINT_CODE, pickupPointCodeWebResponses.getContent().get(0).getPickupPointCode());
  }

  @Test
  public void getUniquePickupPointCodesByProductSkuTest() throws Exception {
    when(pbpFeign.getUniquePickupPointCodes(PRODUCT_SKU)).thenReturn(
        new GdnRestSingleResponse<UniquePickupPointCodeResponse>(uniquePickupPointCodeResponse, REQUEST_ID));
    UniquePickupPointCodeWebResponse uniquePickupPointCodeWebResponse =
        this.productService.getUniquePickupPointCodesByProductSku(PRODUCT_SKU);
    verify(pbpFeign).getUniquePickupPointCodes(PRODUCT_SKU);
    assertEquals(1, uniquePickupPointCodeWebResponse.getItemSkus().size());
    assertEquals(1, uniquePickupPointCodeWebResponse.getPickupPointCodes().size());
    assertEquals(ITEM_SKU, uniquePickupPointCodeWebResponse.getItemSkus().toArray()[0]);
    assertEquals(PICKUP_POINT_CODE, uniquePickupPointCodeWebResponse.getPickupPointCodes().toArray()[0]);
  }

  @Test
  public void updateItemsPriceStockImages1Test() {
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    itemsPriceStockImagesUpdateResponse.setPostLive(true);
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(new ArrayList<>());
    GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> response =
        new GdnRestSingleResponse<>(itemsPriceStockImagesUpdateResponse, REQUEST_ID);
    when(pbpFeign.updateItemsPriceStockImages(PRODUCT_BUSINESS_PARTNER_ID, updateItemsPriceStockImagesRequest))
        .thenReturn(response);
    this.productService.updateItemsPriceStockImages(PRODUCT_BUSINESS_PARTNER_ID, updateItemsPriceStockImagesWebRequest);
    verify(pbpFeign)
        .updateItemsPriceStockImages(PRODUCT_BUSINESS_PARTNER_ID, updateItemsPriceStockImagesRequest);
    Assertions.assertTrue(response.getValue().isPostLive());
    Assertions.assertTrue(response.getValue().isProductReview());
  }

  @Test
  public void getProductEditHistorySummaryTest() throws Exception {
    HistorySummaryWebRequest historySummaryWebRequest =
        new HistorySummaryWebRequest(PRODUCT_SKU, VARIANT_NAME, KEYWORD, new Date(), new Date(), true);
    HistoryRequest historyRequest =
        new HistoryRequest(PRODUCT_SKU, VARIANT_NAME, KEYWORD, new Date(), new Date(), true);
    HistoryResponse historyResponse =
        new HistoryResponse(new Date(), ITEM_SKU, ITEM_NAME, CHANGED_BY, PRODUCT_NAME, PRODUCT_NAME_2, ACTIVITY);
    Mockito.when(pbpFeign.getProductHistorySummary(Mockito.eq(0), Mockito.eq(1), any(HistoryRequest.class))).thenReturn(
        new GdnRestListResponse<HistoryResponse>(Arrays.asList(historyResponse), new PageMetaData(0, 1, 1), REQUEST_ID));
    Page<HistorySummaryWebResponse> variantEditHistorySummeryWebResponses =
        this.productService.getProductEditHistorySummary(historySummaryWebRequest, 0, 1);
    Mockito.verify(pbpFeign).getProductHistorySummary(Mockito.eq(0), Mockito.eq(1), any(HistoryRequest.class));
    assertEquals(ITEM_SKU, variantEditHistorySummeryWebResponses.getContent().get(0).getGdnSku());
    assertEquals(ITEM_NAME, variantEditHistorySummeryWebResponses.getContent().get(0).getGdnName());
    assertEquals(CHANGED_BY, variantEditHistorySummeryWebResponses.getContent().get(0).getChangedBy());
    assertEquals(PRODUCT_NAME, variantEditHistorySummeryWebResponses.getContent().get(0).getOldValues());
    assertEquals(PRODUCT_NAME_2, variantEditHistorySummeryWebResponses.getContent().get(0).getNewValues());
    assertEquals(ACTIVITY, variantEditHistorySummeryWebResponses.getContent().get(0).getActivity());
    assertNotNull(variantEditHistorySummeryWebResponses.getContent().get(0).getAccessTime());
  }

  @Test
  public void updateLogisticsTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.updateLogistics(Boolean.valueOf(Constants.IS_EXTERNAL_ONLY), productLevel3UpdateRequest, false))
        .thenReturn(new GdnBaseRestResponse(true));
    this.productService
        .updateLogistics(productLevel3UpdateWebRequest, BUSINESSPARTNER_CODE, Constants.IS_EXTERNAL_ONLY, false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).updateLogistics(Boolean.valueOf(Constants.IS_EXTERNAL_ONLY), productLevel3UpdateRequest,
        false);
    Assertions.assertEquals(true, gdnBaseRestResponse.isSuccess());
  }

  @Test
  void updateLogisticsInvalidBusinessPartnerTest() throws Exception {
    profileResponse.setMerchantStatus("INACTIVE");
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.updateLogistics(productLevel3UpdateWebRequest,
              BUSINESSPARTNER_CODE, Constants.IS_EXTERNAL_ONLY, false));
    } finally {
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  void updateLogisticsNullBusinessPartnerTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.updateLogistics(productLevel3UpdateWebRequest,
              BUSINESSPARTNER_CODE, Constants.IS_EXTERNAL_ONLY, false));
    } finally {
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  public void updatePickupPointsTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.updatePickupPointCodes(any(PickupPointUpdateRequest.class))).thenReturn(
        new GdnRestSingleResponse<>(new PickupPointUpdateResponse(), REQUEST_ID));
    Mockito.doNothing().when(pickupPointService)
        .markDefaultAddressForBusinessPartner(StringUtils.EMPTY, StringUtils.EMPTY, BUSINESSPARTNER_CODE,
            PICKUP_POINT_CODE);
    this.productService.updatePickupPoints(pickupPointUpdateWebRequest, BUSINESSPARTNER_CODE);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    Mockito.verify(pbpFeign).updatePickupPointCodes(any(PickupPointUpdateRequest.class));
    verify(pickupPointService)
        .markDefaultAddressForBusinessPartner(StringUtils.EMPTY, StringUtils.EMPTY, BUSINESSPARTNER_CODE,
            PICKUP_POINT_CODE);
    markPickupPointAsDefaultRequest.setPickupPointCodes(Arrays.asList(PICKUP_POINT_CODE));
    markPickupPointAsDefaultRequest.setStoreCode(BUSINESSPARTNER_CODE);
    verify(businessPartnerService).updateDefaultPickupPointCode(markPickupPointAsDefaultRequest);
  }

  @Test
  void updatePickupPointsExceptionTest() throws Exception {
    profileResponse.setMerchantStatus("INACTIVE");
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.updatePickupPoints(pickupPointUpdateWebRequest,
              BUSINESSPARTNER_CODE));
    } finally {
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  void updatePickupPoints_nullBusinessPartnerExceptionTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.updatePickupPoints(pickupPointUpdateWebRequest,
              BUSINESSPARTNER_CODE));
    } finally {
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  public void updatePickupPointsDefaultAddressFalseTest() throws Exception {
    pickupPointUpdateWebRequest.setMarkDefaultAddress(false);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.updatePickupPointCodes(any(PickupPointUpdateRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(new PickupPointUpdateResponse(), REQUEST_ID));
    this.productService.updatePickupPoints(pickupPointUpdateWebRequest, BUSINESSPARTNER_CODE);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    Mockito.verify(pbpFeign).updatePickupPointCodes(any(PickupPointUpdateRequest.class));
  }

  @Test
  public void updatePickupPointsDefaultAddressNullTest() throws Exception {
    pickupPointUpdateWebRequest.setDefaultPickupPointCode(null);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.updatePickupPointCodes(any(PickupPointUpdateRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(new PickupPointUpdateResponse(), REQUEST_ID));
    this.productService.updatePickupPoints(pickupPointUpdateWebRequest, BUSINESSPARTNER_CODE);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    Mockito.verify(pbpFeign).updatePickupPointCodes(any(PickupPointUpdateRequest.class));
  }

  @Test
  public void getProductVariantsNameByProductSkuTest() {
    ProductItemNameResponse productItemNameResponse =
        ProductItemNameResponse.builder().itemName(ITEM_NAME).itemSku(ITEM_SKU).build();
    when(pbpFeign.getProductVariantsName(PAGE, SIZE, PRODUCT_SKU)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(productItemNameResponse), new PageMetaData(PAGE, SIZE, TOTAL_ELEMENTS),
            REQUEST_ID));
    Page<ProductItemNameWebResponse> responses =
        this.productService.getProductVariantsNameByProductSku(PRODUCT_SKU, PAGE, SIZE);
    verify(pbpFeign).getProductVariantsName(PAGE, SIZE, PRODUCT_SKU);
    Assertions.assertEquals(ITEM_NAME, responses.getContent().get(0).getItemName());
    Assertions.assertEquals(TOTAL_ELEMENTS, responses.getTotalElements());
  }

  @Test
  public void getL3DetailByProductSkuTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.getL3DetailByProductSku(PRODUCT_SKU_1, false))
        .thenReturn(new GdnRestSingleResponse<>(new ProductLevel3DetailResponse(), REQUEST_ID));
    this.productService.getL3DetailByProductSku(PRODUCT_SKU_1, BUSINESSPARTNER_CODE, false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).getL3DetailByProductSku(PRODUCT_SKU_1, false);
  }

  @Test
  void getL3DetailByProductSkuExceptionTest() throws Exception {
    profileResponse.setMerchantStatus("INACTIVE");
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.getL3DetailByProductSku(PRODUCT_SKU_1, BUSINESSPARTNER_CODE,
              false));
    } finally {
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(pbpFeign, times(0)).getL3DetailByProductSku(GDN_SKU, false);
    }
  }

  @Test
  void getL3DetailByProductSku_nullBusinessPartnerExceptionTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.getL3DetailByProductSku(PRODUCT_SKU_1, BUSINESSPARTNER_CODE,
              false));
    } finally {
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(pbpFeign, times(0)).getL3DetailByProductSku(GDN_SKU, false);
    }
  }

  @Test
  public void getL3DetailByProductSkuCogsViewableTest() throws Exception {
    profileResponse.getCompany().setPurchaseTerm("PO");
    ProductLevel3DetailResponse productLevel3DetailResponse = new ProductLevel3DetailResponse();
    productLevel3DetailResponse.setCategoryId(CATEGORY_ID);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.getL3DetailByProductSku(PRODUCT_SKU_1, false))
        .thenReturn(new GdnRestSingleResponse<>(productLevel3DetailResponse, REQUEST_ID));
    ProductLevel3DetailWebResponse productLevel3DetailWebResponse =
        this.productService.getL3DetailByProductSku(PRODUCT_SKU_1, BUSINESSPARTNER_CODE, false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).getL3DetailByProductSku(PRODUCT_SKU_1, false);
    Assertions.assertTrue(productLevel3DetailWebResponse.getCogsViewable());
  }

  @Test
  public void getInventorySummaryTest() throws Exception {
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKU(GDN_SKU, IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    this.productService.getInventorySummary(GDN_SKU, IS_WARE_HOUSE, BUSINESSPARTNER_CODE);
    verify(xInventoryFeign).reservedStockSummaryByWebSKU(GDN_SKU, IS_WARE_HOUSE);
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
    verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
  }

  @Test
  void getInventorySummaryInvalidResponseTest() throws Exception {
    when(xProductFeign.getItemPickupPointCodeByItemSkus(any())).thenReturn(
        new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKU(GDN_SKU, IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(ClientException.class,
          () -> this.productService.getInventorySummary(GDN_SKU, IS_WARE_HOUSE,
              BUSINESSPARTNER_CODE));
    } finally {
      verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
    }
  }

  @Test
  void getInventorySummaryInvalidResponseContentEmptyTest() throws Exception {
    when(xProductFeign.getItemPickupPointCodeByItemSkus(any())).thenReturn(
        new GdnRestListResponse<>(new ArrayList<>(),
            new PageMetaData(), REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKU(GDN_SKU, IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.getInventorySummary(GDN_SKU, IS_WARE_HOUSE,
              BUSINESSPARTNER_CODE));
    } finally {
      verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
    }
  }

  @Test
  void getInventorySummaryWrongPPCodesTest() throws Exception {
    when(xProductFeign.getItemPickupPointCodeByItemSkus(any())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new ItemSkuPickupPointCodeResponse(PRODUCT_SKU, PICKUP_POINT_CODE)),
            new PageMetaData(), REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKU(GDN_SKU, IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.getInventorySummary(GDN_SKU, IS_WARE_HOUSE, BUSINESSPARTNER_CODE));
    } finally {
      verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
    }
  }

  @Test
  void getInventorySummaryTwoPPCodesTest() throws Exception {
    when(xProductFeign.getItemPickupPointCodeByItemSkus(any())).thenReturn(new GdnRestListResponse<>(
        Arrays.asList(new ItemSkuPickupPointCodeResponse(GDN_SKU, PICKUP_POINT_CODE),
            new ItemSkuPickupPointCodeResponse(GDN_SKU, PICKUP_POINT_CODE)), new PageMetaData(), REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKU(GDN_SKU, IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.getInventorySummary(GDN_SKU, IS_WARE_HOUSE,
              BUSINESSPARTNER_CODE));
    } finally {
      verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
    }
  }

  @Test
  public void getInventorySummaryTest_EmptySummary() throws Exception {
    when(this.xInventoryFeign.reservedStockSummaryByWebSKU(GDN_SKU, IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(), pageMetaData, REQUEST_ID));
    this.productService.getInventorySummary(GDN_SKU, IS_WARE_HOUSE, BUSINESSPARTNER_CODE);
    verify(xInventoryFeign).reservedStockSummaryByWebSKU(GDN_SKU, IS_WARE_HOUSE);
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
    verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
  }

  @Test
  public void getInventorySummaryTest_webStock() throws Exception {
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKU(GDN_SKU, !IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    this.productService.getInventorySummary(GDN_SKU, !IS_WARE_HOUSE, BUSINESSPARTNER_CODE);
    verify(xInventoryFeign).reservedStockSummaryByWebSKU(GDN_SKU, !IS_WARE_HOUSE);
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
    verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
  }

  @Test
  public void getInventorySummaryTest_webStockEmpty() throws Exception {
    inventoryDetailInfoResponseDTO.setWebInventoryResponse(null);
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKU(GDN_SKU, !IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    this.productService.getInventorySummary(GDN_SKU, !IS_WARE_HOUSE, BUSINESSPARTNER_CODE);
    verify(xInventoryFeign).reservedStockSummaryByWebSKU(GDN_SKU, !IS_WARE_HOUSE);
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
    verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
  }

  @Test
  public void getInventorySummary_EmptyResponse() throws Exception {
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(null);
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKU(GDN_SKU, IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(new ReservedStockSummaryResponse(), REQUEST_ID));
    this.productService.getInventorySummary(GDN_SKU, IS_WARE_HOUSE, BUSINESSPARTNER_CODE);
    verify(xInventoryFeign).reservedStockSummaryByWebSKU(GDN_SKU, IS_WARE_HOUSE);
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
    verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
  }

  @Test
  public void getL3ProductCounts_forActiveProductTest() {
    ProductCountResponse productCountResponse = new ProductCountResponse(10L, 30L, null, null);
    when(this.xProductFeign.getProductCountByType(ACTIVE, MERCHANT_CODE))
        .thenReturn(new GdnRestSingleResponse(productCountResponse, REQUEST_ID));
    when(this.pbpFeign.getInProgressProductCount(MERCHANT_CODE))
        .thenReturn(countProductLevel3WipResponseGdnRestSingleResponse);
    ProductL3CountWebResponse response = this.productService.getL3ProductCounts(ACTIVE, MERCHANT_CODE);
    verify(this.xProductFeign).getProductCountByType(ACTIVE, MERCHANT_CODE);
    verify(this.pbpFeign).getInProgressProductCount(MERCHANT_CODE);
    assertNotNull(response);
    assertEquals(10L, response.getActive().longValue());
    assertEquals(30L, response.getOutOfStock().longValue());
    assertEquals(20L, response.getInReview().longValue());
    assertEquals(30L, response.getNeedCorrection().longValue());
  }

  @Test
  public void getL3ProductCounts_forInActiveProductTest() {
    ProductCountResponse productCountResponse = new ProductCountResponse(10L, 30L, 30L, 10L);
    when(this.pbpFeign.getInActiveProductCount(MERCHANT_CODE))
        .thenReturn(countProductLevel3InactiveResponseGdnRestSingleResponse);
    when(this.xProductFeign.getProductCountByType(INACTIVE, MERCHANT_CODE))
        .thenReturn(new GdnRestSingleResponse(productCountResponse, REQUEST_ID));
    when(this.pbpFeign.getInProgressProductCount(MERCHANT_CODE))
        .thenReturn(countProductLevel3WipResponseGdnRestSingleResponse);
    ProductL3CountWebResponse response = this.productService.getL3ProductCounts(INACTIVE, MERCHANT_CODE);
    verify(this.pbpFeign).getInActiveProductCount(MERCHANT_CODE);
    verify(this.xProductFeign).getProductCountByType(INACTIVE, MERCHANT_CODE);
    verify(this.pbpFeign).getInProgressProductCount(MERCHANT_CODE);
    assertNotNull(response);
    assertEquals(10L, response.getActive().longValue());
    assertEquals(30L, response.getOutOfStock().longValue());
    assertEquals(20L, response.getInReview().longValue());
    assertEquals(30L, response.getNeedCorrection().longValue());
    assertEquals(10L, response.getArchived().longValue());
    assertEquals(20L, response.getRejected().longValue());
    assertEquals(30L, response.getSuspended().longValue());
  }

  @Test
  public void getL3ProductCountsEmptyTypeExceptionTest() {
    try {
      this.productService.getL3ProductCounts(StringUtils.EMPTY, MERCHANT_CODE);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
    }
  }

  @Test
  public void downloadTemplateForMultiPickupPointTemplateTest() throws Exception {
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
      .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
      .thenReturn(pickupPointResponseList);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Collections.emptySet());
    productService.downloadTemplateForMultiPickupPointTemplate(BUSINESSPARTNER_CODE,httpServletResponse);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
      Collections.emptySet());
  }

  @Test
  public void downloadTemplateForMultiPickupPointTemplate_restrictAccessOffTest() throws Exception
  {
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
      .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
      .thenReturn(pickupPointResponseList);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(false);
    productService.downloadTemplateForMultiPickupPointTemplate(BUSINESSPARTNER_CODE,httpServletResponse);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    //verifyZeroInteractions(this.mandatoryParameterHelper);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
      Collections.emptySet());
  }

  @Test
  public void downloadTemplateForMultiPickupPointTemplate_NonCncSeller_Test() throws Exception {
    profileResponse.getCompany().setCncActivated(true);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
      .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
      .thenReturn(pickupPointResponseList);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Collections.emptySet());
    productService.downloadTemplateForMultiPickupPointTemplate(BUSINESSPARTNER_CODE, httpServletResponse);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
      Collections.emptySet());
  }

  @Test
  public void downloadTemplateForMultiPickupPointTemplate_NonCncSeller_cncForWarehouseTrue_Test()
      throws Exception {
    ReflectionTestUtils.setField(productService, "cncForWarehouseFeatureSwitch", true);
    profileResponse.getCompany().setCncActivated(true);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
        BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Collections.emptySet());
    productService.downloadTemplateForMultiPickupPointTemplate(BUSINESSPARTNER_CODE,
        httpServletResponse);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
        Collections.emptySet());
  }

  @Test
  public void downloadTemplateForMultiPickupPointTemplateInternationalMerchantTest() throws Exception
  {
    profileResponse.getCompany().setInternationalFlag(true);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
      .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
      .thenReturn(pickupPointResponseList);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Collections.emptySet());
    productService.downloadTemplateForMultiPickupPointTemplate(BUSINESSPARTNER_CODE,httpServletResponse);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
      Collections.emptySet());
  }


  @Test
  public void downloadTemplateForMultiPickupPointTemplateInternationalMerchant_NonCncSeller_Test() throws Exception
  {
    profileResponse.getCompany().setInternationalFlag(true);
    profileResponse.getCompany().setCncActivated(true);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
      .thenReturn(profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE))
      .thenReturn(pickupPointResponseList);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Collections.emptySet());
    productService.downloadTemplateForMultiPickupPointTemplate(BUSINESSPARTNER_CODE,httpServletResponse);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
      Collections.emptySet());
  }

  @Test
  public void downloadTemplateForMultiPickupPointTemplateInternationalMerchant_NonCncSeller_cncForWarehouseTrue_Test()
      throws Exception {
    ReflectionTestUtils.setField(productService, "cncForWarehouseFeatureSwitch", true);
    profileResponse.getCompany().setInternationalFlag(true);
    profileResponse.getCompany().setCncActivated(true);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
        BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Collections.emptySet());
    productService.downloadTemplateForMultiPickupPointTemplate(BUSINESSPARTNER_CODE,
        httpServletResponse);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
        Collections.emptySet());
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_newL5FetchTest() throws Exception {
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_validateUserPIC() throws Exception {
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(
          new HashSet<>());
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          new HashSet<>())).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.userPicService).fetchAccessiblePickupPointCodes(profileResponse);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          new HashSet<>());
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_newL5Fetch_synchronizeStockNullTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setSynchronizeStock(null);
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSkuWithPPFbbTrue() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setSynchronizeStock(null);
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_newL5Fetch_synchronizeStockFalseTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setSynchronizeStock(false);
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_newL5Fetch_multiplePageTest()
    throws Exception {
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.setPageMetaData(new PageMetaData(1, 0, 2));
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(1), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(1), eq(1), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 1);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(1), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(1), eq(1), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_cncActiveSellerTest() throws Exception {
    profileResponse.getCompany().setCncActivated(true);
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_multiDefaultAddressTest() throws Exception {
    profileResponse.setMultiDefaultAddressFlag(true);
    profileResponse.getCompany().setMerchantType(Constants.MERCHANT_TYPE_CM);
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_multiDefaultAddress_nonCMTest() throws Exception {
    profileResponse.setMultiDefaultAddressFlag(true);
    profileResponse.getCompany().setMerchantType(Constants.MERCHANT_TYPE_CC);
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
    productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
    when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, null))
        .thenReturn(pickupPointResponseList);
    when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
      any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
      productLevel3SummaryResponseGdnRestListResponse);
    this.productService
      .downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
        Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, null);
    verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
      any(ProductSkuAndPickupPointCodeRequest.class));
    verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_cncActiveProductTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setCncActive(true);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setB2BResponse(null);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse.getCompany().setCncActivated(true);
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_cncActiveProductB2bNonNullTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setCncActive(true);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setB2BResponse(new B2BResponse());
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_cncActiveProductB2bPriceNonNullTest() throws Exception {
    b2BResponse.setBasePrice(PRICE);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setCncActive(true);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setB2BResponse(b2BResponse);
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_cncActiveProductB2bPriceNonNullManagedTrueTest() throws Exception {
    b2BResponse.setBasePrice(PRICE);
    b2BResponse.setManaged(true);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setCncActive(true);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setB2BResponse(b2BResponse);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0)
        .setViewConfigs(Arrays.asList(viewConfigResponseB2b));
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(null);
      when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
          null);
      verify(this.pbpFeign).getL5SummaryByProductSkuList(eq(0), eq(50), eq(BUSINESSPARTNER_CODE),
          any(ProductSkuAndPickupPointCodeRequest.class));
      verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_emptyProductListTest() throws Exception {
    new GdnRestListResponse<>(Collections.EMPTY_LIST, pageMetaData, REQUEST_ID);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  void downloadTemplateBulkUpdateByProductSku_nullProfileResponseTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE,
              false, Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50));
    } finally {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  void downloadTemplateMultiPickupPoint_nullProfileResponseTest() throws Exception {
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productService.downloadTemplateForMultiPickupPointTemplate(BUSINESSPARTNER_CODE,
              httpServletResponse));
    } finally {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  void downloadTemplateMultiPickupPoint_inactiveMerchantTest() throws Exception {
    profileResponse.setMerchantStatus(null);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productService.downloadTemplateForMultiPickupPointTemplate(BUSINESSPARTNER_CODE,
              httpServletResponse));
    } finally {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  void downloadTemplateBulkUpdateByProductSku_inactiveMerchantTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    profileResponse.setMerchantStatus(null);
    productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE,
              false, Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50));
    } finally {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_amphiB2BTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setBuyable(Boolean.TRUE);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setDisplay(Boolean.FALSE);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_amphiTeaserTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setBuyable(Boolean.FALSE);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setDisplay(Boolean.TRUE);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_amphiOnlineTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setBuyable(Boolean.TRUE);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setDisplay(Boolean.TRUE);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_amphiOfflineTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setBuyable(Boolean.FALSE);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setDisplay(Boolean.FALSE);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_externalOfflineTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setBuyable(Boolean.FALSE);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setDisplay(Boolean.FALSE);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_externalOnlineTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setBuyable(Boolean.TRUE);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setDisplay(Boolean.TRUE);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_externalB2BTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setBuyable(Boolean.TRUE);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).getViewConfigs().get(0)
        .setDisplay(Boolean.FALSE);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_noViewConfigAccessibility() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities_withoutViewConfigEdit();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_noO2OAccessibility() throws Exception {
    this.profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_RC);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = accessibilities_withoutO2OEdit();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSku_2PageTest() throws Exception {
    productLevel3SummaryResponseGdnRestListResponse = new GdnRestListResponse<>(
        Arrays.asList(productLevel3SummaryResponse, productLevel3SummaryResponse), pageMetaData,
        REQUEST_ID);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(2);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), anyInt(), eq(1),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, false,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 1);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign, times(2)).filterSummary(eq(BUSINESSPARTNER_CODE), anyInt(), eq(1),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSkuExternalUserMerchantTypeCC() throws Exception {
    this.profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_CC);
    productLevel3SummaryResponseGdnRestListResponse = new GdnRestListResponse<>(
        Arrays.asList(productLevel3SummaryResponse, productLevel3SummaryResponse), pageMetaData,
        REQUEST_ID);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(2);
    String arr[] = accessibilities();
    productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), anyInt(), eq(1),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 1);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign, times(2)).filterSummary(eq(BUSINESSPARTNER_CODE), anyInt(), eq(1),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSkuExternalUserMerchantTypeTC() throws Exception {
    this.profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_TC);
    productLevel3SummaryResponseGdnRestListResponse = new GdnRestListResponse<>(
        Arrays.asList(productLevel3SummaryResponse, productLevel3SummaryResponse), pageMetaData,
        REQUEST_ID);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(2);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), anyInt(), eq(1),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 1);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign, times(2)).filterSummary(eq(BUSINESSPARTNER_CODE), anyInt(), eq(1),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSkuExternalUserMerchantTypeTD() throws Exception {
    this.profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_TD);
    productLevel3SummaryResponseGdnRestListResponse = new GdnRestListResponse<>(
        Arrays.asList(productLevel3SummaryResponse, productLevel3SummaryResponse), pageMetaData,
        REQUEST_ID);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(2);
    productLevel3SummaryResponseGdnRestListResponse.getContent().get(0).setOff2OnActiveFlag(false);
    String arr[] = accessibilities();
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), anyInt(), eq(1),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 1);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign, times(2)).filterSummary(eq(BUSINESSPARTNER_CODE), anyInt(), eq(1),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSkuBPMerchant() throws Exception {
    this.profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_CC);
    this.profileResponse.getCompany().setInventoryFulfillment(Constants.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    this.profileResponse.getCompany().setOfflineToOnlineFlag(false);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String[] arr = {Accessibilty.MAINTAIN_PRODUK_SKU_DISPLAY_BUYABLE_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA, Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_PICKUP_POINT_CHANGE_DATA, Accessibilty.FUNCTION_MAINTAIN_PRODUK_SKU_SYNC_STOCK,
        Accessibilty.IS_PRIVILEGED_TO_EDIT_PRICE, Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK,
        Accessibilty.MAINTAIN_PRODUK_SKU_O2O_CHANGE_DATA};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void downloadTemplateBulkUpdateByProductSkuBPMerchantNotPrivilidge() throws Exception {
    this.profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_CC);
    this.profileResponse.getCompany().setInventoryFulfillment(Constants.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    productLevel3SummaryResponseGdnRestListResponse.getPageMetaData().setTotalRecords(1);
    String arr[] = new String[1];
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(arr);
      productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      when(this.businessPartnerService.getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE)).thenReturn(pickupPointResponseList);
      when(this.pbpFeign.filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY),
          any(ProductLevel3SummaryRequest.class))).thenReturn(
          productLevel3SummaryResponseGdnRestListResponse);
      this.productService.downloadTemplateBulkUpdateByProductSku(BUSINESSPARTNER_CODE, true,
          Arrays.asList(PRODUCT_SKU), httpServletResponse, 0, 50);
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getAllPickupPointsForBusinessPartner(
          BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).filterSummary(eq(BUSINESSPARTNER_CODE), eq(0), eq(50),
          eq(StringUtils.EMPTY), eq(StringUtils.EMPTY), any(ProductLevel3SummaryRequest.class));
    }
  }

  @Test
  public void updateItemListingTest() throws Exception {
    QuickEditWebRequest quickEditWebRequest = new QuickEditWebRequest();
    quickEditWebRequest.setItemSku(ITEM_SKU);
    quickEditWebRequest.setPrices(Arrays.asList(new ProductLevel3PriceWebRequest()));
    when(pbpFeign.itemListingUpdate(productLevel3QuickEditRequestArgumentCaptor.capture(), eq(PRODUCT_SKU)))
        .thenReturn(new GdnBaseRestResponse(true));
    productService.updateItemListing(PRODUCT_SKU, Arrays.asList(quickEditWebRequest));
    verify(pbpFeign).itemListingUpdate(productLevel3QuickEditRequestArgumentCaptor.getValue(), PRODUCT_SKU);
    Assertions.assertEquals(ITEM_SKU,
        productLevel3QuickEditRequestArgumentCaptor.getValue().getQuickEditRequests().get(0).getItemSku());
    Mockito.verify(mandatoryParameterHelper).isExternal();
  }

  @Test
  public void toggleArchiveProductsTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_ARCHIVE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      productSkus.add(PRODUCT_SKU_1);
      SimpleListStringRequest request = new SimpleListStringRequest();
      request.setValue(Arrays.asList(PRODUCT_SKU_1));
      when(this.pbpFeign.archiveProducts(DO_ARCHIVE, request)).thenReturn(
          new GdnRestSingleResponse<>(new ItemBulkArchiveResponse(), REQUEST_ID));
      this.productService.toggleArchiveProducts(productSkus, DO_ARCHIVE, BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).archiveProducts(DO_ARCHIVE, request);
    }
  }

  @Test
  public void toggleArchiveProductsForEmptyProductSkuTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_ARCHIVE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      productSkus.add(StringUtils.EMPTY);
      this.productService.toggleArchiveProducts(productSkus, DO_ARCHIVE, BUSINESSPARTNER_CODE);
      verify(this.pbpFeign, times(0)).archiveProducts(Mockito.eq(DO_ARCHIVE),
          any(SimpleListStringRequest.class));
    }
  }

  @Test
  public void toggleArchiveProductsFailedTest() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_ARCHIVE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      productSkus.add(PRODUCT_SKU_1);
      SimpleListStringRequest request = new SimpleListStringRequest();
      request.setValue(Arrays.asList(PRODUCT_SKU_1));
      when(this.pbpFeign.archiveProducts(DO_ARCHIVE, request)).thenReturn(
          new GdnRestSingleResponse<>(new ItemBulkArchiveResponse(Arrays.asList(PRODUCT_SKU_1)),
              REQUEST_ID));
      try {
        Assertions.assertThrows(ApplicationRuntimeException.class,
            () -> this.productService.toggleArchiveProducts(productSkus, DO_ARCHIVE,
                BUSINESSPARTNER_CODE));
      } finally {
        verify(this.pbpFeign).archiveProducts(DO_ARCHIVE, request);
      }
    }
  }

  @Test
  public void toggleArchiveProducts1Test() {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_ARCHIVE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      productSkus.add(PRODUCT_SKU_1);
      SimpleListStringRequest request = new SimpleListStringRequest();
      request.setValue(Arrays.asList(PRODUCT_SKU_1));
      when(this.pbpFeign.archiveProducts(DO_ARCHIVE, request)).thenReturn(
          new GdnRestSingleResponse<>(new ItemBulkArchiveResponse(Arrays.asList(PRODUCT_SKU)),
              REQUEST_ID));
      this.productService.toggleArchiveProducts(productSkus, DO_ARCHIVE, BUSINESSPARTNER_CODE);
      verify(this.pbpFeign).archiveProducts(DO_ARCHIVE, request);
    }
  }

  @Test
  public void getProductL3List() {
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setL5Count(1);
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);
    when(xInventoryFeign.findDetailByWebProductSkus( false,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>())))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryStockInfoDTO), pageMetaData, REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(xCampaignFeign.getCampaignPriceInfo(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    when(xProductFeign.getItemPickupPointCodeByItemSkus(any())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new ItemSkuPickupPointCodeResponse(ITEM_SKU, PICKUP_POINT_CODE)),
            new PageMetaData(), REQUEST_ID));
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
    verify(xCampaignFeign).getCampaignPriceInfo(campaignPriceRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
  }

  @Test
  public void getProductL3NullSchedulesList() {
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setL5Count(1);
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).getItemL4SummaryResponse()
        .getItemViewConfigs().forEach(viewConfig -> viewConfig.setItemBuyableSchedules(null));
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).getItemL4SummaryResponse()
        .getItemViewConfigs().forEach(viewConfig -> viewConfig.setItemDiscoverableSchedules(null));
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);
    when(xInventoryFeign.findDetailByWebProductSkus( false,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>())))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryStockInfoDTO), pageMetaData, REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(xCampaignFeign.getCampaignPriceInfo(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    when(xProductFeign.getItemPickupPointCodeByItemSkus(any())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new ItemSkuPickupPointCodeResponse(ITEM_SKU, PICKUP_POINT_CODE)),
            new PageMetaData(), REQUEST_ID));
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
    verify(xCampaignFeign).getCampaignPriceInfo(campaignPriceRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
  }

  @Test
  public void getProductL3List_BothInventoryResponseNull() {
    productSystemParameterResponse1.setValue("false");
    when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse1, REQUEST_ID));
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setVariantCount(2);
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);
    when(xInventoryFeign.findDetailByWebProductSkus( false,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>())))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryStockInfoDTO), pageMetaData, REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(xCampaignFeign.getCampaignPriceInfo(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
  }

  @Test
  public void getProductL3List_L3Stock() {
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setVariantCount(2);
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);
    when(xInventoryFeign.findDetailByWebProductSkus( true,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>())))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryStockInfoDTO), pageMetaData, REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(xCampaignFeign.getCampaignPriceInfo(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(xInventoryFeign).findDetailByWebProductSkus( true,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>()));
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
  }

  @Test
  public void getProductL3List_Variant1ItemNull() {
    productSystemParameterResponse1.setValue("false");
    when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse1, REQUEST_ID));
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setItemL4SummaryResponse(null);
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);
    when(xInventoryFeign.findDetailByWebProductSkus( false,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>())))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryStockInfoDTO), pageMetaData, REQUEST_ID));
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
  }

  @Test
  public void getProductL3List_NullCases() {
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setL5Count(1);
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    campaignPriceResponse.setItemSkuToPriceResponse(new HashMap<>());
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);
    when(xInventoryFeign.findDetailByWebProductSkus( false,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>())))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryStockInfoDTO), pageMetaData, REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())))).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new InventoryDetailInfoResponseDTO()), pageMetaData, REQUEST_ID));
    when(xCampaignFeign.getCampaignPriceInfo(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    when(xProductFeign.getItemPickupPointCodeByItemSkus(any())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new ItemSkuPickupPointCodeResponse(ITEM_SKU, PICKUP_POINT_CODE)),
            new PageMetaData(), REQUEST_ID));
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
    verify(xCampaignFeign).getCampaignPriceInfo(campaignPriceRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
  }

  @Test
  public void getProductL3List_NullCases2() {
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setL5Count(1);
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(null);
    campaignPriceResponse.getItemSkuToPriceResponse().get(ITEM_SKU).setLockPriceUpdate(true);
    campaignPriceResponse.getItemSkuToPriceResponse().get(ITEM_SKU).setLive(false);
    campaignPriceResponse.getItemSkuToPriceResponse().get(ITEM_SKU).setCampaignPrice(null);
    priceDTO.setListOfDiscountPrices(new ArrayList<>());
    productL3SummaryResponse.getItemL4SummaryResponse().setPrice(new HashSet<>(Collections.singletonList(priceDTO)));
    productL3SummaryResponse.getItemL4SummaryResponse()
        .setActivePromoBundlings(new HashSet<>(Collections.singletonList(Constants.WHOLESALE_PRICE)));
    productL3SummaryResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(productL3SummaryResponse), pageMetaData, REQUEST_ID);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);
    when(xInventoryFeign.findDetailByWebProductSkus( false,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>())))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryStockInfoDTO), pageMetaData, REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(xCampaignFeign.getCampaignPriceInfo(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    when(xProductFeign.getItemPickupPointCodeByItemSkus(any())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new ItemSkuPickupPointCodeResponse(ITEM_SKU, PICKUP_POINT_CODE)),
            new PageMetaData(), REQUEST_ID));
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
    verify(xCampaignFeign).getCampaignPriceInfo(campaignPriceRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
  }

  @Test
  public void getProductL3List_withNoItemResponse() {
    productL3SummaryResponse.setVariantCount(2);
    productL3SummaryResponse.setProductScore(null);
    productL3SummaryResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(productL3SummaryResponse), pageMetaData, REQUEST_ID);
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(String.valueOf(true));
    when(xInventoryFeign.findDetailByWebProductSkus( false,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>())))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryStockInfoDTO), pageMetaData, REQUEST_ID));
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(xInventoryFeign).findDetailByWebProductSkus( false,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>()));
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
  }

  @Test
  public void getProductL3List_EmptyList() {
    productL3SummaryResponse.setVariantCount(2);
    productL3SummaryResponseGdnRestListResponse = new GdnRestListResponse<>(Arrays.asList(), pageMetaData, REQUEST_ID);
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
  }

  @Test
  public void getProductL3List_nullValues() {
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setL5Count(1);
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(new ArrayList<>());
    inventoryDetailInfoResponseDTO.setWebInventoryResponse(null);
    campaignPriceResponse.getItemSkuToPriceResponse().get(ITEM_SKU).setLockPriceUpdate(false);
    campaignPriceResponse.getItemSkuToPriceResponse().get(ITEM_SKU).setLive(false);
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    productL3SummaryResponse.getItemL4SummaryResponse().setActivePromoBundlings(new HashSet<>());
    productL3SummaryResponse.getItemL4SummaryResponse().setMasterDataItemImages(null);
    productL3SummaryResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(productL3SummaryResponse), pageMetaData, REQUEST_ID);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);
    when(xInventoryFeign.findDetailByWebProductSkus( false,
        new InventoryDetailStockInfoRequestDTO(productSummaryRequest.getMerchantCode(),
            Arrays.asList(PRODUCT_SKU_NEW), new ArrayList<>())))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryStockInfoDTO), pageMetaData, REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(xCampaignFeign.getCampaignPriceInfo(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    when(xProductFeign.getItemPickupPointCodeByItemSkus(any())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new ItemSkuPickupPointCodeResponse(ITEM_SKU, PICKUP_POINT_CODE)),
            new PageMetaData(), REQUEST_ID));
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
    verify(xCampaignFeign).getCampaignPriceInfo(campaignPriceRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(xProductFeign).getItemPickupPointCodeByItemSkus(any(SimpleListStringRequest.class));
  }

  @Test
  public void getProductL3List_Suspension() {
    productSummaryRequest.setSuspended(true);
    ProductSummaryWebRequest productSummaryWebRequest = new ProductSummaryWebRequest();
    BeanUtils.copyProperties(productSummaryRequest, productSummaryWebRequest);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    when(pbpFeign.fetchProductSuspensionHistory(new ProductSkuListRequest(Arrays.asList(PRODUCT_SKU_NEW))))
        .thenReturn(productSuspensionHistoryResponseGdnRestListResponse);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);
    this.productService.getProductL3List(productSummaryWebRequest, PAGE, SIZE);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(pbpFeign).fetchProductSuspensionHistory(new ProductSkuListRequest(Arrays.asList(PRODUCT_SKU_NEW)));
  }

  @Test
  public void getItemsByProductSkuTest() {
    when(this.xCampaignFeign.getCampaignAvailability(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.pbpFeign
        .filterSummary(eq(BUSINESSPARTNER_CODE), eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
            eq(ORDERBY), eq(SORTBY), any(ProductLevel3SummaryRequest.class)))
        .thenReturn(productLevel3SummaryResponseGdnRestListResponse);
    when(xCampaignFeign.getCampaignPriceInfo(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    Page<ProductLevel3SummaryWebResponse> result =
        this.productService.getItemsByProductSku(PRODUCT_SKU_1, BUSINESSPARTNER_CODE, pageRequest);
    verify(this.pbpFeign)
        .filterSummary(eq(BUSINESSPARTNER_CODE), eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
            eq(ORDERBY), eq(SORTBY), productLevel3SummaryRequestArgumentCaptor.capture());
    verify(this.xCampaignFeign).getCampaignAvailability(productCampaignAvailabilityRequest);
    verify(xCampaignFeign).getCampaignPriceInfo(campaignPriceRequest1);
    assertEquals(PRODUCT_SKU_1, productLevel3SummaryRequestArgumentCaptor.getValue().getProductSkuList().get(0));
    assertNotNull(result);
    assertTrue(result.getContent().get(0).isItemCampaignMapped());
    assertEquals(90.0, result.getContent().get(0).getProductScore(), 0);
  }

  @Test
  public void getItemsByProductSkuMppTest() {
    ReflectionTestUtils.setField(productService, "multiPickupPointEnabled", true);
    when(this.xCampaignFeign.getCampaignAvailabilityV2(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.pbpFeign
        .filterSummary(eq(BUSINESSPARTNER_CODE), eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
            eq(ORDERBY), eq(SORTBY), any(ProductLevel3SummaryRequest.class)))
        .thenReturn(productLevel3SummaryResponseGdnRestListResponse);
    when(xCampaignFeign.getCampaignPriceInfoV2(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    Page<ProductLevel3SummaryWebResponse> result =
        this.productService.getItemsByProductSku(PRODUCT_SKU_1, BUSINESSPARTNER_CODE, pageRequest);
    verify(this.pbpFeign)
        .filterSummary(eq(BUSINESSPARTNER_CODE), eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
            eq(ORDERBY), eq(SORTBY), productLevel3SummaryRequestArgumentCaptor.capture());
    verify(this.xCampaignFeign).getCampaignAvailabilityV2(productCampaignAvailabilityRequest);
    verify(xCampaignFeign).getCampaignPriceInfoV2(campaignPriceRequest1);
    assertEquals(PRODUCT_SKU_1, productLevel3SummaryRequestArgumentCaptor.getValue().getProductSkuList().get(0));
    assertNotNull(result);
    assertTrue(result.getContent().get(0).isItemCampaignMapped());
    assertEquals(90.0, result.getContent().get(0).getProductScore(), 0);
  }

  @Test
  public void getItemsByProductSkuPricingMppTest() {
    ReflectionTestUtils.setField(productService, "pricingMultiPickupPointEnabled", true);
    when(this.xCampaignFeign.getCampaignAvailabilityV2(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.pbpFeign
        .filterSummary(eq(BUSINESSPARTNER_CODE), eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
            eq(ORDERBY), eq(SORTBY), any(ProductLevel3SummaryRequest.class)))
        .thenReturn(productLevel3SummaryResponseGdnRestListResponse);
    when(xCampaignFeign.getCampaignPriceInfoV2(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    Page<ProductLevel3SummaryWebResponse> result =
        this.productService.getItemsByProductSku(PRODUCT_SKU_1, BUSINESSPARTNER_CODE, pageRequest);
    verify(this.pbpFeign)
        .filterSummary(eq(BUSINESSPARTNER_CODE), eq(pageRequest.getPageNumber()), eq(pageRequest.getPageSize()),
            eq(ORDERBY), eq(SORTBY), productLevel3SummaryRequestArgumentCaptor.capture());
    verify(this.xCampaignFeign).getCampaignAvailabilityV2(productCampaignAvailabilityRequest);
    verify(xCampaignFeign).getCampaignPriceInfoV2(campaignPriceRequest1);
    assertEquals(PRODUCT_SKU_1, productLevel3SummaryRequestArgumentCaptor.getValue().getProductSkuList().get(0));
    assertNotNull(result);
    assertTrue(result.getContent().get(0).isItemCampaignMapped());
    assertEquals(90.0, result.getContent().get(0).getProductScore(), 0);
  }

  @Test
  public void getItemsByProductSkuListEmptyTest() {
    when(this.pbpFeign
        .filterSummary(eq(activeProductWebRequest2.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(ORDERBY), eq(SORTBY), any(ProductLevel3SummaryRequest.class)))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID));
    Page<ProductLevel3SummaryWebResponse> result =
        this.productService.getItemsByProductSku(PRODUCT_SKU_1, BUSINESSPARTNER_CODE, pageRequest);
    verify(this.pbpFeign)
        .filterSummary(eq(activeProductWebRequest2.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(ORDERBY), eq(SORTBY),
            productLevel3SummaryRequestArgumentCaptor.capture());
    assertEquals(0, result.getContent().size());
    assertNotNull(result);
  }

  @Test
  public void getItemsByProductSkuTest_emptyTest() {
    campaignPriceResponse.getItemSkuToPriceResponse().get(ITEM_SKU).setCampaignPrice(null);
    campaignPriceResponse.getItemSkuToPriceResponse().get(ITEM_SKU).setLockPriceUpdate(false);
    when(this.xCampaignFeign.getCampaignAvailability(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.pbpFeign
        .filterSummary(eq(activeProductWebRequest2.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(ORDERBY), eq(SORTBY), any(ProductLevel3SummaryRequest.class)))
        .thenReturn(productLevel3SummaryResponseGdnRestListResponse);
    when(xCampaignFeign.getCampaignPriceInfo(any()))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, REQUEST_ID));
    Page<ProductLevel3SummaryWebResponse> result =
        this.productService.getItemsByProductSku(PRODUCT_SKU_1, BUSINESSPARTNER_CODE, pageRequest);
    verify(this.pbpFeign)
        .filterSummary(eq(activeProductWebRequest2.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(ORDERBY), eq(SORTBY),
            productLevel3SummaryRequestArgumentCaptor.capture());
    verify(this.xCampaignFeign).getCampaignAvailability(productCampaignAvailabilityRequest);
    verify(xCampaignFeign).getCampaignPriceInfo(campaignPriceRequest1);
    assertNotNull(result);
    assertTrue(result.getContent().get(0).isItemCampaignMapped());
    assertEquals(90.0, result.getContent().get(0).getProductScore(), 0);
  }

  @Test
  public void getItemsByProductSkuTest_CampNullResponse() {
    when(this.xCampaignFeign.getCampaignAvailability(productCampaignAvailabilityRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productCampaignAvailabilityResponse, REQUEST_ID));
    when(this.pbpFeign
        .filterSummary(eq(activeProductWebRequest2.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(ORDERBY), eq(SORTBY), any(ProductLevel3SummaryRequest.class)))
        .thenReturn(productLevel3SummaryResponseGdnRestListResponse);
    when(xCampaignFeign.getCampaignPriceInfo(any()))
        .thenReturn(new GdnRestSingleResponse<>(new CampaignPriceResponse(new HashMap<>(), null),
          REQUEST_ID));
    Page<ProductLevel3SummaryWebResponse> result =
        this.productService.getItemsByProductSku(PRODUCT_SKU_1, BUSINESSPARTNER_CODE, pageRequest);
    verify(this.pbpFeign)
        .filterSummary(eq(activeProductWebRequest2.getBusinessPartnerCode()), eq(pageRequest.getPageNumber()),
            eq(pageRequest.getPageSize()), eq(ORDERBY), eq(SORTBY),
            productLevel3SummaryRequestArgumentCaptor.capture());
    verify(this.xCampaignFeign).getCampaignAvailability(productCampaignAvailabilityRequest);
    verify(xCampaignFeign).getCampaignPriceInfo(campaignPriceRequest1);
    assertNotNull(result);
    assertTrue(result.getContent().get(0).isItemCampaignMapped());
    assertEquals(90.0, result.getContent().get(0).getProductScore(), 0);
  }

  @Test
  public void getVendorNotesTest() throws Exception {
    VendorNotesResponse vendorNotesResponse = new VendorNotesResponse();
    vendorNotesResponse.setMerchantModifiedFields(List.of(MERCHANT_MODIFIED_FIELD));
    when(this.pbpFeign.getVendorNotes(PRODUCT_CODE))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, vendorNotesResponse));
    productService.getVendorNotes(PRODUCT_CODE);
    verify(pbpFeign).getVendorNotes(PRODUCT_CODE);
  }

  @Test
  public void getVendorNotesEmptyMerchantModifiedFieldsTest() throws Exception {
    VendorNotesResponse vendorNotesResponse = new VendorNotesResponse();
    vendorNotesResponse.setMerchantModifiedFields(null);
    when(this.pbpFeign.getVendorNotes(PRODUCT_CODE))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, vendorNotesResponse));
    productService.getVendorNotes(PRODUCT_CODE);
    verify(pbpFeign).getVendorNotes(PRODUCT_CODE);
  }

  @Test
  public void updateVendorNotesTest() throws Exception {
    when(this.pbpFeign.updateVendorNotes(eq(PRODUCT_CODE), any()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, new VendorNotesResponse()));
    productService.updateVendorNotes(PRODUCT_CODE, new VendorNotesRequest());
    verify(pbpFeign).updateVendorNotes(eq(PRODUCT_CODE), any());
  }

  @Test
  public void submitNeedForRevisionProduct() {
    when(this.pbpFeign.submitNeedRevisionProduct(any(NeedRevisionSubmitRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(new EditProductV2Response(), REQUEST_ID));
    productService.submitNeedForRevisionProduct(new NeedRevisionSubmitWebRequest());
    verify(pbpFeign).submitNeedRevisionProduct(any(NeedRevisionSubmitRequest.class));
  }

  @Test
  public void appealProductsInProgressTest_success() {
    when(this.pbpFeign.updateAppealInProgressProduct(any(AppealProductRequest.class))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new AppealProductResponse(), null));
    boolean response = productService.appealProductsInProgress(new AppealProductWebRequest(),
        BUSINESSPARTNER_CODE);
    verify(pbpFeign).updateAppealInProgressProduct(any(AppealProductRequest.class));
    Assertions.assertTrue(response);
  }

  @Test
  public void appealProductsInProgressTest_nullResponse() {
    when(this.pbpFeign.updateAppealInProgressProduct(any(AppealProductRequest.class))).thenReturn(
        null);
    try {
      boolean response = productService.appealProductsInProgress(new AppealProductWebRequest(),
          BUSINESSPARTNER_CODE);
    }
    catch (ClientException e){
      Assertions.assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, e.getMessage());
    }
    verify(pbpFeign).updateAppealInProgressProduct(any(AppealProductRequest.class));
  }

  @Test
  public void appealProductsInProgressTest_nullValue() {
    when(this.pbpFeign.updateAppealInProgressProduct(any(AppealProductRequest.class))).thenReturn(
        new GdnRestSingleResponse<>("error", null, true, null, null));
    try {
      boolean response = productService.appealProductsInProgress(new AppealProductWebRequest(),
          BUSINESSPARTNER_CODE);
    } catch (ClientException e) {
      Assertions.assertEquals("error", e.getMessage());
    }
    verify(pbpFeign).updateAppealInProgressProduct(any(AppealProductRequest.class));
  }

  @Test
  public void appealProductsInProgressTest_appealLimitCrossed() {
    when(this.pbpFeign.updateAppealInProgressProduct(any(AppealProductRequest.class))).thenReturn(
        new GdnRestSingleResponse<>("error", null, true, AppealProductResponse.builder()
            .errorCode(com.gdn.mta.product.enums.ApiErrorCode.APPEAL_LIMIT_CROSSED.getCode())
            .errorMessage(com.gdn.mta.product.enums.ApiErrorCode.APPEAL_LIMIT_CROSSED.getDesc())
            .build(), null));
    try {
      boolean response = productService.appealProductsInProgress(new AppealProductWebRequest(),
          BUSINESSPARTNER_CODE);
    } catch (ApplicationException e) {
      Assertions.assertEquals(com.gdn.mta.product.enums.ApiErrorCode.APPEAL_LIMIT_CROSSED.getCode(),
          e.getCode());
    }
    verify(pbpFeign).updateAppealInProgressProduct(any(AppealProductRequest.class));
  }

  @Test
  public void appealProductsInProgressTest_invalidState() {
    when(this.pbpFeign.updateAppealInProgressProduct(any(AppealProductRequest.class))).thenReturn(
        new GdnRestSingleResponse<>("error", null, true, AppealProductResponse.builder()
            .errorCode(com.gdn.mta.product.enums.ApiErrorCode.PRODUCT_IN_INVALID_STATE.getCode())
            .errorMessage(com.gdn.mta.product.enums.ApiErrorCode.PRODUCT_IN_INVALID_STATE.getDesc())
            .build(), null));
    try {
      boolean response = productService.appealProductsInProgress(new AppealProductWebRequest(),
          BUSINESSPARTNER_CODE);
    } catch (ApplicationException e) {
      Assertions.assertEquals(com.gdn.mta.product.enums.ApiErrorCode.PRODUCT_IN_INVALID_STATE.getCode(),
          e.getCode());
    }
    verify(pbpFeign).updateAppealInProgressProduct(any(AppealProductRequest.class));
  }

  @Test
  public void appealProductsInProgressTest_productNotFound() {
    when(this.pbpFeign.updateAppealInProgressProduct(any(AppealProductRequest.class))).thenReturn(
        new GdnRestSingleResponse<>("error", null, true, AppealProductResponse.builder()
            .errorCode(com.gdn.mta.product.enums.ApiErrorCode.PRODUCT_NOT_FOUND.getCode())
            .errorMessage(com.gdn.mta.product.enums.ApiErrorCode.PRODUCT_NOT_FOUND.getDesc())
            .build(), null));
    try {
      boolean response = productService.appealProductsInProgress(new AppealProductWebRequest(),
          BUSINESSPARTNER_CODE);
    } catch (ApplicationException e) {
      Assertions.assertEquals(com.gdn.mta.product.enums.ApiErrorCode.PRODUCT_NOT_FOUND.getCode(),
          e.getCode());
    }
    verify(pbpFeign).updateAppealInProgressProduct(any(AppealProductRequest.class));
  }

  @Test
  public void appealProductsInProgressTest_successFalse() {
    when(this.pbpFeign.updateAppealInProgressProduct(any(AppealProductRequest.class))).thenReturn(
        new GdnRestSingleResponse<>("error", null, false, AppealProductResponse.builder()
            .errorCode("errorCode")
            .errorMessage("errorMessage")
            .build(), null));
    try {
      boolean response = productService.appealProductsInProgress(new AppealProductWebRequest(),
          BUSINESSPARTNER_CODE);
    } catch (ClientException e) {
      Assertions.assertEquals("error", e.getMessage());
    }
    verify(pbpFeign).updateAppealInProgressProduct(any(AppealProductRequest.class));
  }

  @Test
  public void updateProductInfoTest() throws Exception {
    UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest =
        UpdateProductLevel3InfoRequest.builder().productType(1).build();
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pbpFeign.updateProductInfo(any(UpdateProductLevel3InfoRequest.class), any()))
        .thenReturn(editProductResponseGdnRestSingleResponse);
    productService.updateProductInfo(updateProductLevel3InfoRequest, BUSINESSPARTNER_CODE, true, PRODUCT_SKU);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    Mockito.verify(pbpFeign).updateProductInfo(any(UpdateProductLevel3InfoRequest.class), any());
  }

  @Test
  void updateProductInfoBusinessPartnerInvalidTest() throws Exception {
    UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest = UpdateProductLevel3InfoRequest.builder().build();
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productService.updateProductInfo(updateProductLevel3InfoRequest, null, true,
              PRODUCT_SKU));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(any());
    }
  }

  @Test
  void updateProductInfoBusinessPartnerInvalidStateTest() throws Exception {
    UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest = UpdateProductLevel3InfoRequest.builder().build();
    profileResponse.setMerchantStatus(INACTIVE);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productService.updateProductInfo(updateProductLevel3InfoRequest,
              BUSINESSPARTNER_CODE, true, PRODUCT_SKU));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(any());
    }
  }

  @Test
  void updateProductInfoInvalidYoutubeUrlTest() throws Exception {
    ReflectionTestUtils.setField(productService,"youtubeRegex",VIDEO_REGEX_WITH_SHORTS);
    UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest =
        UpdateProductLevel3InfoRequest.builder().videoUrl(YOUTUBE_INVALID_URL).build();
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.pbpFeign.findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH))
        .thenReturn(systemParameterResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productService.updateProductInfo(updateProductLevel3InfoRequest,
              BUSINESSPARTNER_CODE, true, PRODUCT_SKU));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(any());
      verify(this.pbpFeign).findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH);
    }
  }

  @Test
  public void updateProductInfoValidYoutubeUrlTest() throws Exception {
    ReflectionTestUtils.setField(productService,"youtubeRegex",VIDEO_REGEX_WITH_SHORTS);
    UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest =
        UpdateProductLevel3InfoRequest.builder().videoUrl(YOUTUBE_URL).build();
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pbpFeign.updateProductInfo(any(UpdateProductLevel3InfoRequest.class), any()))
        .thenReturn(editProductResponseGdnRestSingleResponse);
    when(this.pbpFeign.findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH))
        .thenReturn(systemParameterResponse);
    when(youTube.videos()).thenReturn(videos);
    when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    when(list.execute()).thenReturn(videoListResponse);
    productService.updateProductInfo(updateProductLevel3InfoRequest, BUSINESSPARTNER_CODE, true, PRODUCT_SKU);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    Mockito.verify(pbpFeign).updateProductInfo(any(UpdateProductLevel3InfoRequest.class), any());
    verify(this.pbpFeign).findSystemParameter(Constants.YOUTUBE_VALIDATION_SWITCH);
    verify(youTube, Mockito.times(2)).videos();
    verify(list).execute();
  }

  @Test
  void updateProductInfoInvalidProductTypeTest() throws Exception {
    UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest =
        UpdateProductLevel3InfoRequest.builder().productType(4).build();
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productService.updateProductInfo(updateProductLevel3InfoRequest,
              BUSINESSPARTNER_CODE, true, PRODUCT_SKU));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(any());
    }
  }

  @Test
  void updateProductInfoInvalidProductTypeNegativeTest() throws Exception {
    UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest =
        UpdateProductLevel3InfoRequest.builder().productType(-4).build();
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productService.updateProductInfo(updateProductLevel3InfoRequest, BUSINESSPARTNER_CODE,
              true, PRODUCT_SKU));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(any());
    }
  }

  @Test
  public void fetchItemNamesByItemSkuTest() {
    SimpleMapStringResponse simpleMapStringResponse = new SimpleMapStringResponse(ImmutableMap.of(ITEM_SKU, ITEM_NAME));
    when(xProductFeign.getItemNameByItemSkus(new SimpleListStringRequest(Arrays.asList(ITEM_SKU)))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, simpleMapStringResponse, REQUEST_ID));

    Map<String, String> result = productService.fetchItemNamesByItemSku(Arrays.asList(ITEM_SKU));

    verify(xProductFeign).getItemNameByItemSkus(new SimpleListStringRequest(Arrays.asList(ITEM_SKU)));

    Assertions.assertEquals(ITEM_NAME, result.get(ITEM_SKU));
  }

  @Test
  public void updateImagesTest() throws Exception {
    ProductImageEditWebRequest productImageEditWebRequest = new ProductImageEditWebRequest();
    productImageEditWebRequest.setImagePath(LOCATION_PATH);
    when(imageService.imageExistsAndValid(LOCATION_PATH)).thenReturn(true);
    when(pbpFeign.updateImages(any(ProductImageEditRequest.class))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new ItemsPriceStockImagesUpdateResponse(), REQUEST_ID));

    productService.updateImages(PRODUCT_SKU, BUSINESSPARTNER_CODE, productImageEditWebRequest);

    verify(pbpFeign).updateImages(any(ProductImageEditRequest.class));
    verify(imageService).imageExistsAndValid(LOCATION_PATH);
  }

  @Test
  public void updateImagesWithInputStreamTest() throws Exception {
    ReflectionTestUtils.setField(productService, "imageFormatsSupported",
        List.of("png", "jpg", "jpeg"));
    ProductImageEditWebRequest productImageEditWebRequest = new ProductImageEditWebRequest();
    productImageEditWebRequest.setImageData(LOCATION_PATH);
    when(imageService.imageExistsAndValid(any())).thenReturn(true);
    when(pbpFeign.updateImages(any(ProductImageEditRequest.class))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new ItemsPriceStockImagesUpdateResponse(), REQUEST_ID));
    when(imageService.getImageInputStream(any())).thenReturn(
        new FileInputStream(SRC_TEST_RESOURCES_QR_PIN_PNG));
    when(imageService.uploadImage(any(UploadImageRequest.class))).thenReturn(true);
    when(pbpFeign.getL3DetailByProductSku(any(), anyBoolean())).thenReturn(
        new GdnRestSingleResponse<>(new ProductLevel3DetailResponse(), REQUEST_ID));
    productService.updateImages(PRODUCT_SKU, BUSINESSPARTNER_CODE, productImageEditWebRequest);
    verify(imageService).getImageInputStream(any());
    verify(imageService).uploadImage(any(UploadImageRequest.class));
    verify(pbpFeign).updateImages(any(ProductImageEditRequest.class));
    verify(imageService).imageExistsAndValid(any());
    verify(pbpFeign).getL3DetailByProductSku(any(), anyBoolean());
    verify(fileStorageService).generatePath(any(), any());
  }

  @Test
  public void updateImages_Webp_Test() throws Exception {
    ReflectionTestUtils.setField(productService, "imageFormatsSupported",
        List.of("png", "jpg", "jpeg", "webp"));
    ProductImageEditWebRequest productImageEditWebRequest = new ProductImageEditWebRequest();
    productImageEditWebRequest.setImageData(
        "https://static.desty.app/desty-omni/20250219/21d96f7ef007451e8839997f827cbf61.jpg");

    when(imageService.imageExistsAndValid(any())).thenReturn(true);
    when(pbpFeign.updateImages(any(ProductImageEditRequest.class))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new ItemsPriceStockImagesUpdateResponse(),
            REQUEST_ID));

    FileInputStream fileInputStream = new FileInputStream(SRC_TEST_RESOURCES_QR_PIN_PNG);
    when(imageService.getImageInputStream(any())).thenReturn(fileInputStream);

    when(imageService.uploadImage(any(UploadImageRequest.class))).thenReturn(true);
    when(fileStorageService.generatePath(Mockito.anyString(), Mockito.anyString())).thenReturn(
        "catalog-image/PRODUCT_CODE/fileName.jpeg");

    ProductLevel3DetailResponse productLevel3DetailResponse = new ProductLevel3DetailResponse();
    productLevel3DetailResponse.setProductCode(PRODUCT_CODE);
    productLevel3DetailResponse.setProductName(PRODUCT_NAME);

    when(pbpFeign.getL3DetailByProductSku(any(), anyBoolean())).thenReturn(
        new GdnRestSingleResponse<>(productLevel3DetailResponse, REQUEST_ID));

    productService.updateImages(PRODUCT_SKU, BUSINESSPARTNER_CODE, productImageEditWebRequest);

    verify(imageService).getImageInputStream(any());
    verify(imageService).uploadImage(uploadImageRequestArgumentCaptor.capture());
    verify(pbpFeign).updateImages(any(ProductImageEditRequest.class));
    verify(imageService).imageExistsAndValid(any());
    verify(pbpFeign).getL3DetailByProductSku(any(), anyBoolean());
    verify(fileStorageService).generatePath(any(), any());

    Assertions.assertTrue(uploadImageRequestArgumentCaptor.getValue().getImageFileName()
        .contains(Constants.WEBP_EXTENSION));
    Assertions.assertEquals(Constants.WEBP_FORMAT,
        uploadImageRequestArgumentCaptor.getValue().getOriginalFileType());

    fileInputStream.close();
  }

  @Test
  void updateImagesImagesNotPresentTest() throws Exception {
    ProductImageEditWebRequest productImageEditWebRequest = new ProductImageEditWebRequest();
    productImageEditWebRequest.setImagePath(LOCATION_PATH);
    when(imageService.imageExistsAndValid(LOCATION_PATH)).thenReturn(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productService.updateImages(PRODUCT_SKU, BUSINESSPARTNER_CODE,
              productImageEditWebRequest));
    } finally {
      verify(imageService).imageExistsAndValid(LOCATION_PATH);
    }
  }

  @Test
  void updateImagesWithImageDataExceptionTest() throws Exception {
    ProductImageEditWebRequest productImageEditWebRequest = new ProductImageEditWebRequest();
    productImageEditWebRequest.setImageData(LOCATION_PATH);
    when(pbpFeign.updateImages(any(ProductImageEditRequest.class))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new ItemsPriceStockImagesUpdateResponse(), REQUEST_ID));
    when(imageService.getImageInputStream(LOCATION_PATH)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productService.updateImages(PRODUCT_SKU, BUSINESSPARTNER_CODE,
              productImageEditWebRequest));
    } finally {
      verify(imageService).getImageInputStream(LOCATION_PATH);
    }
  }

  @Test
  void updateImagesInvalidRequestExceptionTest() throws Exception {
    ProductImageEditWebRequest productImageEditWebRequest = new ProductImageEditWebRequest();
    productImageEditWebRequest.setImageData(LOCATION_PATH);
    productImageEditWebRequest.setImagePath(LOCATION_PATH);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> productService.updateImages(PRODUCT_SKU, BUSINESSPARTNER_CODE,
            productImageEditWebRequest));
  }

  @Test()
  public void getL4ItemListByProductSku() {
    ProductBundleRecipeEditableResponse productBundleRecipeEditableResponseContent =
        ProductBundleRecipeEditableResponse.builder().itemCode(ITEM_CODE).bundleRecipeEditable(true).build();
    GdnRestListResponse<ProductBundleRecipeEditableResponse> productAssemblyResponse =
        new GdnRestListResponse<ProductBundleRecipeEditableResponse>("ERROR_MESSAGE", "ERROR_CODE",
            false, Arrays.asList(productBundleRecipeEditableResponseContent), new PageMetaData(0,
            10, 100), REQUEST_ID);
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU_NEW);
    ItemLevel4WebRequest webRequest = new ItemLevel4WebRequest(productSkus);
    ItemLevel4ListingWebRequest clientRequest = RequestHelper.toItemLevel4WebRequest(webRequest);
    Page<ItemLevel4ListingWebResponse> responsePage = null;
    Page<ItemLevel4ListingResponse> listingResponsePage = null;
    when(xProductFeign.getL4ItemListByProductSku(STORE_ID,REQUEST_ID,PAGE,SIZE,clientRequest))
      .thenReturn(new GdnRestListResponse<ItemLevel4ListingResponse>(null, null, true, Collections
        .singletonList(
          ItemLevel4ListingResponse.builder().productSku(PRODUCT_SKU).itemSku(ITEM_SKU).itemCode(ITEM_CODE).brand(BRAND)
            .fbbPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)).build()), new PageMetaData(PAGE, SIZE, TOTAL_ELEMENTS), REQUEST_ID));
    when(productAssemblyFeign.getBundleRecipeEditableInfoByItemCodes(Collections.singletonList(ITEM_CODE)))
        .thenReturn(productAssemblyResponse);
    ReflectionTestUtils.setField(productService, "productBundlingEnabled", true);
    try {
      responsePage = productService.getL4ItemListByProductSku(STORE_ID, REQUEST_ID, 0, 10, webRequest);
      ResponseHelper.toItemLevel4ListingWebResponse(
        new GdnRestListResponse<ItemLevel4ListingResponse>(null, null, true, Arrays.asList(
          ItemLevel4ListingResponse.builder().productSku(PRODUCT_SKU).itemCode(ITEM_CODE).itemSku(ITEM_SKU).brand(BRAND)
            .build()), new PageMetaData(PAGE, SIZE, TOTAL_ELEMENTS), REQUEST_ID), productAssemblyResponse, "");
    } finally {
      verify(xProductFeign).getL4ItemListByProductSku(STORE_ID, REQUEST_ID,
        PAGE, SIZE, clientRequest);
      verify(productAssemblyFeign).getBundleRecipeEditableInfoByItemCodes(Collections.singletonList(ITEM_CODE));
      assertNotNull(responsePage);
      assertFalse(responsePage.getContent().isEmpty());
      Assertions.assertEquals(PICKUP_POINT_CODE,responsePage.getContent().get(0).getFbbPickupPointCodes().get(0));
    }
  }

  @Test()
  public void getL4ItemListByProductSkSwitchOff() {
    ProductBundleRecipeEditableResponse productBundleRecipeEditableResponseContent =
        ProductBundleRecipeEditableResponse.builder().itemCode(ITEM_CODE).bundleRecipeEditable(true).build();
    GdnRestListResponse<ProductBundleRecipeEditableResponse> productAssemblyResponse =
        new GdnRestListResponse<ProductBundleRecipeEditableResponse>("ERROR_MESSAGE", "ERROR_CODE",
            false, Arrays.asList(productBundleRecipeEditableResponseContent), new PageMetaData(0,
            10, 100), REQUEST_ID);
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU_NEW);
    ItemLevel4WebRequest webRequest = new ItemLevel4WebRequest(productSkus);
    ItemLevel4ListingWebRequest clientRequest = RequestHelper.toItemLevel4WebRequest(webRequest);
    Page<ItemLevel4ListingWebResponse> responsePage = null;
    Page<ItemLevel4ListingResponse> listingResponsePage = null;
    when(xProductFeign.getL4ItemListByProductSku(STORE_ID,REQUEST_ID,PAGE,SIZE,clientRequest))
        .thenReturn(new GdnRestListResponse<ItemLevel4ListingResponse>(null, null, true, Collections
            .singletonList(
                ItemLevel4ListingResponse.builder().productSku(PRODUCT_SKU).itemSku(ITEM_SKU).itemCode(ITEM_CODE).brand(BRAND)
                    .fbbPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)).build()), new PageMetaData(PAGE, SIZE, TOTAL_ELEMENTS), REQUEST_ID));
    when(productAssemblyFeign.getBundleRecipeEditableInfoByItemCodes(Collections.singletonList(ITEM_CODE)))
        .thenReturn(productAssemblyResponse);
    ReflectionTestUtils.setField(productService, "productBundlingEnabled", false);
    try {
      responsePage = productService.getL4ItemListByProductSku(STORE_ID, REQUEST_ID, 0, 10, webRequest);
      ResponseHelper.toItemLevel4ListingWebResponse(
          new GdnRestListResponse<ItemLevel4ListingResponse>(null, null, true, Arrays.asList(
              ItemLevel4ListingResponse.builder().productSku(PRODUCT_SKU).itemCode(ITEM_CODE).itemSku(ITEM_SKU).brand(BRAND)
                  .build()), new PageMetaData(PAGE, SIZE, TOTAL_ELEMENTS), REQUEST_ID), productAssemblyResponse, "");
    } finally {
      verify(xProductFeign).getL4ItemListByProductSku(STORE_ID, REQUEST_ID,
          PAGE, SIZE, clientRequest);
      assertNotNull(responsePage);
      assertFalse(responsePage.getContent().isEmpty());
      Assertions.assertEquals(PICKUP_POINT_CODE,responsePage.getContent().get(0).getFbbPickupPointCodes().get(0));
    }
  }

  @Test
  void getL4ItemListByProductSku_ExceptionTest() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU_NEW);
    ItemLevel4WebRequest webRequest = new ItemLevel4WebRequest(productSkus);
    ItemLevel4ListingWebRequest clientRequest = new ItemLevel4ListingWebRequest();
    clientRequest.setProductSkus(productSkus);
    try {
          Assertions.assertThrows(ClientException.class,
              () -> productService.getL4ItemListByProductSku(STORE_ID, REQUEST_ID, PAGE, SIZE,
                  webRequest));
    } finally {
      verify(xProductFeign).getL4ItemListByProductSku(STORE_ID, REQUEST_ID, PAGE, SIZE, clientRequest);
    }
  }

  @Test()
  public void getL4ItemListByProductSku_EmptyResponseTest() {
    ProductBundleRecipeEditableResponse productBundleRecipeEditableResponseContent =
        ProductBundleRecipeEditableResponse.builder().itemCode(ITEM_CODE).bundleRecipeEditable(true).build();
    ReflectionTestUtils.setField(productService, "productBundlingEnabled", true);
    GdnRestListResponse<ProductBundleRecipeEditableResponse> productAssemblyResponse =
        new GdnRestListResponse<ProductBundleRecipeEditableResponse>("ERROR_MESSAGE", "ERROR_CODE", false,
            Arrays.asList(productBundleRecipeEditableResponseContent),
            new PageMetaData(0, 10, 100), REQUEST_ID);
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU_NEW);
    ItemLevel4WebRequest webRequest = new ItemLevel4WebRequest(productSkus);
    ItemLevel4ListingWebRequest clientRequest = new ItemLevel4ListingWebRequest();
    ItemLevel4ListingResponse itemLevel4ListingWebResponse = new ItemLevel4ListingResponse();
    itemLevel4ListingWebResponse.setItemSku(ITEM_SKU);
    itemLevel4ListingWebResponse.setItemCode(ITEM_CODE);
    clientRequest.setProductSkus(productSkus);
    when(xProductFeign.getL4ItemListByProductSku(STORE_ID, REQUEST_ID, PAGE, SIZE, clientRequest)).thenReturn(
        new GdnRestListResponse<ItemLevel4ListingResponse>(null, null, true,
            Arrays.asList(itemLevel4ListingWebResponse), new PageMetaData(PAGE, SIZE, TOTAL_ELEMENTS), REQUEST_ID));
    when(productAssemblyFeign.getBundleRecipeEditableInfoByItemCodes(Collections.singletonList(ITEM_CODE)))
        .thenReturn(productAssemblyResponse);
    Page<ItemLevel4ListingWebResponse> l4ItemListByProductSku =
        productService.getL4ItemListByProductSku(STORE_ID, REQUEST_ID, PAGE, SIZE, webRequest);
    verify(xProductFeign).getL4ItemListByProductSku(STORE_ID, REQUEST_ID, PAGE, SIZE, clientRequest);
    verify(productAssemblyFeign).getBundleRecipeEditableInfoByItemCodes(Collections.singletonList(ITEM_CODE));
    Assertions.assertTrue(Objects.nonNull(l4ItemListByProductSku));
  }

  @Test
  public void getL4ItemListByProductSkuNeedRevisionTest() {
    ProductBundleRecipeEditableResponse productBundleRecipeEditableResponseContent =
        ProductBundleRecipeEditableResponse.builder().itemCode(ITEM_CODE).bundleRecipeEditable(true).build();
    GdnRestListResponse<ProductBundleRecipeEditableResponse> productAssemblyResponse =
        new GdnRestListResponse<ProductBundleRecipeEditableResponse>("ERROR_MESSAGE", "ERROR_CODE",
            false, Arrays.asList(productBundleRecipeEditableResponseContent), new PageMetaData(0,
            10, 100), REQUEST_ID);
    ReflectionTestUtils.setField(productService, "productBundlingEnabled", true);
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU_NEW);
    ItemLevel4WebRequest webRequest = new ItemLevel4WebRequest(productSkus);
    ItemLevel4ListingWebRequest clientRequest = new ItemLevel4ListingWebRequest();
    clientRequest.setProductSkus(productSkus);
    ItemSummaryL4Response itemSummaryL4Response = new ItemSummaryL4Response();
    itemSummaryL4Response.setProductSku(PRODUCT_SKU_NEW);
    itemSummaryL4Response.setItemSku(ITEM_SKU);
    itemSummaryL4Response.setItemCode(ITEM_CODE);
    when(pbpFeign.getL4ProductDetailsByProductSku(PRODUCT_SKU_NEW, PAGE, SIZE)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.singletonList(itemSummaryL4Response),
            new PageMetaData(PAGE, SIZE, TOTAL_ELEMENTS), REQUEST_ID));
    when(productAssemblyFeign.getBundleRecipeEditableInfoByItemCodes(Collections.singletonList(ITEM_CODE)))
        .thenReturn(productAssemblyResponse);
    webRequest.setNeedRevision(true);
    Page<ItemLevel4ListingWebResponse> l4ItemListByProductSku =
        productService.getL4ItemListByProductSku(STORE_ID, REQUEST_ID, PAGE, SIZE, webRequest);
    verify(pbpFeign).getL4ProductDetailsByProductSku(PRODUCT_SKU_NEW, PAGE, SIZE);
    verify(productAssemblyFeign).getBundleRecipeEditableInfoByItemCodes(Collections.singletonList(ITEM_CODE));
    Assertions.assertTrue(Objects.nonNull(l4ItemListByProductSku));
    Assertions.assertEquals(PRODUCT_SKU_NEW, l4ItemListByProductSku.getContent().get(0).getProductSku());
  }

  @Test
  public void getL4ItemListByProductSkuNeedRevisionSwitchOffTest() {
    ProductBundleRecipeEditableResponse productBundleRecipeEditableResponseContent =
        ProductBundleRecipeEditableResponse.builder().itemCode(ITEM_CODE).bundleRecipeEditable(true).build();
    GdnRestListResponse<ProductBundleRecipeEditableResponse> productAssemblyResponse =
        new GdnRestListResponse<ProductBundleRecipeEditableResponse>("ERROR_MESSAGE", "ERROR_CODE",
            false, Arrays.asList(productBundleRecipeEditableResponseContent), new PageMetaData(0,
            10, 100), REQUEST_ID);
    ReflectionTestUtils.setField(productService, "productBundlingEnabled", false);
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU_NEW);
    ItemLevel4WebRequest webRequest = new ItemLevel4WebRequest(productSkus);
    ItemLevel4ListingWebRequest clientRequest = new ItemLevel4ListingWebRequest();
    clientRequest.setProductSkus(productSkus);
    ItemSummaryL4Response itemSummaryL4Response = new ItemSummaryL4Response();
    itemSummaryL4Response.setProductSku(PRODUCT_SKU_NEW);
    itemSummaryL4Response.setItemSku(ITEM_SKU);
    itemSummaryL4Response.setItemCode(ITEM_CODE);
    when(pbpFeign.getL4ProductDetailsByProductSku(PRODUCT_SKU_NEW, PAGE, SIZE)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.singletonList(itemSummaryL4Response),
            new PageMetaData(PAGE, SIZE, TOTAL_ELEMENTS), REQUEST_ID));
    webRequest.setNeedRevision(true);
    Page<ItemLevel4ListingWebResponse> l4ItemListByProductSku =
        productService.getL4ItemListByProductSku(STORE_ID, REQUEST_ID, PAGE, SIZE, webRequest);
    verify(pbpFeign).getL4ProductDetailsByProductSku(PRODUCT_SKU_NEW, PAGE, SIZE);
    Assertions.assertTrue(Objects.nonNull(l4ItemListByProductSku));
    Assertions.assertEquals(PRODUCT_SKU_NEW, l4ItemListByProductSku.getContent().get(0).getProductSku());
  }

  @Test
  public void getProductUpdateHistoryTest() throws Exception {
    PageMetaData pageMetaData = new PageMetaData(PAGE, SIZE, SIZE);
    when(this.pbpFeign.getProductUpdateHistory(eq(PAGE), eq(SIZE),
      any(HistoryUpdateRequest.class))).thenReturn(
      new GdnRestListResponse<>(null, null, true, Collections.singletonList(historyUpdateResponse),
        pageMetaData, REQUEST_ID));
    Page<HistoryUpdateWebResponse> response =
      productService.getProductUpdateHistory(historyUpdateWebRequest, PAGE, SIZE);
    verify(this.pbpFeign).getProductUpdateHistory(eq(PAGE), eq(SIZE),
      historyUpdateRequestArgumentCaptor.capture());
    assertNotNull(response.getContent().get(0));
    Assertions.assertEquals(ACTIVITY, response.getContent().get(0).getActivity());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getGdnName());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, response.getContent().get(0).getPickupPointName());
    Assertions.assertEquals(ITEM_SKU, response.getContent().get(0).getGdnSku());
    Assertions.assertTrue(historyUpdateRequestArgumentCaptor.getValue().isBeforeOneMonths());
    Assertions.assertEquals(PRODUCT_SKU, historyUpdateRequestArgumentCaptor.getValue().getProductSku());
    Assertions.assertEquals(KEYWORD, historyUpdateRequestArgumentCaptor.getValue().getKeyword());
    Assertions.assertEquals(PICKUP_POINT_CODE,
      historyUpdateRequestArgumentCaptor.getValue().getPickupPointCode());
    assertNotNull(historyUpdateRequestArgumentCaptor.getValue().getStartDate());
    assertNotNull(historyUpdateRequestArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void getItemPickupPointListingByProductSkuTest() {
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setPickupPointCodes(Collections.emptySet());
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.emptySet());
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
      new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 1, 1), REQUEST_ID));
    when(userPicService.filterInaccessiblePickupPoints(eq(null), anySet()))
      .thenReturn(Collections.emptySet());
    productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU, itemPickupPointListingL3WebRequest,
        false);
    verify(userPicService).filterInaccessiblePickupPoints(eq(null), anySet());
    verify(pbpFeign).getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request);
  }

  @Test
  public void getItemPickupPointListingByProductSkuNeedRevisionTrueTest() {
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setNeedCorrection(true);
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setNeedCorrection(true);
    when(userPicService.filterInaccessiblePickupPoints(null,
      itemPickupPointListingL3WebRequest.getPickupPointCodes())).thenReturn(
      itemPickupPointListingL3WebRequest.getPickupPointCodes());
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 1, 1), REQUEST_ID));
    productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU, itemPickupPointListingL3WebRequest,
        false);
    verify(pbpFeign).getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request);
    verify(userPicService).filterInaccessiblePickupPoints(null,
      itemPickupPointListingL3WebRequest.getPickupPointCodes());
  }

  @Test
  public void getItemPickupPointListingByProductSkuTestForInProgressCF() {
    ReflectionTestUtils.setField(productService, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productService, "cfCountAtListingEnabled", true);
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setItemSku(ITEM_SKU);
    itemPickupPointListingL3WebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3WebRequest.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Request.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Arrays.asList(
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(10).build(),
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(5).build()
    ));
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setMerchantCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setViewConfigs(Collections.emptyList());
    itemPickupPointListingL3Response.setImages(Collections.emptyList());
    itemPickupPointListingL3Response.setPrices(Collections.emptyList());
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
      new GdnRestListResponse<>(null, null, true,Arrays.asList(itemPickupPointListingL3Response), new PageMetaData(0, 1, 1), REQUEST_ID));
    when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),anyString())).thenReturn(consignmentFormCountResponse);
    when(userPicService.filterInaccessiblePickupPoints(eq(null), anySet()))
      .thenReturn(Collections.singleton(PICKUP_POINT_CODE));
    productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU, itemPickupPointListingL3WebRequest,
        false);
    verify(userPicService).filterInaccessiblePickupPoints(eq(null), anySet());
    verify(fbbFeign).countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),anyString());
    verify(pbpFeign).getItemPickupPointL3Listing(anyInt(), anyInt(), anyBoolean(), anyBoolean(), any(ItemPickupPointListingL3Request.class));
  }

  @Test
  public void getItemPickupPointListingByProductSkuTestForInProgressCF_withFbbException() {
    ReflectionTestUtils.setField(productService, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productService, "cfCountAtListingEnabled", true);
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setItemSku(ITEM_SKU);
    itemPickupPointListingL3WebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3WebRequest.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Request.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(null);
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setMerchantCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setViewConfigs(Collections.emptyList());
    itemPickupPointListingL3Response.setImages(Collections.emptyList());
    itemPickupPointListingL3Response.setPrices(Collections.emptyList());
    itemPickupPointListingL3Response.setInitialPreOrderQuota(10);
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
      new GdnRestListResponse<>(null, null, true,Arrays.asList(itemPickupPointListingL3Response), new PageMetaData(0, 1, 1), REQUEST_ID));
    when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),anyString())).thenReturn(consignmentFormCountResponse);
    when(userPicService.filterInaccessiblePickupPoints(eq(null), anySet()))
      .thenReturn(Collections.singleton(PICKUP_POINT_CODE));
    Page<ItemPickupPointListingL3WebResponse> itemPickupPointListingByProductSku =
      productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU,
        itemPickupPointListingL3WebRequest, false);
    verify(userPicService).filterInaccessiblePickupPoints(eq(null), anySet());
    verify(fbbFeign).countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),anyString());
    verify(pbpFeign).getItemPickupPointL3Listing(anyInt(), anyInt(), anyBoolean(), anyBoolean(), any(ItemPickupPointListingL3Request.class));
    assertNotNull(itemPickupPointListingByProductSku);
    assertFalse(itemPickupPointListingByProductSku.getContent().stream()
      .map(ItemPickupPointListingL3WebResponse::getInProgressConsignmentCount)
      .anyMatch(Objects::nonNull));
    assertTrue(itemPickupPointListingByProductSku.getContent().get(0).getInitialPreOrderQuota() == 10);
  }

  @Test
  public void getItemPickupPointListingByProductSkuTestWithNoFbbL4_sTest() {
    ReflectionTestUtils.setField(productService, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productService, "cfCountAtListingEnabled", true);
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setItemSku(ITEM_SKU);
    itemPickupPointListingL3WebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3WebRequest.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Request.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setMerchantCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setViewConfigs(Collections.emptyList());
    itemPickupPointListingL3Response.setImages(Collections.emptyList());
    itemPickupPointListingL3Response.setPrices(Collections.emptyList());
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(itemPickupPointListingL3Response),
            new PageMetaData(0, 1, 1), REQUEST_ID));
    when(userPicService.filterInaccessiblePickupPoints(eq(null), anySet())).thenReturn(
        Collections.singleton(PICKUP_POINT_CODE));
    Page<ItemPickupPointListingL3WebResponse> itemPickupPointListingByProductSku =
        productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU,
            itemPickupPointListingL3WebRequest, false);
    verify(userPicService).filterInaccessiblePickupPoints(eq(null), anySet());
    verify(pbpFeign).getItemPickupPointL3Listing(anyInt(), anyInt(), anyBoolean(), anyBoolean(), any(ItemPickupPointListingL3Request.class));
    assertNotNull(itemPickupPointListingByProductSku);
  }

  @Test
  public void getItemPickupPointListingByProductSkuTestForInProgressCF_EmptyTest() {
    ReflectionTestUtils.setField(productService, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productService, "cfCountAtListingEnabled", true);
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setItemSku(ITEM_SKU);
    itemPickupPointListingL3WebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3WebRequest.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Request.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Arrays.asList(
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(10).build(),
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(5).build()
    ));
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setMerchantCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setViewConfigs(Collections.emptyList());
    itemPickupPointListingL3Response.setImages(Collections.emptyList());
    itemPickupPointListingL3Response.setPrices(Collections.emptyList());
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
      new GdnRestListResponse<>(null, null, true,Arrays.asList(), new PageMetaData(0, 1, 1), REQUEST_ID));
    when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),anyString())).thenReturn(consignmentFormCountResponse);
    when(userPicService.filterInaccessiblePickupPoints(eq(null), anySet()))
      .thenReturn(Collections.singleton(PICKUP_POINT_CODE));
    productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU, itemPickupPointListingL3WebRequest,
        false);
    verify(pbpFeign).getItemPickupPointL3Listing(anyInt(), anyInt(), anyBoolean(), anyBoolean(), any(ItemPickupPointListingL3Request.class));
    verify(userPicService).filterInaccessiblePickupPoints(eq(null), anySet());
  }

  @Test
  public void getItemPickupPointListingByProductSkuTestForInProgressCF2() {
    ReflectionTestUtils.setField(productService, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productService, "cfCountAtListingEnabled", true);
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setItemSku(ITEM_SKU);
    itemPickupPointListingL3WebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3WebRequest.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Request.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Collections.emptyList());
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setMerchantCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setViewConfigs(Collections.emptyList());
    itemPickupPointListingL3Response.setImages(Collections.emptyList());
    itemPickupPointListingL3Response.setPrices(Collections.emptyList());
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
      new GdnRestListResponse<>(null, null, true,Arrays.asList(itemPickupPointListingL3Response), new PageMetaData(0, 1, 1), REQUEST_ID));
    when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),anyString())).thenReturn(consignmentFormCountResponse);
    when(userPicService.filterInaccessiblePickupPoints(eq(null), anySet()))
      .thenReturn(Collections.singleton(PICKUP_POINT_CODE));
    productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU, itemPickupPointListingL3WebRequest,
        false);
    verify(fbbFeign).countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),anyString());
    verify(pbpFeign).getItemPickupPointL3Listing(anyInt(), anyInt(), anyBoolean(), anyBoolean(), any(ItemPickupPointListingL3Request.class));
    verify(userPicService).filterInaccessiblePickupPoints(eq(null), anySet());
  }

  @Test
  public void getItemPickupPointListingByProductSkuForResizeSwitchOnTest() {
    ReflectionTestUtils.setField(productService, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productService, "cfCountAtListingEnabled", true);
    ReflectionTestUtils.setField(productService,"resizeImageRemoval",true);
    ReflectionTestUtils.setField(productService,"overrideImageActiveFlagForL5Listing",true);
    ReflectionTestUtils.setField(productService,"resizeImagePathList","resize/");
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setItemSku(ITEM_SKU);
    itemPickupPointListingL3WebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3WebRequest.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Request.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
        new Response<>();
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Collections.emptyList());
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setMerchantCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setViewConfigs(Collections.emptyList());
    List<ImageResponse> imageResponsesList = new ArrayList<>();
    ImageResponse imageResponse = new ImageResponse();
    imageResponse.setLocationPath("resize/");
    imageResponse.setActiveLocation(false);
    ImageResponse imageResponse1= new ImageResponse();
    imageResponse1.setLocationPath("resize1/");
    imageResponse1.setActiveLocation(false);
    imageResponsesList.add(imageResponse);
    imageResponsesList.add(imageResponse1);
    imageResponsesList.add(null);
    itemPickupPointListingL3Response.setImages(imageResponsesList);
    itemPickupPointListingL3Response.setPrices(Collections.emptyList());
    when(userPicService.filterInaccessiblePickupPoints(eq(null), anySet()))
      .thenReturn(Collections.singleton(PICKUP_POINT_CODE));
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
        new GdnRestListResponse<>(null, null, true,Arrays.asList(itemPickupPointListingL3Response), new PageMetaData(0, 1, 1), REQUEST_ID));
    when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
        CountConsignmentFormsByItemSkusRequest.class),anyString())).thenReturn(consignmentFormCountResponse);
    Page<ItemPickupPointListingL3WebResponse> itemPickupPointListingL3WebResponsePage = productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU, itemPickupPointListingL3WebRequest,
        false);
    verify(fbbFeign).countInProgressConsignmentFormsByItemSkus(any(
        CountConsignmentFormsByItemSkusRequest.class),anyString());
    verify(pbpFeign).getItemPickupPointL3Listing(anyInt(), anyInt(), anyBoolean(), anyBoolean(), any(ItemPickupPointListingL3Request.class));
    verify(userPicService).filterInaccessiblePickupPoints(eq(null), anySet());
    Assertions.assertTrue(
        itemPickupPointListingL3WebResponsePage.getContent().stream().findFirst().get().getImages().stream()
            .allMatch(ItemImageWebResponse::isActiveLocation));
  }

  @Test
  public void getItemPickupPointListingByProductSkuForResizeSwitchOnActiveTrueTest() {
    ReflectionTestUtils.setField(productService, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productService, "cfCountAtListingEnabled", true);
    ReflectionTestUtils.setField(productService,"resizeImageRemoval",true);
    ReflectionTestUtils.setField(productService,"resizeImagePathList","resize/");
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setItemSku(ITEM_SKU);
    itemPickupPointListingL3WebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3WebRequest.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Request.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
        new Response<>();
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Collections.emptyList());
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setMerchantCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setViewConfigs(Collections.emptyList());
    List<ImageResponse> imageResponsesList = new ArrayList<>();
    ImageResponse imageResponse = new ImageResponse();
    imageResponse.setLocationPath("resize/");
    imageResponse.setActiveLocation(true);
    ImageResponse imageResponse1= new ImageResponse();
    imageResponse1.setLocationPath("resize1/");
    imageResponsesList.add(imageResponse);
    imageResponsesList.add(imageResponse1);
    imageResponsesList.add(null);
    itemPickupPointListingL3Response.setImages(imageResponsesList);
    itemPickupPointListingL3Response.setPrices(Collections.emptyList());
    when(userPicService.filterInaccessiblePickupPoints(null,
      itemPickupPointListingL3WebRequest.getPickupPointCodes())).thenReturn(
      itemPickupPointListingL3WebRequest.getPickupPointCodes());
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
        new GdnRestListResponse<>(null, null, true,Arrays.asList(itemPickupPointListingL3Response), new PageMetaData(0, 1, 1), REQUEST_ID));
    when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
        CountConsignmentFormsByItemSkusRequest.class),anyString())).thenReturn(consignmentFormCountResponse);
    Page<ItemPickupPointListingL3WebResponse> itemPickupPointListingL3WebResponsePage = productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU, itemPickupPointListingL3WebRequest,
        false);
    verify(userPicService).filterInaccessiblePickupPoints(null,
      itemPickupPointListingL3WebRequest.getPickupPointCodes());
    verify(fbbFeign).countInProgressConsignmentFormsByItemSkus(any(
        CountConsignmentFormsByItemSkusRequest.class),anyString());
    verify(pbpFeign).getItemPickupPointL3Listing(anyInt(), anyInt(), anyBoolean(), anyBoolean(), any(ItemPickupPointListingL3Request.class));
  }

  @Test
  public void getItemPickupPointListingByProductSkuWithImagesNullTest() {
    ReflectionTestUtils.setField(productService, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productService, "cfCountAtListingEnabled", true);
    ReflectionTestUtils.setField(productService,"resizeImageRemoval",true);
    ReflectionTestUtils.setField(productService,"resizeImagePathList","resize/");
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setItemSku(ITEM_SKU);
    itemPickupPointListingL3WebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3WebRequest.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Request.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
        new Response<>();
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Collections.emptyList());
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setMerchantCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setViewConfigs(Collections.emptyList());
    itemPickupPointListingL3Response.setImages(null);
    itemPickupPointListingL3Response.setPrices(Collections.emptyList());
    when(userPicService.filterInaccessiblePickupPoints(null,
        itemPickupPointListingL3WebRequest.getPickupPointCodes())).thenReturn(
        itemPickupPointListingL3WebRequest.getPickupPointCodes());
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
        new GdnRestListResponse<>(null, null, true,Arrays.asList(itemPickupPointListingL3Response), new PageMetaData(0, 1, 1), REQUEST_ID));
    when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
        CountConsignmentFormsByItemSkusRequest.class),anyString())).thenReturn(consignmentFormCountResponse);
    productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU, itemPickupPointListingL3WebRequest,
        false);
    verify(userPicService).filterInaccessiblePickupPoints(null,
        itemPickupPointListingL3WebRequest.getPickupPointCodes());
    verify(fbbFeign).countInProgressConsignmentFormsByItemSkus(any(
        CountConsignmentFormsByItemSkusRequest.class),anyString());
    verify(pbpFeign).getItemPickupPointL3Listing(anyInt(), anyInt(), anyBoolean(), anyBoolean(), any(ItemPickupPointListingL3Request.class));
  }

  @Test
  void getItemPickupPointListingByProductSkuTestForInProgressCFEmptyMerchantCode() {
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest = new ItemPickupPointListingL3WebRequest();
    itemPickupPointListingL3WebRequest.setItemSku(ITEM_SKU);
    itemPickupPointListingL3WebRequest.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    itemPickupPointListingL3WebRequest.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Collections.emptyList());
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setViewConfigs(Collections.emptyList());
    itemPickupPointListingL3Response.setImages(Collections.emptyList());
    itemPickupPointListingL3Response.setPrices(Collections.emptyList());
    when(pbpFeign.getItemPickupPointL3Listing(0, 1, false, false, itemPickupPointListingL3Request)).thenReturn(
      new GdnRestListResponse<>(null, null, true,Arrays.asList(itemPickupPointListingL3Response), new PageMetaData(0, 1, 1), REQUEST_ID));
    when(userPicService.filterInaccessiblePickupPoints(eq(null), anySet()))
      .thenReturn(Collections.singleton(PICKUP_POINT_CODE));
    try{
      Assertions.assertThrows(ClientException.class,
          () -> productService.getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU,
              itemPickupPointListingL3WebRequest, false));
   }
    finally {
      verify(userPicService).filterInaccessiblePickupPoints(eq(null), anySet());
      verify(pbpFeign).getItemPickupPointL3Listing(anyInt(), anyInt(), anyBoolean(), anyBoolean(), any(ItemPickupPointListingL3Request.class));
    }

  }


  @Test
  public void getPickupDetailByCodeTest() throws Exception {
    List<String> pickupPointCodes = Arrays.asList(PICKUP_POINT_CODE);
    List<BusinessPartnerPickupPointWebResponse> pickupDetailByCode = null;
    SimpleListStringRequest request = new SimpleListStringRequest();
    request.setValue(pickupPointCodes);
    BusinessPartnerPickupPointOutboundResponse response =
        BusinessPartnerPickupPointOutboundResponse.builder().businessPartnerCode(BUSINESSPARTNER_CODE)
        .build();
    GdnRestListResponse<BusinessPartnerPickupPointOutboundResponse> restListResponse =
      new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(response), pageMetaData, REQUEST_ID);
    when(xProductFeign.getPickupDetailByCodes(request, MERCHANT_CODE)).thenReturn(restListResponse);
    pickupDetailByCode = productService.getPickupDetailByCode(pickupPointCodes, MERCHANT_CODE);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(pickupPointCodes));
    verify(xProductFeign).getPickupDetailByCodes(request, MERCHANT_CODE);
    assertNotNull(pickupDetailByCode);
  }

  @Test
  void getPickupDetailByCodeExceptionTest() throws Exception {
    List<String> pickupPointCodes = Collections.emptyList();
    List<BusinessPartnerPickupPointWebResponse> pickupDetailByCode = null;
    SimpleListStringRequest request = new SimpleListStringRequest();
    request.setValue(pickupPointCodes);
    BusinessPartnerPickupPointResponse response =
      BusinessPartnerPickupPointResponse.builder().businessPartnerCode(BUSINESSPARTNER_CODE)
        .build();
    GdnRestListResponse<BusinessPartnerPickupPointResponse> restListResponse =
      new GdnRestListResponse<BusinessPartnerPickupPointResponse>(null, null, true,
        Collections.singletonList(response), pageMetaData, REQUEST_ID);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> productService.getPickupDetailByCode(pickupPointCodes, MERCHANT_CODE));
  }

  @Test
  public void editL5ProductStockAndPriceTest() throws Exception {
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setL3Version(10L);
    Mockito.when(pbpFeign.editL5PriceStockInfo(any()))
        .thenReturn(new GdnRestSingleResponse(null, null, true, itemsPriceStockImagesUpdateResponse, REQUEST_ID));
    ItemsPriceStockImagesUpdateWebResponse response =
        productService.editL5ProductStockAndPrice(productVariantUpdateWebRequest, BUSINESSPARTNER_CODE);
    verify(pbpFeign).editL5PriceStockInfo(any());
    verify(userPicService)
        .validateUserPicPickupPoints(Collections.singleton(PICKUP_POINT_CODE));
    Assertions.assertEquals(10L, response.getL3Version(), 0);
  }

  @Test
  void editL5ProductStockAndPriceTest_inAccessiblePickupPointCodes() throws Exception {
    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setL3Version(10L);
    doThrow(new ValidationException(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT)).when(userPicService)
        .validateUserPicPickupPoints(pickupPointSetInput);
    try {
      Assertions.assertThrows(ValidationException.class,
              () -> productService.editL5ProductStockAndPrice(productVariantUpdateWebRequest,
                  BUSINESSPARTNER_CODE));
    } finally {
      verify(userPicService).validateUserPicPickupPoints(pickupPointSetInput);
    }
  }

  @Test
  public void editL5ProductStockAndPrice1Test() throws Exception {
    productVariantUpdateWebRequest.setCommonImages(new ArrayList<>());
    productVariantUpdateWebRequest.setAddPickupPoints(new ArrayList<>());
    productVariantUpdateWebRequest.setDeletePickupPoints(new ArrayList<>());
    productVariantUpdateWebRequest.getProductItems().get(0).setModifiedItemPickupPoints(new ArrayList<>());
    productVariantUpdateWebRequest.getProductItems().get(0).setImages(new ArrayList<>());
    Mockito.when(pbpFeign.editL5PriceStockInfo(any()))
        .thenReturn(new GdnRestSingleResponse(null, null, true, new ItemsPriceStockImagesUpdateResponse(), REQUEST_ID));
    productService.editL5ProductStockAndPrice(productVariantUpdateWebRequest, BUSINESSPARTNER_CODE);
    verify(pbpFeign).editL5PriceStockInfo(any());
  }

  @Test
  public void downloadProductTemplateGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", true);
    when(gcsProperties.isEnabled()).thenReturn(true);
    when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-upload-template");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    profileResponse.getCompany().setInternationalFlag(false);
    profileResponse.setBigProductFlag(false);
    profileResponse.setBopisFlag(false);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of(PICKUP_POINT_CODE));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
      Set.of(PICKUP_POINT_CODE))).thenReturn(pickupPointResponseList);
    when(gcsConfig.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(blob);
    when(this.categoryService.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponse);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID2)).thenReturn(attributeResponse2);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.productService.downloadProductTemplate(USERNAME, BUSINESSPARTNER_CODE, CATEGORY_ID, httpServletResponse, false);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
      Set.of(PICKUP_POINT_CODE));
    verify(categoryService).getCategoryDetail(CATEGORY_ID);
  }

  @Test
  public void downloadProductTemplateGCSEnabledHIdeFromSellerTest() throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", true);
    ReflectionTestUtils.setField(productService, "productSuitabilityFeatureEnabled", true);
    when(gcsProperties.isEnabled()).thenReturn(true);
    when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-upload-template");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    profileResponse.getCompany().setInternationalFlag(false);
    profileResponse.setBigProductFlag(false);
    profileResponse.setBopisFlag(false);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of(PICKUP_POINT_CODE));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
        Set.of(PICKUP_POINT_CODE))).thenReturn(pickupPointResponseList);
    when(gcsConfig.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(blob);
    when(this.categoryService.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponse);
    attributeResponse2.setHideForSeller(true);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID2)).thenReturn(attributeResponse2);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.productService.downloadProductTemplate(USERNAME, BUSINESSPARTNER_CODE, CATEGORY_ID, httpServletResponse, false);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE,
        Set.of(PICKUP_POINT_CODE));
    verify(categoryService).getCategoryDetail(CATEGORY_ID);
  }

  @Test
  public void downloadProductTemplate_noAccessRestrictionTest() throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", true);
    when(gcsProperties.isEnabled()).thenReturn(true);
    when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-upload-template");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    profileResponse.getCompany().setInternationalFlag(false);
    profileResponse.setBigProductFlag(false);
    profileResponse.setBopisFlag(false);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(false);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
      .thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Collections.emptySet()))
      .thenReturn(pickupPointResponseList);
    when(gcsConfig.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(blob);
    when(this.categoryService.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponse);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID2)).thenReturn(attributeResponse2);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.productService.downloadProductTemplate(USERNAME, BUSINESSPARTNER_CODE, CATEGORY_ID, httpServletResponse, false);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Collections.emptySet());
    verify(categoryService).getCategoryDetail(CATEGORY_ID);
  }

  @Test
  public void downloadProductTemplateProductBundlingEnabledAndTdMerchantTest() throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", true);
    ReflectionTestUtils.setField(productService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productService, "productBundlingEligibleMerchantTypes", "TD");
    when(gcsProperties.isEnabled()).thenReturn(true);
    when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-upload-template");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    profileResponse.getCompany().setInternationalFlag(false);
    profileResponse.setBigProductFlag(false);
    profileResponse.setBopisFlag(false);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of(PICKUP_POINT_CODE));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE)))
        .thenReturn(pickupPointResponseList);
    when(gcsConfig.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(blob);
    when(this.categoryService.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponse);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID2)).thenReturn(attributeResponse2);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.productService.downloadProductTemplate(USERNAME, BUSINESSPARTNER_CODE, CATEGORY_ID, httpServletResponse, false);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE));
    verify(categoryService).getCategoryDetail(CATEGORY_ID);
  }

  @Test
  public void downloadProductTemplateProductBundlingEnabledAndEligibleMerchantTypeTest() throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", true);
    ReflectionTestUtils.setField(productService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productService, "productBundlingEligibleMerchantTypes", "TD");
    when(gcsProperties.isEnabled()).thenReturn(true);
    when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-upload-template");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    profileResponse.getCompany().setInternationalFlag(false);
    profileResponse.getCompany().setMerchantType("TD");
    profileResponse.setBigProductFlag(false);
    profileResponse.setBopisFlag(false);
    categoryDetailResponse.getCategoryAttributes().get(2).getAttribute().setSkuValue(true);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of(PICKUP_POINT_CODE));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE)))
        .thenReturn(pickupPointResponseList);
    when(gcsConfig.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(blob);
    when(this.categoryService.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponse);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID2)).thenReturn(attributeResponse2);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.productService.downloadProductTemplate(USERNAME, BUSINESSPARTNER_CODE, CATEGORY_ID, httpServletResponse, false);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE));
    verify(categoryService).getCategoryDetail(CATEGORY_ID);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
  }


  @Test
  void downloadProductTemplateProductBundlingEnabledAndEligibleMerchantTypeNoOtherAttributeTest()
      throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", true);
    ReflectionTestUtils.setField(productService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productService, "productBundlingEligibleMerchantTypes", "TD");
    when(gcsProperties.isEnabled()).thenReturn(true);
    when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-upload-template");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    profileResponse.getCompany().setInternationalFlag(false);
    profileResponse.getCompany().setMerchantType("TD");
    profileResponse.setBigProductFlag(false);
    profileResponse.setBopisFlag(false);
    categoryDetailResponse.setCategoryAttributes(new ArrayList<>());
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of(PICKUP_POINT_CODE));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE))).thenReturn(
        pickupPointResponseList);
    when(gcsConfig.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(blob);
    when(this.categoryService.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.productService.downloadProductTemplate(USERNAME, BUSINESSPARTNER_CODE, CATEGORY_ID, httpServletResponse,
          false));
    } finally {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE));
      verify(categoryService).getCategoryDetail(CATEGORY_ID);
      verify(this.userPicService).shouldRestrictAccess(profileResponse);
      verify(this.mandatoryParameterHelper).getPickupPoints();
    }
  }

  @Test
  public void downloadProductTemplateInternationalGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", true);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-template-upload-English");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    profileResponse.getCompany().setInternationalFlag(true);
    profileResponse.setBigProductFlag(true);
    profileResponse.setBopisFlag(true);
    when(gcsProperties.isEnabled()).thenReturn(true);
    when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of(PICKUP_POINT_CODE));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE)))
        .thenReturn(pickupPointResponseList);
    when(gcsConfig.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(blob);
    when(this.categoryService.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponse);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID2)).thenReturn(attributeResponse2);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.productService.downloadProductTemplate(USERNAME, BUSINESSPARTNER_CODE, CATEGORY_ID, httpServletResponse, false);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE));
    verify(categoryService).getCategoryDetail(CATEGORY_ID);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
  }

  private Map<String, String> getFiles(String fileName) throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.getEncoder().encode(IOUtils.toByteArray(
      Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("Excel-template" + File.separator + fileName
          + ".xlsx"))),
      "UTF-8");
    files.put("xls", excelData);
    return files;
  }

  @Test
  public void getExternalDownloadTemplateFilePathsTest() {
    when(fileStorageService.getExternalDownloadTemplateFilePaths()).thenReturn(new HashMap<>());
    productService.getExternalDownloadTemplateFilePaths();
    verify(fileStorageService).getExternalDownloadTemplateFilePaths();
  }

  @Test
  public void getItemBasicDetailsTest() {
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    when(xProductFeign.getItemBasicDetails(PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response), null, PRODUCT_ID));
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setSkuCode(ITEM_CODE);
    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemResponse1.setProductItemAttributeValueResponses(
        Collections.singletonList(productItemAttributeValueResponse));
    GdnRestListResponse<ProductItemResponse> gdnRestListResponse =
        new GdnRestListResponse<>(Collections.singletonList(productItemResponse1), null, REQUEST_ID);
    Mockito.when(pcbFeign.getItemDeatilBySkuCodes(any())).thenReturn(gdnRestListResponse);
    productService.getItemBasicDetails(PRODUCT_SKU, true);
    verify(xProductFeign).getItemBasicDetails(PRODUCT_SKU);
    verify(pcbFeign).getItemDeatilBySkuCodes(new SkuCodesRequest(Collections.singletonList(ITEM_CODE)));
  }

  @Test
  public void getItemBasicDetailsTest_fetchAllDetailsFalse() {
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    itemBasicDetailV2Response.setBundleRecipeList(List.of(new BundleRecipeV2Response()));
    when(xProductFeign.getItemBasicDetails(PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response),
            null, REQUEST_ID));
    productService.getItemBasicDetails(PRODUCT_SKU, false);
    verify(xProductFeign).getItemBasicDetails(PRODUCT_SKU);
    Mockito.verifyNoInteractions(pcbFeign);
  }

  @Test
  public void getItemBasicDetailsTestSwitchTrue() {
    ReflectionTestUtils.setField(productService, "validateSellerItemBasicDetails", true);
    ReflectionTestUtils.setField(productService, "itemBasicDetailsValidationRelaxForSellerTypes", StringUtils.EMPTY);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    itemBasicDetailV2Response.setBundleRecipeList(List.of(new BundleRecipeV2Response()));
    itemBasicDetailV2Response.setMerchantCode(BUSINESS_PARTNER_NAME);
    when(xProductFeign.getItemBasicDetails(PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response), null, REQUEST_ID));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESSPARTNER_CODE);
    productService.getItemBasicDetails(PRODUCT_SKU, false);
    verify(xProductFeign).getItemBasicDetails(PRODUCT_SKU);
    verify(mandatoryParameterHelper, times(2)).getBusinessPartnerCode();
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_NAME);
  }

  @Test
  public void getItemBasicDetailsTestSwitchTrueValidSeller() {
    ReflectionTestUtils.setField(productService, "validateSellerItemBasicDetails", true);
    ReflectionTestUtils.setField(productService, "itemBasicDetailsValidationRelaxForSellerTypes", StringUtils.EMPTY);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    itemBasicDetailV2Response.setBundleRecipeList(List.of(new BundleRecipeV2Response()));
    itemBasicDetailV2Response.setMerchantCode(BUSINESSPARTNER_CODE);
    when(xProductFeign.getItemBasicDetails(BUSINESSPARTNER_CODE + PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response), null, REQUEST_ID));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESSPARTNER_CODE);
    productService.getItemBasicDetails(BUSINESSPARTNER_CODE + PRODUCT_SKU, false);
    verify(xProductFeign).getItemBasicDetails(BUSINESSPARTNER_CODE + PRODUCT_SKU);
    verify(mandatoryParameterHelper, times(2)).getBusinessPartnerCode();
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
  }

  @Test
  public void getItemBasicDetailsTestSwitchTrueCmSeller() {
    ReflectionTestUtils.setField(productService, "validateSellerItemBasicDetails", true);
    ReflectionTestUtils.setField(productService, "itemBasicDetailsValidationRelaxForSellerTypes",
        Constants.MERCHANT_TYPE_TD);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    itemBasicDetailV2Response.setBundleRecipeList(List.of(new BundleRecipeV2Response()));
    itemBasicDetailV2Response.setMerchantCode(BUSINESS_PARTNER_NAME);
    when(xProductFeign.getItemBasicDetails(PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response), null, REQUEST_ID));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESSPARTNER_CODE);
    profileResponse.getCompany().setMerchantType(Constants.MERCHANT_TYPE_TD);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    productService.getItemBasicDetails(PRODUCT_SKU, false);
    verify(xProductFeign).getItemBasicDetails(PRODUCT_SKU);
    verify(mandatoryParameterHelper, times(2)).getBusinessPartnerCode();
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_NAME);
  }

  @Test
  public void getItemBasicDetailsTestSwitchTrueCmSellerAuthorized() {
    ReflectionTestUtils.setField(productService, "validateSellerItemBasicDetails", true);
    ReflectionTestUtils.setField(productService, "itemBasicDetailsValidationRelaxForSellerTypes",
        Constants.MERCHANT_TYPE_TD);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    itemBasicDetailV2Response.setBundleRecipeList(List.of(new BundleRecipeV2Response()));
    itemBasicDetailV2Response.setMerchantCode(BUSINESS_PARTNER_NAME);
    when(xProductFeign.getItemBasicDetails(MERCHANT_CODE + PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response), null, REQUEST_ID));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESSPARTNER_CODE);
    profileResponse.getCompany().setMerchantType(Constants.MERCHANT_TYPE_TD);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    productService.getItemBasicDetails(MERCHANT_CODE + PRODUCT_SKU, false);
    verify(xProductFeign).getItemBasicDetails(MERCHANT_CODE + PRODUCT_SKU);
    verify(mandatoryParameterHelper, times(2)).getBusinessPartnerCode();
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_NAME);
  }

  @Test
  void getItemBasicDetailsTestSwitchTrueCmSellerAuthorized2() {
    ReflectionTestUtils.setField(productService, "validateSellerItemBasicDetails", true);
    ReflectionTestUtils.setField(productService, "itemBasicDetailsValidationRelaxForSellerTypes",
        Constants.MERCHANT_TYPE_CM);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    itemBasicDetailV2Response.setBundleRecipeList(List.of(new BundleRecipeV2Response()));
    itemBasicDetailV2Response.setMerchantCode(BUSINESS_PARTNER_NAME);
    when(xProductFeign.getItemBasicDetails(MERCHANT_CODE + PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response), null, REQUEST_ID));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESSPARTNER_CODE);
    profileResponse.getCompany().setMerchantType(Constants.MERCHANT_TYPE_TD);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> productService.getItemBasicDetails(BUSINESSPARTNER_CODE + PRODUCT_SKU, false));
    } finally {
      verify(xProductFeign).getItemBasicDetails(BUSINESSPARTNER_CODE + PRODUCT_SKU);
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  void getItemBasicDetailsTestSwitchTrueCmSellerNotAuthorized() {
    ReflectionTestUtils.setField(productService, "validateSellerItemBasicDetails", true);
    ReflectionTestUtils.setField(productService, "itemBasicDetailsValidationRelaxForSellerTypes",
        Constants.MERCHANT_TYPE_TD);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    itemBasicDetailV2Response.setBundleRecipeList(List.of(new BundleRecipeV2Response()));
    itemBasicDetailV2Response.setMerchantCode(BUSINESS_PARTNER_NAME);
    when(xProductFeign.getItemBasicDetails(MERCHANT_CODE + PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response), null, REQUEST_ID));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESSPARTNER_CODE);
    profileResponse.getCompany().setMerchantType(Constants.MERCHANT_TYPE_TC);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    try {
      Assertions.assertThrows(UnauthorizedException.class,
          () -> productService.getItemBasicDetails(MERCHANT_CODE + PRODUCT_SKU, false));
    } finally {
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  void getItemBasicDetailsTestSwitchTrueCmSellerNotAuthorizedForTheFetchedData() {
    ReflectionTestUtils.setField(productService, "validateSellerItemBasicDetails", true);
    ReflectionTestUtils.setField(productService, "itemBasicDetailsValidationRelaxForSellerTypes",
        Constants.MERCHANT_TYPE_TD);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    itemBasicDetailV2Response.setBundleRecipeList(List.of(new BundleRecipeV2Response()));
    itemBasicDetailV2Response.setMerchantCode(BUSINESS_PARTNER_NAME);
    when(xProductFeign.getItemBasicDetails(MERCHANT_CODE + PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response), null, REQUEST_ID));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESSPARTNER_CODE);
    profileResponse.getCompany().setMerchantType(Constants.MERCHANT_TYPE_TD);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    ProfileResponse profileResponse1 = new ProfileResponse();
    profileResponse1.setCompany(new CompanyDTO());
    profileResponse1.getCompany().setMerchantType(Constants.MERCHANT_TYPE_RC);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_NAME))
        .thenReturn(profileResponse1);
    try {
      Assertions.assertThrows(com.gdn.partners.core.security.exception.UnauthorizedException.class,
          () -> productService.getItemBasicDetails(MERCHANT_CODE + PRODUCT_SKU, false));
    } finally {
      verify(xProductFeign).getItemBasicDetails(MERCHANT_CODE + PRODUCT_SKU);
      verify(mandatoryParameterHelper, times(2)).getBusinessPartnerCode();
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_NAME);
    }
  }

  @Test
  public void checkIfFetchedDataIsAuthorizedToBeReturnedTest() {
    productService.checkIfFetchedDataIsAuthorizedToBeReturned(PRODUCT_SKU, new GdnRestListResponse<>());
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getItemBasicDetailsByItemSkusTest() throws Exception {
    ReflectionTestUtils.setField(productService, "maxAllowedItemSkus", 5);
    CategoryWebResponse categoryWebResponse = new CategoryWebResponse();
    categoryWebResponse.setCategoryCode(CATEGORY_CODE);
    categoryWebResponse.setName(CATEGORY_NAME_1);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    itemBasicDetailV2Response.setItemSku(ITEM_SKU_1);
    itemBasicDetailV2Response.setCategoryCode(CATEGORY_CODE);
    when(xProductFeign.getItemBasicDetails(Boolean.TRUE, new SimpleListStringRequest(itemSkus)))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response), null, REQUEST_ID));
    when(categoryService.getCategoriesByCategoryCodes(Collections.singletonList(CATEGORY_CODE)))
        .thenReturn(Collections.singletonList(categoryWebResponse));
    List<ItemDetailWebResponse> responses = productService.getItemBasicDetails(itemSkus, Boolean.TRUE);
    verify(xProductFeign).getItemBasicDetails(Boolean.TRUE, new SimpleListStringRequest(itemSkus));
    verify(categoryService).getCategoriesByCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    Assertions.assertEquals(CATEGORY_NAME_1, responses.get(0).getCategoryName());
  }

  @Test
  void getItemBasicDetailsByItemSkusForDifferentBusinessPartnerTest() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn("BUSINESSPARTNER_CODE_ITEM_SKU");
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", true);
    ReflectionTestUtils.setField(productService, "maxAllowedItemSkus", 5);
    itemSkus.add("notItemSku");
      Assertions.assertThrows(ValidationException.class,
          () -> productService.getItemBasicDetails(itemSkus, Boolean.TRUE));
  }

  @Test
  public void getItemBasicDetailsByItemSkusForDifferentBusinessPartner2Test() throws Exception {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn("BUSINESSPARTNER_CODE_ITEM_SKU");
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", true);
    ReflectionTestUtils.setField(productService, "maxAllowedItemSkus", 5);
    CategoryWebResponse categoryWebResponse = new CategoryWebResponse();
    categoryWebResponse.setCategoryCode(CATEGORY_CODE);
    categoryWebResponse.setName(CATEGORY_NAME_1);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setProductSku(PRODUCT_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_CODE);
    itemBasicDetailV2Response.setMainImageUrl(IMAGE_URL);
    itemBasicDetailV2Response.setItemSku(ITEM_SKU_1);
    itemBasicDetailV2Response.setCategoryCode(CATEGORY_CODE);
    itemSkus.add("BUSINESSPARTNER_CODE_ITEM_SKU");
    when(xProductFeign.getItemBasicDetails(Boolean.TRUE, new SimpleListStringRequest(itemSkus)))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(itemBasicDetailV2Response), null, REQUEST_ID));
    when(categoryService.getCategoriesByCategoryCodes(Collections.singletonList(CATEGORY_CODE)))
        .thenReturn(Collections.singletonList(categoryWebResponse));
    List<ItemDetailWebResponse> responses = productService.getItemBasicDetails(itemSkus, Boolean.TRUE);
    verify(xProductFeign).getItemBasicDetails(Boolean.TRUE, new SimpleListStringRequest(itemSkus));
    verify(categoryService).getCategoriesByCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    Assertions.assertEquals(CATEGORY_NAME_1, responses.get(0).getCategoryName());
  }

  @Test
  void getItemBasicDetailsByItemSkus_ExceptionTest() throws Exception {
    ReflectionTestUtils.setField(productService, "maxAllowedItemSkus", 1);
    itemSkus.add(ITEM_SKU);
    when(xProductFeign.getItemBasicDetails(Boolean.TRUE, new SimpleListStringRequest(itemSkus))).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(new ItemBasicDetailV2Response()), null, REQUEST_ID));
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> productService.getItemBasicDetails(itemSkus, Boolean.TRUE));
  }

  @Test
  void getItemBasicDetailsByItemSkus_ItemSkuLimitExceedExceptionTest() throws Exception {
    ReflectionTestUtils.setField(productService, "maxAllowedItemSkus", 5);
    when(xProductFeign.getItemBasicDetails(Boolean.TRUE, new SimpleListStringRequest(itemSkus)))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> productService.getItemBasicDetails(itemSkus, Boolean.TRUE));
    } finally {
      verify(xProductFeign).getItemBasicDetails(Boolean.TRUE, new SimpleListStringRequest(itemSkus));
    }
  }

  @Test
  public void getItemL5Details_test() {
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(PRODUCT_SKU);
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", true);
    when(xProductFeign.getItemL5Details(PAGE, SIZE, PRODUCT_SKU, false, false,
        new SimpleListStringRequest())).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(new ItemL5ListingResponse()),
            new PageMetaData(PAGE, SIZE, 0), PRODUCT_SKU));
    productService.getItemL5Details(PRODUCT_SKU, PAGE, SIZE, false, false);
    verify(xProductFeign)
        .getItemL5Details(PAGE, SIZE, PRODUCT_SKU, false, false, new SimpleListStringRequest());
  }

  @Test
  public void getItemL5DetailsForInValidBusinessParnterCodetest() {
    try {
      when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn("DR6-44661");
      ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", true);
      productService.getItemL5Details("DR6-44662-0001", PAGE, SIZE, false, false);
    } catch (ValidationException validationException) {
      Assertions.assertEquals("Accessing details of incorrect business partner",
          validationException.getMessage());
    }
  }


  @Test
  public void getItemL5Details_PageNulltest() {
    when(xProductFeign.getItemL5Details(null, SIZE, PRODUCT_SKU, false, false,
        new SimpleListStringRequest())).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(new ItemL5ListingResponse()),
            new PageMetaData(PAGE, SIZE, 0), PRODUCT_SKU));
    productService.getItemL5Details(PRODUCT_SKU, null, SIZE, false, false);
    verify(xProductFeign)
        .getItemL5Details(null, SIZE, PRODUCT_SKU, false, false, new SimpleListStringRequest());
  }

  @Test
  public void getItemL5Details_ExceptionTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> productService.getItemL5Details("", PAGE, SIZE, false, false));
  }

  @Test
  public void getWarehouseStockByItemSkusAndWarehouseCodeTest() {
    WarehouseInventoryStockInfoDTO stockInfoDTO = new WarehouseInventoryStockInfoDTO();
    stockInfoDTO.setAvailableStock(12);
    stockInfoDTO.setWarehouseCode(WAREHOUSE_CODE);
    WarehouseInventoryDetailResponseDTO inventoryResponse = new WarehouseInventoryDetailResponseDTO();
    inventoryResponse.setWarehouseItemSku(ITEM_SKU);
    inventoryResponse.setWarehouseInventoryStockInfos(Collections.singletonList(stockInfoDTO));
    Mockito.when(xInventoryFeign.findByWebItemSkusAndWarehouseCodes(any())).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(inventoryResponse), new PageMetaData(PAGE, SIZE, 0),
            REQUEST_ID));
    List<InventoryWarehouseStockWebResponse> responses =
        productService.getWarehouseStockByItemSkusAndWarehouseCode(WAREHOUSE_CODE, Collections.singletonList(ITEM_SKU));
    verify(xInventoryFeign).findByWebItemSkusAndWarehouseCodes(any());
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void getStockForNonDistributedWareHouseByItemSkusAndWareHouseCodes() {
    ReflectionTestUtils.setField(productService, "getStockForNonDistributedWareHouse", true);
    WarehouseInventoryStockInfoDTO stockInfoDTO = new WarehouseInventoryStockInfoDTO();
    stockInfoDTO.setAvailableStock(12);
    stockInfoDTO.setWarehouseCode(WAREHOUSE_CODE);
    WarehouseInventoryDetailResponseDTO inventoryResponse = new WarehouseInventoryDetailResponseDTO();
    inventoryResponse.setWarehouseItemSku(ITEM_SKU);
    inventoryResponse.setWarehouseInventoryStockInfos(Collections.singletonList(stockInfoDTO));
    Mockito.when(xInventoryFeign.getStockForNonDistributedWareHouseByItemSkusAndWareHouseCodes(
        anyBoolean(),
        any())).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(inventoryResponse), new PageMetaData(PAGE, SIZE, 0),
            REQUEST_ID));
    List<InventoryWarehouseStockWebResponse> responses =
        productService.getWarehouseStockByItemSkusAndWarehouseCode(WAREHOUSE_CODE, Collections.singletonList(ITEM_SKU));
    verify(xInventoryFeign)
        .getStockForNonDistributedWareHouseByItemSkusAndWareHouseCodes(anyBoolean(), any());
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void testDownloadTemplates() throws Exception {
    Blob mockBlob = mock(Blob.class);
    XSSFWorkbook workbook = new XSSFWorkbook();
    XSSFSheet sheet = workbook.createSheet("Sheet1");
    XSSFSheet sheet1 = workbook.createSheet("Sheet2");
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    workbook.write(bos);
    byte[] excelData = bos.toByteArray();
    bos.close();
    when(mockBlob.getContent()).thenReturn(excelData);
    Mockito.when(gcsConfig.downloadFile(any(), any())).thenReturn(mockBlob);
    MasterWarehouseListWebResponse warehouseListWebResponse = new MasterWarehouseListWebResponse();
    warehouseListWebResponse.setWarehouseCode("warehouseCode");
    List<MasterWarehouseListWebResponse> dummyResponseList = Arrays.asList(warehouseListWebResponse);
    GdnRestListResponse<MasterWarehouseListWebResponse> dummyResponse = new GdnRestListResponse<>();
    dummyResponse.setContent(dummyResponseList);
    dummyResponse.setSuccess(true);
    when(productAssemblyService.getMasterWarehouseListResponse()).thenReturn(dummyResponse);
    productService.downloadWorkOrderTemplates("TRANSFER_REQUEST", httpServletResponse);
    verify(productAssemblyService).getMasterWarehouseListResponse();
  }

  @Test
  public void testDownloadTemplatesForAssemblyRequestTest() throws Exception {
    Blob mockBlob = mock(Blob.class);
    XSSFWorkbook workbook = new XSSFWorkbook();
    XSSFSheet sheet = workbook.createSheet("Sheet1");
    XSSFSheet sheet1 = workbook.createSheet("Sheet2");
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    workbook.write(bos);
    byte[] excelData = bos.toByteArray();
    bos.close();
    when(mockBlob.getContent()).thenReturn(excelData);
    Mockito.when(gcsConfig.downloadFile(any(), any())).thenReturn(mockBlob);
    MasterWarehouseListWebResponse warehouseListWebResponse = new MasterWarehouseListWebResponse();
    warehouseListWebResponse.setWarehouseCode("warehouseCode");
    List<MasterWarehouseListWebResponse> dummyResponseList = Arrays.asList(warehouseListWebResponse);
    GdnRestListResponse<MasterWarehouseListWebResponse> dummyResponse = new GdnRestListResponse<>();
    dummyResponse.setContent(dummyResponseList);
    dummyResponse.setSuccess(true);
    when(productAssemblyService.getMasterWarehouseListResponse()).thenReturn(dummyResponse);
    productService.downloadWorkOrderTemplates("ASSEMBLY_REQUEST", httpServletResponse);
    verify(productAssemblyService).getMasterWarehouseListResponse();
  }

  @Test
  public void downloadProductTemplateBfbSellerGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", true);
    when(gcsProperties.isEnabled()).thenReturn(true);
    when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-upload-template");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    profileResponse.getCompany().setInternationalFlag(false);
    profileResponse.getCompany().setSalesChannel(Collections.singletonList(Constants.B2B_SELLER_CHANNEL));
    profileResponse.setBopisFlag(true);
    profileResponse.setBigProductFlag(true);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of(PICKUP_POINT_CODE));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
      .thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE)))
      .thenReturn(pickupPointResponseList);
    when(gcsConfig.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(blob);
    when(this.categoryService.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponse);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID2)).thenReturn(attributeResponse2);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.productService.downloadProductTemplate(USERNAME, BUSINESSPARTNER_CODE, CATEGORY_ID, httpServletResponse, false);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE));
    verify(categoryService).getCategoryDetail(CATEGORY_ID);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
  }

  @Test
  public void downloadProductTemplateInternationalBfbSellerGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", true);
    when(gcsProperties.isEnabled()).thenReturn(true);
    when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-template-upload-English");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    profileResponse.getCompany().setInternationalFlag(true);
    profileResponse.getCompany().setSalesChannel(Collections.singletonList(Constants.B2B_SELLER_CHANNEL));
    profileResponse.setBigProductFlag(false);
    profileResponse.setBopisFlag(false);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of(PICKUP_POINT_CODE));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
      .thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE)))
      .thenReturn(pickupPointResponseList);
    when(gcsConfig.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(blob);
    when(this.categoryService.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponse);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID2)).thenReturn(attributeResponse2);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.productService.downloadProductTemplate(USERNAME, BUSINESSPARTNER_CODE, CATEGORY_ID, httpServletResponse, false);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE));
    verify(categoryService).getCategoryDetail(CATEGORY_ID);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
  }

  @Test
  public void downloadProductTemplateBpBopisDisableTest() throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", false);
    when(gcsProperties.isEnabled()).thenReturn(true);
    when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-template-upload-English");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    profileResponse.getCompany().setInternationalFlag(true);
    profileResponse.getCompany().setSalesChannel(Collections.singletonList(Constants.B2B_SELLER_CHANNEL));
    profileResponse.setBigProductFlag(false);
    profileResponse.setBopisFlag(false);
    when(this.userPicService.shouldRestrictAccess(profileResponse)).thenReturn(true);
    when(this.mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of(PICKUP_POINT_CODE));
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
      .thenReturn(profileResponse);
    when(this.businessPartnerService.getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE)))
      .thenReturn(pickupPointResponseList);
    when(gcsConfig.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(blob);
    when(this.categoryService.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponse);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID2)).thenReturn(attributeResponse2);
    when(this.categoryService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.productService.downloadProductTemplate(USERNAME, BUSINESSPARTNER_CODE, CATEGORY_ID, httpServletResponse, false);
    verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(this.businessPartnerService).getPickupPointsForBusinessPartner(BUSINESSPARTNER_CODE, Set.of(PICKUP_POINT_CODE));
    verify(categoryService).getCategoryDetail(CATEGORY_ID);
    verify(this.userPicService).shouldRestrictAccess(profileResponse);
    verify(this.mandatoryParameterHelper).getPickupPoints();
  }

  @Test
  public void createProduct_whenBrandApprovedAndPBPClientExceptionBpBopisEnableTest() throws Exception {
    ReflectionTestUtils.setField(productService, "bpBopisRestrictionEnabled", true);
    String[] accessibility = {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
    CreateProductResponse response = null;
    productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS_APPROVED);
    when(pbpFeign.createProduct(productCreationRequest, FLOW_TYPE)).thenReturn(null);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
      .thenReturn(profileResponse);
    try {
      response = this.productService.createProduct(USERNAME, productCreationRequest, BUSINESSPARTNER_CODE, FLOW_TYPE);
    } catch (ClientException ex) {
    } finally {
      verify(userPicService).validateUserPicPickupPointsForBusinessPartner(
          productCreationRequest.getProductItemRequests().stream().flatMap(
                  productItemCreationRequest -> productItemCreationRequest.getPickupPoints().stream())
              .map(PickupPointCreateRequest::getPickupPointId).collect(Collectors.toSet()),
          profileResponse);
      verify(pbpFeign).createProduct(productCreationRequest, FLOW_TYPE);
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      assertNull(response);
    }
    }
  }

  @Test
  public void fetchAppealProductConfigTst() {
    AppealProductConfigResponse appealProductConfigResponse = new AppealProductConfigResponse();
    when(pbpFeign.getAppealProductConfig(STORE_ID, REQUEST_ID, BUSINESSPARTNER_CODE))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, appealProductConfigResponse, null));
    productService.fetchAppealProductConfig(STORE_ID, REQUEST_ID, BUSINESSPARTNER_CODE);
    verify(pbpFeign).getAppealProductConfig(STORE_ID, REQUEST_ID, BUSINESSPARTNER_CODE);
  }

  @Test
  public void submitEvidenceTest() {
    SubmitEvidenceIPRRequest submitEvidenceIPRRequest = SubmitEvidenceIPRRequest.builder().build();
    when(pdtFeign.submitEvidence(REQUEST_ID, submitEvidenceIPRRequest))
        .thenReturn(gdnBaseRestResponse);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    productService.submitEvidenceForIPR(submitEvidenceIPRRequest);
    verify(pdtFeign).submitEvidence(REQUEST_ID, submitEvidenceIPRRequest);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void getUpcStatus() {
    UpcStatusWebRequest upcStatusWebRequest = new UpcStatusWebRequest();
    UpcStatusRequest upcStatusRequest = new UpcStatusRequest();
    List<UpcStatusResponse> upcStatusResponses = new ArrayList<>();
    when(xProductFeign.getUpcStatus(upcStatusRequest))
        .thenReturn(new GdnRestListResponse<>(upcStatusResponses, pageMetaData, REQUEST_ID));
    productService.getUpcStatus(upcStatusWebRequest);
    verify(xProductFeign).getUpcStatus(upcStatusRequest);
  }

  @Test
  public void republishProductsToAgpTest() {
    when(xProductFeign.republishProductsToAgp(Collections.singletonList(PRODUCT_ID)))
        .thenReturn(new GdnBaseRestResponse(true));
    productService.republishProductToAgp(PRODUCT_ID);
    verify(xProductFeign).republishProductsToAgp(Collections.singletonList(PRODUCT_ID));
  }

  @Test
  public void republishItemsToAgpTest() {
    when(xProductFeign.republishItemsToAgp(Collections.singletonList(PRODUCT_ID)))
        .thenReturn(new GdnBaseRestResponse(true));
    productService.republishItemToAgp(PRODUCT_ID);
    verify(xProductFeign).republishItemsToAgp(Collections.singletonList(PRODUCT_ID));
  }

  @Test
  public void republishItemPickupPointToAgpTest() {
    when(xProductFeign.republishItemPickupPointToAgp(true,
        Collections.singletonList(new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE))))
        .thenReturn(new GdnBaseRestResponse(true));
    productService.republishItemPickupPointToAgp(ITEM_SKU, PICKUP_POINT_CODE, true);
    verify(xProductFeign).republishItemPickupPointToAgp(true,
        Collections.singletonList(new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE)));
  }

  @Test
  public void getProductListForReelsTest() {
    InventoryStockInfoDTO inventoryStockInfoDTO = new InventoryStockInfoDTO();
    inventoryStockInfoDTO.setWebProductSku(PRODUCT_SKU);
    inventoryStockInfoDTO.setTotalStock(10);
    ReelProductDetailWebResponse response = new ReelProductDetailWebResponse();
    response.setProductName(PRODUCT_NAME);
    response.setProductSku(PRODUCT_SKU);
    response.setInStock(true);
    ReelProductListingWebRequest reelProductListingWebRequest = new ReelProductListingWebRequest();
    reelProductListingWebRequest.setMerchantCode(MERCHANT_CODE);
    reelProductListingWebRequest.setCategoryCodes(List.of(CATEGORY_CODE));
    when(xProductFeign.getReelProductList(0, 10, reelProductListingWebRequest)).thenReturn(
        new GdnRestListResponse<>(List.of(response), new PageMetaData(0, 10, 0), REQUEST_ID));
    productService.getProductListForReels(reelProductListingWebRequest, 0, 10);
    verify(xProductFeign).getReelProductList(0, 10, reelProductListingWebRequest);
  }

  @Test
  public void getProductListForReelsWithTradingProductTrueTest() {
    ReelProductDetailWebResponse response = new ReelProductDetailWebResponse();
    response.setProductName(PRODUCT_NAME);
    response.setProductSku(PRODUCT_SKU_NEW);
    response.setInStock(true);
    ReelProductListingWebRequest reelProductListingWebRequest = new ReelProductListingWebRequest();
    reelProductListingWebRequest.setTradingProduct(true);
    Map<String, String> bpCodeAndNameMap = new HashMap<>();
    bpCodeAndNameMap.put(BUSINESSPARTNER_CODE, BUSINESS_PARTNER_NAME);
    when(
        businessPartnerService.getBusinessPartnerDetailsByList(Mockito.anyInt(), Mockito.anyInt(),
            Mockito.any())).thenReturn(new SimpleMapStringResponse(bpCodeAndNameMap));
    when(xProductFeign.getReelProductList(0, 10, reelProductListingWebRequest)).thenReturn(
        new GdnRestListResponse<>(List.of(response), new PageMetaData(0, 10, 0), REQUEST_ID));
    productService.getProductListForReels(reelProductListingWebRequest, 0, 10);
    verify(xProductFeign).getReelProductList(0, 10, reelProductListingWebRequest);
    verify(businessPartnerService)
        .getBusinessPartnerDetailsByList(Mockito.anyInt(), Mockito.anyInt(), Mockito.any());
  }

  @Test
  public void getProductListForReelsWithTradingProductTrueWithEmptyResultTest() {
    ReelProductListingWebRequest reelProductListingWebRequest = new ReelProductListingWebRequest();
    reelProductListingWebRequest.setTradingProduct(true);
    Map<String, String> bpCodeAndNameMap = new HashMap<>();
    bpCodeAndNameMap.put(BUSINESSPARTNER_CODE, BUSINESS_PARTNER_NAME);
    Mockito.when(
        businessPartnerService.getBusinessPartnerDetailsByList(Mockito.anyInt(), Mockito.anyInt(),
            Mockito.any())).thenReturn(new SimpleMapStringResponse(bpCodeAndNameMap));
    Mockito.when(xProductFeign.getReelProductList(0, 10, reelProductListingWebRequest)).thenReturn(
        new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(0, 10, 0), REQUEST_ID));
    productService.getProductListForReels(reelProductListingWebRequest, 0, 10);
    Mockito.verify(xProductFeign).getReelProductList(0, 10, reelProductListingWebRequest);
  }

  @Test
  public void getProductListForReelsExceptionTest() {
    InventoryStockInfoDTO inventoryStockInfoDTO = new InventoryStockInfoDTO();
    inventoryStockInfoDTO.setWebProductSku(PRODUCT_SKU);
    inventoryStockInfoDTO.setTotalStock(10);
    ReelProductDetailWebResponse response = new ReelProductDetailWebResponse();
    response.setProductName(PRODUCT_NAME);
    response.setProductSku(PRODUCT_SKU);
    response.setInStock(true);
    ReelProductListingWebRequest reelProductListingWebRequest = new ReelProductListingWebRequest();
    reelProductListingWebRequest.setMerchantCode(MERCHANT_CODE);
    reelProductListingWebRequest.setCategoryCodes(List.of(CATEGORY_CODE));
    when(xProductFeign.getReelProductList(0, 10, reelProductListingWebRequest)).thenReturn(
        new GdnRestListResponse<>(List.of(response), new PageMetaData(0, 10, 0), REQUEST_ID));
    productService.getProductListForReels(reelProductListingWebRequest, 0, 10);
    verify(xProductFeign).getReelProductList(0, 10, reelProductListingWebRequest);
  }

  @Test
  public void  getProductBasicDetailsByProductSkusTest() throws Exception {
    when(xProductFeign.getProductBasicDetails(
        new SimpleListStringRequest(List.of(PRODUCT_SKU)))).thenReturn(
            new GdnRestListResponse<>(Collections.singletonList(new ProductBasicWebResponse()),
                null, REQUEST_ID));
    productService.getProductBasicDetailsByProductSkus(Collections.singletonList(PRODUCT_SKU));
    verify(xProductFeign)
        .getProductBasicDetails(new SimpleListStringRequest(List.of(PRODUCT_SKU)));
  }

  @Test
  public void downloadAllProductWithFiltersBasicInfoTest() {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      productSummaryWebRequest.setPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
      when(
          this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
          profileResponse);
      this.productService.downloadAllProductBasicInfo(USERNAME, BUSINESSPARTNER_CODE,
          productSummaryWebRequest);
    } finally {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      verify(kafkaProducer).send(Mockito.eq(kafkaTopicProperties.getBulkDownloadAllEvent()),
          Mockito.eq(BUSINESSPARTNER_CODE),
          productBasicInfoDownloadRequestArgumentCaptor.capture());
      verify(kafkaTopicProperties, times(2)).getBulkDownloadAllEvent();
      assertEquals(BulkProcessEntity.PRODUCT_BASIC_INFO,
          productBasicInfoDownloadRequestArgumentCaptor.getValue().getBulkProcessEntity());
      assertEquals(Collections.singletonList(PICKUP_POINT_CODE),
          productBasicInfoDownloadRequestArgumentCaptor.getValue().getProductSummaryRequest()
              .getPickupPointCodes());
    }
  }

  @Test
  public void downloadAllProductExceptionNullTest() {
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(null);
    try {
      this.productService.downloadAllProductBasicInfo(USERNAME, BUSINESSPARTNER_CODE,
          productSummaryWebRequest);
    } catch (ApplicationRuntimeException e) {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      assertEquals(ErrorCategory.VALIDATION.getMessage(), e.getErrorCodes().getMessage());
    }
  }

  @Test
  public void downloadAllProductExceptionInactiveTest() {
    profileResponse.setMerchantStatus(INACTIVE);
    when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    try {
      this.productService.downloadAllProductBasicInfo(USERNAME, BUSINESSPARTNER_CODE,
          productSummaryWebRequest);
    } catch (ApplicationRuntimeException e) {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      assertEquals(ErrorCategory.VALIDATION.getMessage(), e.getErrorCodes().getMessage());
    }
  }

  @Test
  void reindexBrandCollectionTest() {
    when(pcbFeign.reindexBrandCollection(BRAND_CODE)).thenReturn(new GdnBaseRestResponse(true));
    productService.reindexBrandCollection(BRAND_CODE);
    Mockito.verify(pcbFeign).reindexBrandCollection(BRAND_CODE);
  }
  @Test
  public void testDownloadAllProductWithEAN_Success() {
    String username = "user";
    boolean isOnlyExternalUser = false;
    String businessPartnerCode = "BP001";
    ProductSummaryWebRequest request = new ProductSummaryWebRequest();
    request.setMerchantCode(businessPartnerCode);

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMerchantStatus("ACTIVE");
    CompanyDTO company = new CompanyDTO();
    company.setEmail("test@example.com");
    profileResponse.setCompany(company);

    when(businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode)).thenReturn(profileResponse);
    when(kafkaTopicProperties.getBulkDownloadAllEvent()).thenReturn("topic");

    try (MockedStatic<RequestHelper> requestHelperMockedStatic = mockStatic(RequestHelper.class)) {
      Map<String, Boolean> privilegeMap = new HashMap<>();
      requestHelperMockedStatic.when(() -> RequestHelper.getReadAccessibilities(any(), anyBoolean()))
          .thenReturn(privilegeMap);
      
      ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
      requestHelperMockedStatic.when(() -> RequestHelper.toProductSummaryRequest(any(ProductSummaryWebRequest.class)))
          .thenReturn(productSummaryRequest);

      productService.downloadAllProductWithEAN(username, isOnlyExternalUser, businessPartnerCode, request);
      verify(kafkaProducer).send(eq("topic"), any(ProductDownloadEANRequest.class));
      verify(businessPartnerService).filterByBusinessPartnerCode(Mockito.anyString());
      verify(kafkaTopicProperties).getBulkDownloadAllEvent();
    }
  }

  @Test
  public void testDownloadAllProductWithEAN_ValidationException() {
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", true);
    String username = "user";
    boolean isOnlyExternalUser = false;
    String businessPartnerCode = "BP001";
    ProductSummaryWebRequest request = new ProductSummaryWebRequest();
    request.setMerchantCode("BP002"); // Mismatch

    Assertions.assertThrows(ValidationException.class, () -> {
      productService.downloadAllProductWithEAN(username, isOnlyExternalUser, businessPartnerCode, request);
    });
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", false);
  }

  @Test
  public void testDownloadAllProductWithEAN_InactiveMerchant() {
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", true);
    String username = "user";
    boolean isOnlyExternalUser = false;
    String businessPartnerCode = "BP001";
    ProductSummaryWebRequest request = new ProductSummaryWebRequest();
    request.setMerchantCode(businessPartnerCode);

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMerchantStatus("INACTIVE");

    when(businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode)).thenReturn(profileResponse);

    Assertions.assertThrows(RuntimeException.class, () -> {
      productService.downloadAllProductWithEAN(username, isOnlyExternalUser, businessPartnerCode, request);
    });
    verify(businessPartnerService).filterByBusinessPartnerCode(Mockito.anyString());
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", false);
  }

  @Test
  public void testDownloadAllProductWithEAN_Profile_Response_null() {
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", true);
    String username = "user";
    boolean isOnlyExternalUser = false;
    String businessPartnerCode = "BP001";
    ProductSummaryWebRequest request = new ProductSummaryWebRequest();
    request.setMerchantCode(businessPartnerCode);



    when(businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode)).thenReturn(null);

    Assertions.assertThrows(RuntimeException.class, () -> {
      productService.downloadAllProductWithEAN(username, isOnlyExternalUser, businessPartnerCode, request);
    });
    verify(businessPartnerService).filterByBusinessPartnerCode(Mockito.anyString());
    ReflectionTestUtils.setField(productService, "validateBusinessPartnerCodeForSecurityEnabled", false);
  }
}
