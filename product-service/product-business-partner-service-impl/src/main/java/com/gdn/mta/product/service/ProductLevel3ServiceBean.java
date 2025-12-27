package com.gdn.mta.product.service;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;
import static com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE;
import static com.gdn.partners.pbp.commons.constants.Constants.ACTIVE;
import static com.gdn.partners.pbp.commons.constants.Constants.DEFAULT;
import static com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_REQUEST_ID;
import static com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_STORE_ID;
import static com.gdn.partners.pbp.commons.constants.Constants.DELIMITER;
import static com.gdn.partners.pbp.commons.constants.Constants.DELIMITER_DASH;
import static com.gdn.partners.pbp.commons.constants.EditedReviewTypeConstants.CONTENT_REFRESH;
import static com.gdn.partners.pbp.commons.constants.SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE;
import static java.util.Optional.of;
import static java.util.Optional.ofNullable;
import java.util.function.Predicate;

import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.DimensionRefreshRequest;
import com.gda.mta.product.dto.DistributionInfoRequest;
import com.gda.mta.product.dto.ItemImageUpdateRequestPCB;
import com.gda.mta.product.dto.NeedCorrectionProductActivationRequest;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.L5HistoryDTO;
import com.gda.mta.product.dto.ProductCollectionDTO;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductEditContext;
import com.gda.mta.product.dto.ProductItemImageHistoryDTO;
import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.mta.product.util.ProductChangeUtil;
import com.gdn.partners.pbp.dao.UpdatePoDateByL3RequestDTO;
import com.gda.mta.product.dto.response.InProgressProductsBySizeChartCodeResponse;
import com.gdn.mta.product.commons.constant.DimensionHolder;
import com.gdn.mta.product.enums.AddDeleteVariantStatus;
import com.gdn.mta.product.util.ValueTypeUtil;
import com.gdn.mta.product.util.validator.ValidationUtil;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.util.ProductLevel3InventoryUtil;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.google.common.collect.ImmutableMap;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.B2BFields;
import com.gda.mta.product.dto.BrandAndCategoryItemSummaryRequest;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.CopyImageEditRequest;
import com.gda.mta.product.dto.DeletedProductItems;
import com.gda.mta.product.dto.EditFlagChangesDTO;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.EditedResizeAndImagesUpdateStatusResponse;
import com.gda.mta.product.dto.EditedResizeImagesResponse;
import com.gda.mta.product.dto.FbbAndCncDataChangeDto;
import com.gda.mta.product.dto.ItemImageEditRequest;
import com.gda.mta.product.dto.ItemNotesDto;
import com.gda.mta.product.dto.ItemPickupPointDto;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ItemSkuPickupPointSyncStockDto;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.PickupPointRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gda.mta.product.dto.PostLiveFlagsDTO;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemHistoryForWholesale;
import com.gda.mta.product.dto.ProductItemLevel3LogisticResponse;
import com.gda.mta.product.dto.ProductItemLogisticsRequest;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.ProductItemsImageUpdateRequest;
import com.gda.mta.product.dto.ProductL3SummaryRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.ProductLevel3DTO;
import com.gda.mta.product.dto.ProductLevel3ImageResponse;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditRequest;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigStockRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductScoreResponse;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.response.AgpSimpleQueryResponse;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse;
import com.gda.mta.product.dto.response.InternalProductHistoryEventModel;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.PreOrderResponse;
import com.gda.mta.product.dto.response.ProductBusinessPartnerAndItemViewConfigDto;
import com.gda.mta.product.dto.response.ProductItemNameResponse;
import com.gda.mta.product.dto.response.ProductL3SummaryResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.GdnBaseEntity;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.domain.event.modal.EditedImageResizeEvent;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.product.commons.constant.ProductLevel3InactiveSummaryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3SummaryCriteria;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.BulkDownloadProductLevel3Summary;
import com.gdn.mta.product.entity.Cogs;
import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.entity.PickupPointCodeResponse;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductItemLevel3Order;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductItemWholesalePriceVo;
import com.gdn.mta.product.entity.ProductItemsCogs;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductLevel3Dashboard;
import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.ProductLevel3Order;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetails;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetailsImage;
import com.gdn.mta.product.entity.ProductLevel3UpdateSummary;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.entity.ProductScore;
import com.gdn.mta.product.entity.ProductSuspensionHistory;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.ProductWarehouse;
import com.gdn.mta.product.entity.UniquePickupPointCodeResponse;
import com.gdn.mta.product.entity.UsageStatus;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.ExceptionMsg;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.mta.product.enums.SuspensionStatus;
import com.gdn.mta.product.repository.AttributeRepository;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.PickupPointRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductItemWholesalePriceRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductStockAlertRepository;
import com.gdn.mta.product.repository.ProductSuspensionHistoryRepository;
import com.gdn.mta.product.repository.SolrActiveProductCollectionRepository;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.service.converter.UpdateProductItemLevel3ModelConverter;
import com.gdn.mta.product.service.exception.ApiIncorrectInputDataException;
import com.gdn.mta.product.service.exception.InvalidDataStateException;
import com.gdn.mta.product.service.exception.ProductInNeedCorrectionException;
import com.gdn.mta.product.service.exception.ProductInReviewException;
import com.gdn.mta.product.service.exception.ProductRejectedException;
import com.gdn.mta.product.service.generator.GeneratorService;
import com.gdn.mta.product.service.partners.pbp.distributiontask.PredefinedAttributeAllowedValueService;
import com.gdn.mta.product.service.solr.SolrActiveProductCollectionServiceBean;
import com.gdn.mta.product.service.util.ApproveProductUtils;
import com.gdn.mta.product.service.util.MapperUtil;
import com.gdn.mta.product.service.util.WholesaleValidationUtil;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.mta.product.valueobject.EstimateItemPriceDTO;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilterDetails;
import com.gdn.mta.product.valueobject.SimpleMasterProductUpdateRequestDTO;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.mta.product.valueobject.UpdateProductItemLevel3Model;
import com.gdn.partners.pbp.calendar.CalendarService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.EditedReviewTypeConstants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorState;
import com.gdn.partners.pbp.helper.ProductItemCreationRequestHelper;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.model.vo.CacheKeys;
import com.gdn.partners.pbp.outbound.AGPQuery.AGPQueryFeign;
import com.gdn.partners.pbp.outbound.campaign.CampaignOutbound;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.outbound.productAnalytics.ProductAnalyticsOutbound;
import com.gdn.partners.pbp.outbound.productPricing.ProductPricingOutbound;
import com.gdn.partners.pbp.outbound.sap.SapOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel3.BulkProductLevel3AgrregatorService;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorServiceOld;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Converter;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Helper;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3LogisticsService;
import com.gdn.partners.pbp.service.sysparam.SystemParameterService;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.partners.product.pricing.model.enums.PromoBundlingStatus;
import com.gdn.partners.product.pricing.model.enums.PromoBundlingType;
import com.gdn.partners.product.pricing.web.model.dto.FailedItemReasonDto;
import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.partners.product.pricing.web.model.dto.WholeSalePriceSkuStatusDto;
import com.gdn.partners.pbp.dto.promo.request.PromoBundlingPricingSummaryRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSummaryResponse;
import com.gdn.partners.product.pricing.web.model.request.BulkActivateDeactivateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceBulkUpdateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuRequest;
import com.gdn.partners.product.pricing.web.model.response.BulkActivateDeactivateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceBulkUpdateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.commons.enums.MerchantStatus;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.rest.web.model.dto.UpdateDiscountDTO;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignUpdateDiscountResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryUpdatePickupPointResponseDTO;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.DescriptiveAttributeValueType;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.PromoType;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.L3VersionResponse;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.PredefinedAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRequest;
import com.gdn.x.product.rest.web.model.request.AddVariantRequest;
import com.gdn.x.product.rest.web.model.request.B2bFields;
import com.gdn.x.product.rest.web.model.request.B2bFieldsRequest;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateItemRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.QuickEditUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedValueResponse;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

/**
 * Move to class {@link com.gdn.partners.pbp.service.productlevel3.ProductLevel3ServiceBean}. Please
 * as much as possible add new method in new class.
 */
@Lazy
@Slf4j
@Service("ProductLevel3ServiceBeanOld")
public class ProductLevel3ServiceBean implements ProductLevel3Service {

  @Value("${product.limit.switch.enabled}")
  private boolean productLimitSwitchEnabled;

  @Value("${delete.l5.from.inventory.during.l5.deletion.via.need.revision}")
  private boolean deleteL5sFromInventoryDuringL5DeletionViaNeedRevision;

  @Value("${remove.duplicate.sync.call.for.unsync.product.edit.switch}")
  private boolean removeDuplicateSyncCallSwitch;

  @Value("${sanitize.product.name.and.seller.sku}")
  private boolean sanitizeProductNameAndSellerSku;

  @Value("${sync.product.data.on.logistic.update}")
  private boolean syncProductDataOnLogisticUpdate;

  @Value("${migrate.product.in.other.logistic.update.flow}")
  private boolean migrateProductInOtherLogisticUpdateFlow;

  @Value("${price.info.vendor.edited.enabled}")
  private boolean priceInfoVendorEditedEnabled;

  @Value("${price.info.max.variant.limit}")
  private int priceInfoMaxVariantLimit;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${suppress.insert.inventory.exception}")
  private boolean suppressInsertInventoryException;

  @Value("${bopis.category.restriction.feature.switch}")
  private boolean bopisCategoryRestrictionFeatureEnabled;

  @Value("${bopis.category.validation.for.merchant.types}")
   private String bopisCategoryValidationMerchantTypes;

  @Value("${bopis.cnc.restriction.feature.switch}")
  private boolean bopisCNCRestrictionEnabled;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${merge.stock.value.sync.stock.update}")
  private boolean mergeStockValueAndSyncStockUpdate;

  @Value("${wholesale.price.fetch.batch.size}")
  private int wholesalePriceFetchBatchSize;

  @Value("${validate.attribute.add.for.new.item.addition}")
  private boolean validateAttributeAdditionForOnlyNewlyAddedAttributes;

  @Value("${validate.update.product.level3.info}")
  private boolean validateUpdateProductLevel3Info;

  @Value("${seller.penalty.enabled.phase2}")
  private boolean sellerPenaltyEnabledPhase2;

  @Value("${brand.category.edit.enabled.for.external}")
  private boolean brandCategoryEditEnabledForExternal;

  @Value("${convert.pre.order.date.to.jkt}")
  private boolean convertPreOrderDateToJKT;

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductLevel3ServiceBean.class);
  private static final String ORDER_BY_ITEM_SKU = "itemSku";
  private static final String INVALID_REASON = "Internal mistake";
  private static final String SUSPEND = "SUSPEND";
  private static final String REACTIVATE = "REACTIVATE";
  private static final String SKU_SEPARATOR = "-";
  private static final String DOT_SEPARATOR = ".";
  private static final String HTML = "html";
  private static final String CREATED_DATE = "createdDate";
  private static final String DESC = "desc";
  private static final String UPDATE_ARCHIVED_ERROR =
      "Tidak berhasil update  product yang telah diarsipkan";
  private static final String WEIGHT_EXCEED_ERROR =
      "cannot update stock for regular product because weight > 50!";

  private static final String PICKUP_POINT_NAME_ERROR =
    "pickup point name mapped to the code was not found";

  private static final String NO_MAIN_IMAGE_PRESENT = "No main image present";
  private static final int ZERO_INDEX = 0;
  private static final String EMPTY_STRING = "";
  public static final ImmutableList<String> MERCHANT_TYPE_ORDERS_FULLFILL_BY_BLIBLI =
      ImmutableList.of("CM", "RB");
  private static final String ACTIVE_STATUS = "ACTIVE";
  private static final String INACTIVE_STATUS = "INACTIVE";
  private static final String REGEX_FOR_NON_ASCII_CHARS_AND_NEW_LINE = "(/[^\\x00-\\x7F])|(/ +/ig)";
  private static final String REGEX_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT =
      "(\\<.*?\\>|&\\w+.;)|(/\\r\\n|\\n|\\r/gm)|([\\•|\\)]\\s+)";
  private static final Pattern PATTERN_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT =
      Pattern.compile(REGEX_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT);
  private static final Pattern PATTERN_FOR_NON_ASCII_CHARS_AND_NEW_LINE =
      Pattern.compile(REGEX_FOR_NON_ASCII_CHARS_AND_NEW_LINE);
  private static final String REGEX_FOR_EXTRA_SPACE = "\\s+";
  private static final Pattern PATTERN_FOR_EXTRA_SPACE = Pattern.compile(REGEX_FOR_EXTRA_SPACE);
  private static final String POST_LIVE = "Post-live";
  private static final String PRE_LIVE = "Pre-live";
  private static final String POST_LIVE_REVIEW_TYPE = "Post-live";
  private static final String PRE_LIVE_REVIEW_TYPE = "Pre-live";
  private static final String NEW = "new";
  private static final String UPDATE = "update";
  private static final String INTERNAL = "INTERNAL";
  public static final String IMAGE = "IMAGE";
  public static final String CONTENT = "CONTENT";
  public static final String IMAGE_REFRESH = "IMAGE_REFRESH";
  public static final String ITEM_SKU = "itemSku";
  public static final String PICKUP_POINT_CODE = "pickupPointCode";
  public static final String NEW_UPC_CODE = "newUpcCode";
  public static final String OLD_UPC_CODE = "oldUpcCode";
  public static final String BRAND = "Brand";
  public static final String DATE = "DATE";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_NAME = "itemName";
  public static final String DELETE = "delete";
  public static final String MAIN_IMAGE = "MainImage";
  public static final String COLONE = ":";
  public static final String CURRENT_VALUE = "current_value";
  public static final String PREVIOUS_VALUE = "previous_value";
  public static final String ACTIVITY = "activity";
  public static final String LEADINGZERO = "^0+(?!$)";
  public static final String HYPHEN = "-";
  public static final String WHOLESALE_PRICE = "WHOLESALE_PRICE";
  public static final String WHOLESALE = "WHOLESALE";
  public static final String PRODUCT_STATE_ACTIVE = "ACTIVE";
  public static final double GMS_TO_KG_FACTOR = 1000.00;

  @Autowired
  private PreOrderConfig preOrderConfig;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private PickupPointRepository pickupPointRepository;

  @Autowired
  private AttributeRepository attributeRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductInventoryService productInventoryService;

  @Autowired
  private BulkProductLevel3AgrregatorService bulkProductLevel3AgrregatorService;

  @Lazy
  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private UpdateProductItemLevel3ModelConverter updateProductItemLevel3ModelConverter;

  @Autowired
  private ProductStockAlertRepository productStockAlertRepository;

  @Autowired
  private SolrActiveProductCollectionServiceBean solrActiveProductCollectionServiceBean;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private DistributionInfoServiceBean distributionInfoServiceBean;

  @Autowired
  private ProductSuspensionHistoryRepository productSuspensionHistoryRepository;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private ProductMailEventService productMailEventService;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Autowired
  private MapperUtil mapperUtil;

  @Autowired
  private ProductPricingOutbound productPricingOutbound;

  @Autowired
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  @Lazy
  private ProductPublisherService productPublisherService;

  @Autowired
  @Lazy
  private VariantEditValidationService variantEditValidationService;

  @Autowired
  private SolrActiveProductCollectionRepository solrActiveProductCollectionRepository;

  @Autowired
  private ProductAnalyticsOutbound productAnalyticsOutbound;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Autowired
  private CalendarService calendarService;

  @Autowired
  private GeneratorService generatorService;

  @Autowired
  private PickupPointOutbound pickupPointOutbound;

  @Autowired
  @Qualifier("redisTemplate")
  private RedisTemplate<String, Object> redisTemplate;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${redis.enabled}")
  private boolean redisEnabled;

  @Value("${redis.cache.defaultExpiration}")
  private String redisCacheExpiration;

  @Value("${wholesaleprice.addition.batchsize}")
  private int wholesalePriceBatchsize;

  @Value("${max.wholesale.price.requests}")
  private int maxWholesalePriceRequests;

  @Value("${skip.product.level3.aggregator.creation}")
  private boolean skipProductLevel3AggregatorCreation;

  @Value("${add.delete.variants.switch}")
  private boolean addDeleteVariantSwitch;

  @Value("${update.logistics.newly.added.item}")
  private boolean updateLogisticsForNewlyAddedItem;

  @Value("${override.flags.from.seller.sales.channel}")
  private boolean overrideFlagsFromSellerSalesChannel;

  @Value("${inventory.batch.size}")
  private int inventoryBatchSize;

  @Value("${inventory.partition.switch}")
  private boolean inventoryPartitionSwitch;

  @Value("${new.upc.code.validate.flow}")
  private boolean newUpcCodeValidateFlow;

  @Value("${inventory.insert.batch.size.edit}")
  private int inventoryInsertBatchSizeForEdit;

  @Value("${suppress.item.image.update.error}")
  private boolean suppressItemImageUpdateError;

  @Value("${combined.edit.flow.enabled}")
  private boolean combinedEditFlowEnabled;

  @Value("${delete.insert.inventory.on.pickup.point.update}")
  private boolean deleteInsertInventoryOnPickupPointUpdate;

  @Value("${warehouse.bom.activated}")
  private boolean warehouseBomActivated;

  @Value("${brand.attribute.code.values}")
  private String brandAttributeCodes;

  @Value("${validate.attributes.by.brand.attribute.code}")
  private boolean validateAttributesByBrandCode;

  @Value("${validate.on.original.selling.price}")
  private boolean validateOnOriginalSellingPrice;

  @Value("${validate.on.original.selling.price.client.id}")
  private String validateOnOriginalSellingPriceClientId;

  @Value("${add.fbb.pickup.point.codes.for.logistics.update.check}")
  private boolean addFbbPickupPointCodesForLogisticsUpdateCheck;

  @Value("${fbb.pickup.point.codes.logistics.update.validation.change}")
  private boolean fbbPickupPointCodesLogisticsUpdateValidationChange;

  @Value("${wholesale.l5.entry.by.store.id}")
  private boolean wholesaleL5EntryByStoreId;

  @Value("${set.waiting.deletion.for.delete.pickup.point}")
  private boolean setWaitingDeletionForDeletePickupPoint;

  @Value("${faas.feature.switch}")
  private boolean faasFeatureSwitch;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Autowired
  private ProductItemWholesalePriceRepository productItemWholesalePriceRepository;

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Autowired
  private ProductLevel3Converter productLevel3Converter;

  @Autowired
  @Qualifier("globalSystemParameterService")
  private SystemParameterService systemParameterService;

  @Autowired
  @Qualifier("productLevel3DirectAggregatorService")
  private ProductLevel3AggregatorServiceOld productLevel3DirectAggregatorService;

  @Autowired
  @Qualifier("productLevel3MaterializedViewAggregatorService")
  private ProductLevel3AggregatorServiceOld productLevel3MaterializedViewAggregatorService;

  @Lazy
  @Autowired
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Autowired
  private SapOutbound sapOutbound;

  @Autowired
  private WholesaleValidationUtil wholesaleValidationUtil;

  @Autowired
  private ProductLevel3LogisticsService productLevel3LogisticsService;

  @Autowired
  protected ProductLevel3Converter modelConverter;

  @Value("${business.partner.call.require:true}")
  private boolean isBusinessPartnerCallRequire;

  @Autowired
  private CampaignOutbound campaignOutbound;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Value("${campaign.priceInfo.batchsize}")
  private int campaignPriceInfoBatchSize;

  @Value("${enable.add.pickup.point.check}")
  private boolean enableAddPickupPointCheck;

  @Value("${skip.definitive.action}")
  private boolean skipDefinitiveAction;

  @Value("${validate.dimension.l3.update}")
  private boolean validateDimensionInL3Update;

  @Value("${max.product.dimension.limit}")
  private int maxProductDimensionLimit;

  @Autowired
  private ProductLevel3Helper productLevel3Helper;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductNotificationService productNotificationService;

  @Lazy
  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private PredefinedAttributeAllowedValueService predefinedAttributeAllowedValueService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private AGPQueryFeign agpQueryFeign;

  @Autowired
  private ApplicationContext applicationContext;

  @Lazy
  @Autowired
  private ProductLevel3V2Service productLevel3V2Service;

  @Autowired
  private BundleRecipeService bundleRecipeService;

  @Value("${mpp.allowed.sellers}")
  private String mppAllowedSellers;

  @Value("${inventory.L5.sync.stock.enabled}")
  private boolean inventoryL5SyncStockEnabled;

  @Value("${product.history.update.event}")
  private boolean productHistoryUpdateThroughEvent;

  @Value("${mpp.for.wh.enabled}")
  private boolean mppForWhEnabled;

  @Value("${check.restricted.keywords.in.edited.image}")
  private boolean checkRestrictedKeywordsInEditedImage;

  @Value(value = "${schedules.add.edit.enabled}")
  private boolean schedulesAddEditEnabled;

  @Value(value = "${throw.error.on.empty.l5.response}")
  private boolean throwErrorOnEmptyL5Response;

  @Value("${relax.price.edit.disabled.check.for.campaign.validation}")
  private boolean relaxPriceEditDisabledCheck;

  @Value("${bopis.category.action.on.category.change.switch}")
  private boolean bopisCategoryActionOnCategoryChangeSwitch;

  @Value("${fetch.l5.data.for.newly.added.pickup.point.enabled}")
  private boolean fetchL5DataForAddPickupPointEnabled;

  @Value("${newly.added.pickup.point.l5.fetch.batch.size}")
  private int newlyAddedPickupPointL5FetchBatchSize;

  @Value("${product.suitability.feature.enabled}")
  private boolean productSuitabilityFeatureEnabled;

  @Value("${update.common.image.before.variant.images}")
  private boolean updateCommonImageBeforeVariantImages;

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  public static final int SKIP_ALL_ACTIONS = -1;

  @Override
  public Page<InProgressProductsByPickupPointCodeResponse> getInProgressProductsByBusinessPartnerAndPickupPointCode(
      String storeId, String businessPartnerCode, String pickupPointCode, int page, int size) {
    checkArgument(StringUtils.isNotBlank(businessPartnerCode), ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_NULL);
    PageRequest pageRequest = PageRequest.of(page, size);
    Page<InProgressProductsByPickupPointCodeResponse> inProgressProductResponsePageResponseByPickupPoints =
        productBusinessPartnerRepository.findProductDataInProgress(businessPartnerCode, pickupPointCode,
            Arrays.asList(ProductLevel3SummaryCriteria.IN_PROGRESS.name(),
                ProductLevel3SummaryCriteria.NEED_CORRECTION.name(), Constants.ACTIVE), pageRequest);
    return new PageImpl<>(inProgressProductResponsePageResponseByPickupPoints.getContent(), pageRequest,
        inProgressProductResponsePageResponseByPickupPoints.getTotalElements());
  }

  @Override
  public Page<InProgressProductsBySizeChartCodeResponse> getInProgressProductsBySizeChartCode(
      String storeId, String sizeChartCode, int page, int size) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(sizeChartCode),
        ErrorMessages.SIZE_CHART_CODE_MUST_NOT_BE_BLANK);
    PageRequest pageRequest = PageRequest.of(page, size);
    Page<InProgressProductsBySizeChartCodeResponse> inProgressProductsBySizeChartCode =
        productBusinessPartnerRepository.findInProgressProductsWithSizeChartCode(storeId,
            Arrays.asList(ProductLevel3SummaryCriteria.IN_PROGRESS.name(),
                ProductLevel3SummaryCriteria.NEED_CORRECTION.name()), sizeChartCode, pageRequest);
    return new PageImpl<>(inProgressProductsBySizeChartCode.getContent(), pageRequest,
        inProgressProductsBySizeChartCode.getTotalElements());
  }

  @Override
  public ProductLevel3 findDetailByGdnSku(String businessPartnerCode, String gdnSku, boolean concatValueAndValueTypes)
      throws Exception {
    ProductAndItemsResponse productData = null;
    String state = this.productBusinessPartnerService.findProductStateByStoreIdAndItemSku(
        GdnMandatoryRequestParameterUtil.getStoreId(), gdnSku);
    if (StringUtils.isEmpty(state) || Constants.ACTIVE.equals(state) || Constants.DRAFT.equals(state)) {
      productData = this.xProductOutbound.getProductAndSingleItemByItemSku(gdnSku, false).getValue();
    } else if (Constants.DELETED_STATE.equals(state)) {
      throw new ProductRejectedException(ApiErrorCode.ITEM_IS_REJECTED.getCode(),
          ApiErrorCode.ITEM_IS_REJECTED.getDesc());
    } else if (Constants.NEED_CORRECTION.equals(state)) {
      throw new ProductInNeedCorrectionException(ApiErrorCode.ITEM_IS_IN_NEED_CORRECTION.getCode(),
          ApiErrorCode.ITEM_IS_IN_NEED_CORRECTION.getDesc());
    } else if (Constants.IN_PROGRESS_STATE.equals(state)) {
      throw new ProductInReviewException(ApiErrorCode.ITEM_IS_IN_REVIEW.getCode(),
          ApiErrorCode.ITEM_IS_IN_REVIEW.getDesc());
    }
    checkState(Objects.nonNull(productData), ErrorMessages.ITEM_SKU_NOT_FOUND);
    return getProductLevel3Details(businessPartnerCode, gdnSku, productData, concatValueAndValueTypes);
  }

  private ProductLevel3 getProductLevel3Details(String businessPartnerCode, String gdnSku,
      ProductAndItemsResponse productData,  boolean concatValueAndValueTypes) throws Exception {
    ProductWarehouse productWarehouse = null;
    String cogsErrorCode = StringUtils.EMPTY;
    List<CategoryResponse> categoriesData = new ArrayList<>();

    categoriesData = getCategoryResponses(productData, categoriesData);
    ProductLevel3Inventory inventory =
        this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndGdnSku(
            businessPartnerCode, gdnSku);
    PickupPointResponse pickupPointData;
    if (inventory == null) {
      pickupPointData = generateDefaultPickupPointResponse();
    } else {
      String pickupPointCode = inventory.getWebPickupPointCode();
      pickupPointData = this.pickupPointRepository.findByPickupPointCode(pickupPointCode);
      if (inventory.getWebMinAlert() == 0) {
        Integer pbpMinStock =
            productBusinessPartnerService.getMinimumStockByGdnProductItemSku(gdnSku);
        inventory.setWebMinAlert((pbpMinStock == null) ? 0 : pbpMinStock);
      }
    }
    Double cogsValue = null;
    ProfileResponse businessPartner = null;
    try {
      businessPartner =
          this.businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
      productWarehouse = getProductWareHouseData(productData.getItems().get(0).getItemCode(), businessPartner);
    } catch (Exception e) {
      LOGGER.error("error invoking findCogsByProductSku on open bravo", e);
      cogsErrorCode = "ERR-00000";
    }
    if (productWarehouse != null && productWarehouse.getCogs() != null) {
      cogsValue = productWarehouse.getCogs().getCogs();
    }
    List<String> itemSkus = productData.getItems().stream().map(ItemResponse::getItemSku).collect(Collectors.toList());
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap = new HashMap<>();
    for (String itemSku : itemSkus) {
      WholesalePriceSkuResponse wholesalePrice =
          productPricingOutbound.getWholesalePrice(itemSku, productData.getItems().get(0).getPickupPointCode());
      wholesalePriceSkuResponseMap.put(wholesalePrice.getItemSku(), wholesalePrice);
    }
    List<ProductLevel3Logistics> productLevel3Logistics = new ArrayList<>();
    if (Objects.nonNull(businessPartner) && Objects.nonNull(businessPartner.getCompany())
        && StringUtils.isNotBlank(businessPartner.getCompany().getMerchantDeliveryType())) {
      productLevel3Logistics = productLevel3LogisticsService.findLogisticsByItemSku(gdnSku,
          businessPartnerCode, businessPartner.getCompany().getMerchantDeliveryType());
      productData.getProduct().setForceReview(businessPartner.isTrustedSeller() ?
        Boolean.FALSE :
        productData.getProduct().isForceReview());
      productData.getProduct().setSuspended(
        businessPartner.isTrustedSeller() ? Boolean.FALSE : productData.getProduct().isSuspended());
    }
    ProductLevel3 product = generateProductLevel3(productData, categoriesData, pickupPointData,
        inventory, cogsValue, cogsErrorCode, wholesalePriceSkuResponseMap, productLevel3Logistics, concatValueAndValueTypes);
    product.setVersion(productData.getItems().get(0).getVersion());
    return product;
  }

  public List<CategoryResponse> getCategoryResponses(ProductAndItemsResponse productData,
      List<CategoryResponse> categoriesData) throws Exception {
    if (Objects.nonNull(productData) && Objects.nonNull(productData.getProduct()) && Objects.nonNull(
        productData.getProduct().getMasterCatalog()) && Objects.nonNull(
        productData.getProduct().getMasterCatalog().getCategory()) && StringUtils.isNotBlank(
        productData.getProduct().getMasterCatalog().getCategory().getCategoryCode())) {
      categoriesData = this.categoryRepository.findHierarchyByCategoryCode(
          productData.getProduct().getMasterCatalog().getCategory().getCategoryCode());
    }
    return categoriesData;
  }

  @Override
  public ProductLevel3 findDetailByGdnSkuForAllItemSkus(String businessPartnerCode, String gdnSku) throws Exception {
    ProductAndItemsResponse productData = this.productLevel3Repository.findProductAndItemDetailByGdnSku(gdnSku);
    ProductLevel3 productLevel3 = getProductLevel3Details(businessPartnerCode, gdnSku, productData, false);
    UniquePickupPointCodeResponse uniquePickupPointCodeResponse =
        getUniquePickupPointCodes(productData.getProduct().getProductSku());
    if (CollectionUtils.isNotEmpty(uniquePickupPointCodeResponse.getPickupPointCodes())) {
      productLevel3.setPickupPointCodes(new ArrayList<>(uniquePickupPointCodeResponse.getPickupPointCodes()));
    }
    return productLevel3;
  }

  @Override
  public boolean isPristineCategory(String categoryId) throws Exception {
    return false;
  }

  private static String validateAndGetPristineSupportedCategory(String categoryId,
      Map<String, Set<String>> supportedCategories) {
    return supportedCategories.entrySet().stream().filter(
        entry -> !org.springframework.util.CollectionUtils.isEmpty(entry.getValue()) && entry.getValue()
            .contains(categoryId)).findFirst().map(Map.Entry::getKey).orElse(StringUtils.EMPTY);
  }

  /**
   * Calling OB/SAP based on merchant type to get warehouse cogs
   * @param itemCode
   * @return
   * @throws Exception
   */
  public ProductWarehouse getProductWareHouseData(String itemCode, ProfileResponse businessPartner) {
    ProductWarehouse productWarehouse = new ProductWarehouse();
    boolean sapCallEnable = Boolean.valueOf(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SAP_CALL_ENABLED).getValue());
    if (sapCallEnable) {
      if (Objects.nonNull(businessPartner) && Objects.nonNull(businessPartner.getCompany())
          && !MERCHANT_TYPE_ORDERS_FULLFILL_BY_BLIBLI.contains(businessPartner.getCompany().getMerchantType())) {
        LOGGER.info("Calling SAP to get COGS for itemCode : {} ", itemCode);
        CogsValueResponse response = sapOutbound.getCogsValueResponse(itemCode);
        productWarehouse.setCogs(new Cogs(0, itemCode, response.getMovingPrice(), null));
      }
    }
    return productWarehouse;
  }

  /**
   * Get product level 3 aggregation without category hirerarchy, pickup point and cogs
   *
   * @param businessPartnerCode Business partner code
   * @param gdnSku Item sku
   * @return ProductLeve3 (aggregation model)
   * @throws Exception
   */
  private ProductLevel3 findDetailByGdnSkuForUpdateHistory(String businessPartnerCode, String gdnSku)
      throws Exception {
    ProductAndItemsResponse productData = this.productLevel3Repository.findDetailByGdnSku(gdnSku);
    PickupPointResponse pickupPointData = new PickupPointResponse();
    pickupPointData.setCode(productData.getItems().get(0).getPickupPointCode());
    ProductLevel3Inventory inventory =
        this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndGdnSku(
            businessPartnerCode, gdnSku);
    if (Objects.nonNull(inventory) && inventory.getWebMinAlert().equals(0)) {
      Integer pbpMinStock =
          productBusinessPartnerService.getMinimumStockByGdnProductItemSku(gdnSku);
      inventory.setWebMinAlert((pbpMinStock == null) ? 0 : pbpMinStock);
    }
    ProductLevel3 product =
        generateProductLevel3(productData, null, pickupPointData, inventory, null, null,
            new HashMap<>(), new ArrayList<>(), false);
    product.setVersion(productData.getItems().get(0).getVersion());
    return product;
  }

  private ProductLevel3 findDetailByGdnSkuToUpdateHistory(String businessPartnerCode, String gdnSku, ProductAndItemsResponse productData)
      throws Exception {
    PickupPointResponse pickupPointData = new PickupPointResponse();
    pickupPointData.setCode(productData.getItems().get(0).getPickupPointCode());
    ProductLevel3Inventory inventory =
        this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndGdnSku(
            businessPartnerCode, gdnSku);
    if (Objects.nonNull(inventory) && inventory.getWebMinAlert().equals(0)) {
      Integer pbpMinStock =
          productBusinessPartnerService.getMinimumStockByGdnProductItemSku(gdnSku);
      inventory.setWebMinAlert((pbpMinStock == null) ? 0 : pbpMinStock);
    }
    ProductLevel3 product =
        generateProductLevel3(productData, null, pickupPointData, inventory, null, null,
            new HashMap<>(), new ArrayList<>(), false);
    return product;
  }

  @Override
  public ProductLevel3Order findDetailOrderByGdnSku(String businessPartnerCode, String gdnSku)
      throws Exception {
    ProductAndItemsResponse productData = this.productLevel3Repository.findDetailByGdnSku(gdnSku);
    ProductLevel3Order product = generateProductLevel3Order(productData);
    return product;
  }

  /**
   * Method for aggregating data from X-Product, PCB, XBP, X-Inventory, and OpenBravo X-Product data
   * is mandatory, the others will be set to null if not present
   *
   * @param productData X-Product data
   * @param categories PCB category data
   * @param pickupPointData XBP pickup point data
   * @param inventory X-Inventory inventory data
   * @param cogs OB cost of goods sold data
   * @return ProductLevel3 aggregation result
   * @throws Exception if occurs
   */
  protected ProductLevel3 generateProductLevel3(ProductAndItemsResponse productData,
    List<CategoryResponse> categories, PickupPointResponse pickupPointData,
    ProductLevel3Inventory inventory, Double cogs, String cogsErrorCode,
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
    List<ProductLevel3Logistics> productLevel3Logistics, boolean concatValueAndValueTypes) throws Exception {
    Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap =
        ResponseHelper.getAttributeCodeAndValueAndValueTypeMap(productData.getProduct().getMasterDataProduct(),
            sizeChartValueTypeDelimiter);
    ProductLevel3 product = new ProductLevel3();
    product.setId(productData.getProduct().getId());
    product.setStoreId(productData.getProduct().getStoreId());
    product.setCreatedBy(productData.getProduct().getCreatedBy());
    product.setCreatedDate(productData.getProduct().getCreatedDate());
    product.setUpdatedBy(productData.getProduct().getUpdatedBy());
    product.setUpdatedDate(productData.getProduct().getUpdatedDate());
    product.setProductSku(productData.getProduct().getProductSku());
    product.setProductCode(productData.getProduct().getProductCode());
    product.setBusinessPartnerCode(productData.getProduct().getMerchantCode());
    product.setSynchronize(productData.getProduct().isSynchronized());
    product.setProductType(productData.getProduct().getProductType().getCode());
    product.setForceReview(productData.getProduct().isForceReview());
    product.setVersion(productData.getVersion());
    product.setSuspended(productData.getProduct().isSuspended());
    product.setOff2OnChannelActive(productData.getProduct().isOff2OnChannelActive());
    product.setFreeSample(productData.getProduct().isFreeSample());
    if(Objects.nonNull(productData.getProduct().getMasterCatalog())) {
      product.setCategoryCode(productData.getProduct().getMasterCatalog().getCategory()
          .getCategoryCode());
    }
    if (Objects.nonNull(productData.getProduct().getProductScore())) {
      ProductScore productScore = new ProductScore();
      BeanUtils.copyProperties(productData.getProduct().getProductScore(), productScore);
      product.setProductScore(productScore);
    }

    if (CollectionUtils.isNotEmpty(categories)) {
      String[] categoryNameAndHierarchy = generateCategoryNameIdAndHierarchy(categories);
      product.setCategoryName(categoryNameAndHierarchy[0]);
      product.setCategoryHierarchy(categoryNameAndHierarchy[1]);
      product.setCategoryId(categoryNameAndHierarchy[2]);
      product.setWholesalePriceConfigEnabled(categories.get(0).isWholesalePriceConfigEnabled());
    }

    if(Objects.nonNull(productData.getProduct().getMasterDataProduct())) {
      product.setProductName(productData.getProduct().getMasterDataProduct().getProductName());
      product.setBrand(productData.getProduct().getMasterDataProduct().getBrand());
      product.setDescription(new String(productData.getProduct().getMasterDataProduct()
          .getDescription()));
      product.setSpecificationDetail(productData.getProduct().getMasterDataProduct()
          .getSpecificationDetail());
      product.setUniqueSellingPoint(productData.getProduct().getMasterDataProduct()
          .getUniqueSellingPoint());
      product.setProductStory(productData.getProduct().getMasterDataProduct().getProductStory());
      product.setUrl(productData.getProduct().getMasterDataProduct().getUrl());
    }
    product.setInstallationRequired(productData.getProduct().isInstallationRequired());
    product.setItems(new ArrayList<>());
    product.setAttributes(new ArrayList<>());
    product.setImages(new ArrayList<>());
    for (ItemResponse itemData : productData.getItems()) {
      ProductItemLevel3 item = new ProductItemLevel3();
      item.setId(itemData.getId());
      item.setStoreId(itemData.getStoreId());
      item.setCreatedBy(itemData.getCreatedBy());
      item.setCreatedDate(itemData.getCreatedDate());
      item.setUpdatedBy(itemData.getCreatedBy());
      item.setUpdatedDate(itemData.getCreatedDate());
      item.setItemSku(itemData.getItemSku());
      item.setSkuCode(itemData.getItemCode());
      item.setMerchantPromoDiscount(itemData.isMerchantPromoDiscount());
      item.setMerchantPromoDiscountActivated(itemData.isMerchantPromoDiscountActive());
      item.setMerchantSku(itemData.getMerchantSku());
      item.setDisableUnSync(itemData.isDisableUnSync());
      item.setMarkForDelete(itemData.isMarkForDelete());
      item.setFreeSample(productData.getProduct().isFreeSample());
      if (null != itemData.getMasterDataItem()) {
        item.setUpcCode(itemData.getMasterDataItem().getUpcCode());
        item.setItemName(itemData.getMasterDataItem().getGeneratedItemName());
        item.setLength(itemData.getMasterDataItem().getItemLength());
        item.setWidth(itemData.getMasterDataItem().getItemWidth());
        item.setHeight(itemData.getMasterDataItem().getItemHeight());
        item.setWeight(itemData.getMasterDataItem().getItemWeight());
        item.setShippingWeight(itemData.getMasterDataItem().getItemDeliveryWeight());
        item.setDangerousGoodsLevel(itemData.getMasterDataItem().getDangerousLevel());
      } else {
        LOGGER.warn("master data item is null for itemSku : {}", itemData.getItemSku());
      }
      item.setLateFulfillment(itemData.isLateFulfillment());
      item.setArchived(itemData.getArchived());
      item.setPromoBundling(itemData.isPromoBundling());
      if (itemData.getPristineDataItem() != null) {
        item.setPristineId(itemData.getPristineDataItem().getPristineId());
      }
      if (pickupPointData != null) {
        item.setPickupPointCode(pickupPointData.getCode());
        item.setPickupPointName(pickupPointData.getName());
      }
      if(Objects.nonNull(inventory)) {
        item.setAvailableStockLevel1(inventory.getWarehouseAvailable());
        item.setReservedStockLevel1(inventory.getWarehouseReserved());
        item.setAvailableStockLevel2(inventory.getWebAvailable());
        item.setReservedStockLevel2(inventory.getWebReserved());
        item.setSynchronizeStock(inventory.isWebSyncStock());
        item.setMinimumStock(inventory.getWebMinAlert());
      }
      item.setOff2OnActiveFlag(itemData.getOff2OnChannelActive());
      item.setPrices(new ArrayList<>());
      item.setViewConfigs(new ArrayList<>());
      item.setCogs(cogs);
      item.setCogsErrorCode(cogsErrorCode);
      item.setImages(new ArrayList<>());
      for (PriceDTO priceData : itemData.getPrice()) {
        ProductLevel3Price price = new ProductLevel3Price();
        price.setChannelId(priceData.getChannel());
        price.setPrice(priceData.getListPrice());
        price.setSalePrice(priceData.getOfferPrice());
        if (priceData.getListOfDiscountPrices() != null
            && !priceData.getListOfDiscountPrices().isEmpty()) {
          price.setDiscountAmount(priceData.getListOfDiscountPrices().get(0).getDiscountPrice());
          price.setDiscountStartDate(priceData.getListOfDiscountPrices().get(0).getStartDateTime());
          price.setDiscountEndDate(priceData.getListOfDiscountPrices().get(0).getEndDateTime());
          price.setPromotionName(priceData.getListOfDiscountPrices().get(0).getAdjustmentName());
        }
        item.getPrices().add(price);
      }
      for (ItemViewConfigDTO viewConfigData : itemData.getItemViewConfigs()) {
        ProductLevel3ViewConfig viewConfig = new ProductLevel3ViewConfig();
        viewConfig.setChannelId(viewConfigData.getChannel());
        viewConfig.setDisplay(viewConfigData.isDiscoverable());
        viewConfig.setBuyable(viewConfigData.isBuyable());
        item.getViewConfigs().add(viewConfig);
      }
      if (Objects.nonNull(itemData.getMasterDataItem())) {
        for (MasterDataItemImageDTO imageData : itemData.getMasterDataItem().getMasterDataItemImages()) {
          ProductLevel3Image image = new ProductLevel3Image();
          image.setMainImage(imageData.isMainImage());
          image.setSequence(imageData.getSequence());
          image.setLocationPath(imageData.getLocationPath());
          item.getImages().add(image);
        }
      }
      item.setPriceEditDisabled(itemData.isPriceEditDisabled());
      if (Objects.nonNull(wholesalePriceSkuResponseMap.get(itemData.getItemSku()))) {
        WholesalePriceSkuResponse wholesalePriceSkuResponse = wholesalePriceSkuResponseMap.get(itemData.getItemSku());
        item.setWholesalePromoActivated(wholesalePriceSkuResponse.getPromoActive());
        if (Objects.nonNull(wholesalePriceSkuResponse.getSkuStatus())) {
          item.setWholesalePriceActivated(wholesalePriceSkuResponse.getSkuStatus().equalsIgnoreCase(ACTIVE));
        }
        item.setProductItemWholesalePrices(wholesalePriceSkuResponse.getWholesaleRules().entrySet().stream()
            .map(map -> new ProductItemWholesalePriceVo(map.getKey(), map.getValue())).collect(Collectors.toList()));
      }  else {
        item.setWholesalePriceActivated(itemData.getWholesalePriceActivated());
      }
      item.setLogistics(productLevel3Logistics);
      product.getItems().add(item);
    }
    Map<String, MasterDataAttributeDTO> attributeDatas =
        new HashMap<String, MasterDataAttributeDTO>();
    if(Objects.nonNull(productData.getProduct().getMasterDataProduct())) {
      for (MasterDataProductAttributeDTO attributeData : productData.getProduct()
          .getMasterDataProduct().getMasterDataProductAttributes()) {
        attributeDatas.put(attributeData.getMasterDataAttribute().getAttributeCode(),
            attributeData.getMasterDataAttribute());
      }
    }

    List<String> specialAttributes = new ArrayList<>();
    if (productData.getProduct().getProductSpecialAttributes() != null) {
      for (ProductSpecialAttributeDTO attributeSpecialData : productData.getProduct()
          .getProductSpecialAttributes()) {
        ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
        productLevel3Attribute.setValues(new ArrayList<>());
        productLevel3Attribute.setAttributeCode(attributeSpecialData.getAttributeCode());
        setProductLevel3SpecialAttributes(attributeDatas, attributeSpecialData, productLevel3Attribute);
        productLevel3Attribute.getValues().add(attributeSpecialData.getAttributeValue());
        product.getAttributes().add(productLevel3Attribute);
        specialAttributes.add(productLevel3Attribute.getAttributeCode());
      }
    }

    MasterDataAttributeDTO masterDataAttributeDTO = null;
    for (ProductAttributeDetailDTO descriptiveAttributeData : productData.getProduct().getDescriptiveAttributes()) {
      if (!specialAttributes.contains(descriptiveAttributeData.getAttributeCode())) {
        ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
        productLevel3Attribute.setValues(new ArrayList<>());
        productLevel3Attribute.setAttributeCode(descriptiveAttributeData.getAttributeCode());
        productLevel3Attribute.setAttributeName(descriptiveAttributeData.getAttributeName());
        masterDataAttributeDTO = attributeDatas.get(descriptiveAttributeData.getAttributeCode());
        if (masterDataAttributeDTO != null && masterDataAttributeDTO.getAttributeType() != null) {
          productLevel3Attribute.setAttributeType(masterDataAttributeDTO.getAttributeType().name());
          productLevel3Attribute.setVariantCreation(masterDataAttributeDTO.isVariantCreation());
          productLevel3Attribute.setBasicView(masterDataAttributeDTO.isBasicView());
          productLevel3Attribute.setMandatory(masterDataAttributeDTO.isMandatory());
          productLevel3Attribute.setSkuValue(masterDataAttributeDTO.isSkuValue());
        }
        productLevel3Attribute.getValues().add(descriptiveAttributeData.getAttributeValue());
        product.getAttributes().add(productLevel3Attribute);
      }
    }
    if (productData.getProduct().getDefiningAttributes() != null) {
      for (ProductAttributeDTO definingAttributeData : productData.getProduct()
          .getDefiningAttributes()) {
        String itemSku = definingAttributeData.getItemSku();
        for (ProductAttributeDetailDTO definingDetailAttributeData : definingAttributeData
            .getProductAttributeDetails()) {
          ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
          productLevel3Attribute.setValues(new ArrayList<>());
          productLevel3Attribute.setAttributeCode(definingDetailAttributeData.getAttributeCode());
          productLevel3Attribute.setAttributeName(definingDetailAttributeData.getAttributeName());
          masterDataAttributeDTO =
              attributeDatas.get(definingDetailAttributeData.getAttributeCode());
          if (masterDataAttributeDTO != null && masterDataAttributeDTO.getAttributeType() != null) {
            productLevel3Attribute.setAttributeType(masterDataAttributeDTO.getAttributeType()
                .name());
            productLevel3Attribute.setBasicView(masterDataAttributeDTO.isBasicView());
            productLevel3Attribute.setMandatory(masterDataAttributeDTO.isMandatory());
            productLevel3Attribute.setSkuValue(masterDataAttributeDTO.isSkuValue());
            productLevel3Attribute.setVariantCreation(masterDataAttributeDTO.isVariantCreation());
          }
          productLevel3Attribute.getValues().add(
              ResponseHelper.getValueAndValueType(attributeCodeAndValueAndValueTypeMap,
                  definingDetailAttributeData.getAttributeCode(), definingDetailAttributeData.getAttributeValue(),
                  valueTypeAdditionForDefiningAttributes, concatValueAndValueTypes));
          productLevel3Attribute.setItemSku(itemSku);
          product.getAttributes().add(productLevel3Attribute);
        }
      }
    }
    if(Objects.nonNull(productData.getProduct().getMasterDataProduct())) {
      for (MasterDataProductImageDTO imageData : productData.getProduct().getMasterDataProduct()
          .getMasterDataProductImages()) {
        ProductLevel3Image image = new ProductLevel3Image();
        image.setMainImage(imageData.isMainImage());
        image.setSequence(imageData.getSequence());
        image.setLocationPath(imageData.getLocationPath());
        product.getImages().add(image);
      }
    }

    if (Objects.nonNull(productData.getProduct().getPreOrder())) {
      com.gdn.mta.product.entity.PreOrderDTO preOrderDTO = new com.gdn.mta.product.entity.PreOrderDTO();
      BeanUtils.copyProperties(productData.getProduct().getPreOrder(), preOrderDTO);
      product.setPreOrder(preOrderDTO);
    }
    productData.getItems().stream().findFirst()
      .ifPresent(item -> product.setVersion(item.getVersion()));
    return product;
  }

  private void setProductLevel3SpecialAttributes(Map<String, MasterDataAttributeDTO> attributeDatas,
      ProductSpecialAttributeDTO attributeSpecialData, ProductLevel3Attribute productLevel3Attribute) {
    if (!attributeDatas.containsKey(attributeSpecialData.getAttributeCode())) {
      AttributeResponse attributeResponse =
          productOutbound.getAttributeDetailByAttributeCode(attributeSpecialData.getAttributeCode());
      productLevel3Attribute.setAttributeName(attributeResponse.getName());
      productLevel3Attribute.setAttributeType(attributeResponse.getAttributeType());
      productLevel3Attribute.setBasicView(attributeResponse.isBasicView());
      productLevel3Attribute.setMandatory(attributeResponse.isMandatory());
      productLevel3Attribute.setSkuValue(attributeResponse.isSkuValue());
      productLevel3Attribute.setVariantCreation(attributeResponse.isVariantCreation());
    } else {
      MasterDataAttributeDTO masterDataAttributeDetail =
          attributeDatas.get(attributeSpecialData.getAttributeCode());
      productLevel3Attribute.setAttributeName(masterDataAttributeDetail.getAttributeName());
      productLevel3Attribute.setAttributeType(String.valueOf(masterDataAttributeDetail.getAttributeType()));
      productLevel3Attribute.setBasicView(masterDataAttributeDetail.isBasicView());
      productLevel3Attribute.setMandatory(masterDataAttributeDetail.isMandatory());
      productLevel3Attribute.setSkuValue(masterDataAttributeDetail.isSkuValue());
      productLevel3Attribute.setVariantCreation(masterDataAttributeDetail.isVariantCreation());
    }
  }

  private void setProductLevel3SpecialAttributes(Map<String, MasterDataAttributeDTO> attributeDatas,
      ProductSpecialAttributeDTO attributeSpecialData, ProductLevel3AttributeResponse  productLevel3Attribute) {
    if (!attributeDatas.containsKey(attributeSpecialData.getAttributeCode())) {
      AttributeResponse attributeResponse =
          productOutbound.getAttributeDetailByAttributeCode(attributeSpecialData.getAttributeCode());
      productLevel3Attribute.setAttributeName(attributeResponse.getName());
      productLevel3Attribute.setAttributeType(attributeResponse.getAttributeType());
      productLevel3Attribute.setBasicView(attributeResponse.isBasicView());
      productLevel3Attribute.setMandatory(attributeResponse.isMandatory());
      productLevel3Attribute.setSkuValue(attributeResponse.isSkuValue());
      productLevel3Attribute.setVariantCreation(attributeResponse.isVariantCreation());
    } else {
      MasterDataAttributeDTO masterDataAttributeDetail =
          attributeDatas.get(attributeSpecialData.getAttributeCode());
      productLevel3Attribute.setAttributeName(masterDataAttributeDetail.getAttributeName());
      productLevel3Attribute.setAttributeType(String.valueOf(masterDataAttributeDetail.getAttributeType()));
      productLevel3Attribute.setBasicView(masterDataAttributeDetail.isBasicView());
      productLevel3Attribute.setMandatory(masterDataAttributeDetail.isMandatory());
      productLevel3Attribute.setSkuValue(masterDataAttributeDetail.isSkuValue());
      productLevel3Attribute.setVariantCreation(masterDataAttributeDetail.isVariantCreation());
    }
  }


  String[] generateCategoryNameIdAndHierarchy(List<CategoryResponse> categories) throws Exception {
    StringBuilder categoryHierarchy = new StringBuilder();
    String categoryName = StringUtils.EMPTY;
    String categoryId = StringUtils.EMPTY;
    if (categories != null) {
      int i = 0;
      ListIterator<CategoryResponse> iterator = categories.listIterator(categories.size());
      while (iterator.hasPrevious()) {
        CategoryResponse category = iterator.previous();
        if (i == categories.size() - 1) {
          categoryName = category.getName();
          categoryId = category.getId();
        }
        categoryHierarchy.append(category.getName());
        if (categories.size() > 1 && i < categories.size() - 1) {
          categoryHierarchy.append(" > ");
        }
        i++;
      }
    }
    return new String[] {categoryName, categoryHierarchy.toString(), categoryId};
  }

  private ProductLevel3Order generateProductLevel3Order(ProductAndItemsResponse productData)
      throws Exception {
    ProductLevel3Order product = new ProductLevel3Order();
    product.setDescription(new String(productData.getProduct().getMasterDataProduct()
        .getDescription()));
    product.setSpecificationDetail(productData.getProduct().getMasterDataProduct()
        .getSpecificationDetail());
    product.setProductStory(productData.getProduct().getMasterDataProduct().getProductStory());
    product.setInstallationRequired(productData.getProduct().isInstallationRequired());
    product.setBrand(productData.getProduct().getMasterDataProduct().getBrand());
    product.setItems(new ArrayList<>());
    for (ItemResponse itemData : productData.getItems()) {
      ProductItemLevel3Order item = new ProductItemLevel3Order();
      item.setSkuCode(itemData.getItemCode());
      item.setHeight(itemData.getMasterDataItem().getItemHeight());
      item.setLength(itemData.getMasterDataItem().getItemLength());
      item.setWidth(itemData.getMasterDataItem().getItemWidth());
      item.setOff2OnActiveFlag(itemData.getOff2OnChannelActive());
      item.setMerchantSku(itemData.getMerchantSku());
      for (PriceDTO priceData : itemData.getPrice()) {
        ProductLevel3Price price = new ProductLevel3Price();
        price.setChannelId(priceData.getChannel());
        price.setPrice(priceData.getListPrice());
        price.setSalePrice(priceData.getOfferPrice());
        if (CollectionUtils.isNotEmpty(priceData.getListOfDiscountPrices())) {
          price.setDiscountAmount(priceData.getListOfDiscountPrices().get(0).getDiscountPrice());
          price.setDiscountStartDate(priceData.getListOfDiscountPrices().get(0).getStartDateTime());
          price.setDiscountEndDate(priceData.getListOfDiscountPrices().get(0).getEndDateTime());
          price.setPromotionName(priceData.getListOfDiscountPrices().get(0).getAdjustmentName());
        }
        item.getPrices().add(price);
      }
      product.getItems().add(item);
    }
    return product;
  }

  private Page<ProductLevel3Summary> generateProductLevel3Summary(
      Page<ItemSummaryResponse> productDatas, Map<String, List<CategoryResponse>> categoriesData,
      Map<String, PickupPointResponse> pickupPointDatas,
      Map<String, ProductLevel3Inventory> inventoryDatas, PageRequest pageRequest,
      Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap) throws Exception {
    List<ProductLevel3Summary> products = new ArrayList<ProductLevel3Summary>();
    for (ItemSummaryResponse productData : productDatas) {
      MasterCatalogDTO masterCatalogDTO = productData.getMasterCatalog();
      if (masterCatalogDTO == null) {
        LOGGER.error("error retrieving master Catalog for given product. itemSku: {}",
            productData.getItemSku());
        continue;
      }
      String categoryCodeData = "";
      if (masterCatalogDTO.getCategory() != null) {
        categoryCodeData = masterCatalogDTO.getCategory().getCategoryCode();
      }
      String[] categoryNameAndHierarchy =
          generateCategoryNameIdAndHierarchy(categoriesData.get(categoryCodeData));
      PickupPointResponse pickupPointData = pickupPointDatas.get(productData.getPickupPointCode());
      ProductLevel3Inventory inventory = inventoryDatas.get(productData.getItemSku());
      if (inventory == null) {
        LOGGER.error("inventory not found in inventoryDatas mapping. itemSku: {}",
            productData.getItemSku());
        continue;
      }
      ProductLevel3Summary product = new ProductLevel3Summary();
      product.setProductCode(productData.getProductCode());
      product.setItemSku(productData.getItemSku());
      product.setSkuCode(productData.getItemCode());
      product.setProductSku(productData.getProductSku());
      product.setMerchantSku(productData.getMerchantSku());
      product.setMerchantCode(productData.getMerchantCode());
      product.setItemName(productData.getGeneratedItemName());
      product.setProductName(productData.getProductName());
      product.setCreatedDate(productData.getCreatedDate());
      product.setCreatedBy(productData.getCreatedBy());
      product.setUpdatedDate(productData.getUpdatedDate());
      product.setUpdatedBy(productData.getUpdatedBy());
      product.setIsArchived(productData.getArchived());
      product.setForceReview(productData.isForceReview());
      product.setCncActive(productData.isCncActive());
      if (masterCatalogDTO.getCategory() != null) {
        product.setCategoryCode(masterCatalogDTO.getCategory().getCategoryCode());
      }
      product.setCategoryName(categoryNameAndHierarchy[0]);
      product.setCategoryHierarchy(categoryNameAndHierarchy[1]);
      product.setProductType(productData.getProductType().getCode());
      product.setPickupPointCode(pickupPointData.getCode());
      product.setPickupPointName(pickupPointData.getName());
      product.setLateFulfillment(productData.isLateFulfillment());
      product.setAvailableStockLevel1(inventory.getWarehouseAvailable());
      product.setReservedStockLevel1(inventory.getWarehouseReserved());
      product.setAvailableStockLevel2(inventory.getWebAvailable());
      product.setReservedStockLevel2(inventory.getWebReserved());
      product.setMinimumStockLevel2(inventory.getWebMinAlert());
      product.setSynchronizeStock(inventory.isWebSyncStock());
      product.setPrices(new ArrayList<>());
      product.setViewConfigs(new ArrayList<>());
      product.setImages(new ArrayList<>());
      product.setOff2OnActiveFlag(productData.isOff2OnChannelActive());
      product.setBrand(productData.getBrand());
      product.setPromoBundling(productData.isPromoBundling());
      product.setMerchantPromoDiscount(productData.isMerchantPromoDiscount());
      product.setWholesalePriceActivated(productData.getWholesalePriceActivated());
      product.setPromoTypes(productData.getPromoTypes());
      product.setActivePromoBundlings(productData.getActivePromoBundlings());
      product.setPriceEditDisabled(productData.isPriceEditDisabled());
      product.setPriceEditDisabledReasons(productData.getPriceEditDisabledReasons());
      if (MapUtils.isNotEmpty(campaignPriceSkuResponseMap) &&
          campaignPriceSkuResponseMap.containsKey(productData.getItemSku())) {
        CampaignPriceSkuResponse campaignPriceSkuResponse = campaignPriceSkuResponseMap.get(productData.getItemSku());
        product.setItemCampaignMapped(campaignPriceSkuResponse.isRegistered());
        product.setItemCampaignActivated(campaignPriceSkuResponse.isLive());
        product.setCampaignCurrentPrice(campaignPriceSkuResponse.getCampaignPrice());
        product.setCampaignMinPrice(campaignPriceSkuResponse.getMinAllowedPrice());
        product.setCampaignMaxPrice(campaignPriceSkuResponse.getMaxAllowedPrice());
        if(campaignPriceSkuResponse.isLockPriceUpdate()) {
          product.setPriceEditDisabled(true);
          product.getPriceEditDisabledReasons().add(PromoType.CAMPAIGN.getDescription());
        }
      }
      product.setOriginalSellingPrice(productData.getOriginalSellingPrice());
      if(productData.getPrice() != null) {
        for (PriceDTO priceData : productData.getPrice()) {
          ProductLevel3Price price = new ProductLevel3Price();
          price.setChannelId(priceData.getChannel());
          price.setPrice(priceData.getListPrice());
          price.setSalePrice(priceData.getOfferPrice());
          if (priceData.getListOfDiscountPrices() != null && !priceData.getListOfDiscountPrices().isEmpty()) {
            price.setDiscountAmount(priceData.getListOfDiscountPrices().get(0).getDiscountPrice());
            price.setDiscountStartDate(priceData.getListOfDiscountPrices().get(0).getStartDateTime());
            price.setDiscountEndDate(priceData.getListOfDiscountPrices().get(0).getEndDateTime());
            price.setPromotionName(priceData.getListOfDiscountPrices().get(0).getAdjustmentName());
          }
          product.getPrices().add(price);
        }
      }
      for (ItemViewConfigDTO viewConfigData : productData.getItemViewConfigs()) {
        ProductLevel3ViewConfig viewConfig = new ProductLevel3ViewConfig();
        viewConfig.setChannelId(viewConfigData.getChannel());
        viewConfig.setDisplay(viewConfigData.isDiscoverable());
        viewConfig.setBuyable(viewConfigData.isBuyable());
        product.getViewConfigs().add(viewConfig);
      }
      for (MasterDataItemImageDTO imageData : productData.getMasterDataItemImages()) {
        ProductLevel3Image image = new ProductLevel3Image();
        image.setMainImage(imageData.isMainImage());
        image.setSequence(imageData.getSequence());
        image.setLocationPath(imageData.getLocationPath());
        product.getImages().add(image);
      }
      products.add(product);
    }
    return new PageImpl<ProductLevel3Summary>(products, pageRequest,
        productDatas.getTotalElements());
  }

  private Map<String, List<CategoryResponse>> generateCategoriesData(
      Page<ItemSummaryResponse> productDatas) throws Exception {
    Map<String, List<CategoryResponse>> categoriesData =
        new HashMap<String, List<CategoryResponse>>();
    for (ItemSummaryResponse productData : productDatas.getContent()) {
      try {
        String categoryCodeData = "";
        MasterCatalogDTO catalogDTO = productData.getMasterCatalog();
        if (catalogDTO != null && catalogDTO.getCategory() != null) {
          categoryCodeData = catalogDTO.getCategory().getCategoryCode();
        }
        if (categoriesData.get(categoryCodeData) == null) {
          List<CategoryResponse> categoryData =
              this.categoryRepository.findHierarchyByCategoryCode(categoryCodeData);
          categoriesData.put(categoryCodeData, categoryData);
        }
      } catch (Exception e) {
        LOGGER.error("error while retrieving productData contents. productData: {} ", productData,
            e);
      }
    }
    return categoriesData;
  }

  private Map<String, PickupPointResponse> generatePickupPointDatas(
      Page<ItemSummaryResponse> productDatas) throws Exception {
    Map<String, PickupPointResponse> pickupPointDatas = new HashMap<String, PickupPointResponse>();
    for (ItemSummaryResponse productData : productDatas.getContent()) {
      String pickupPointCodeData = productData.getPickupPointCode();
      if (pickupPointDatas.get(pickupPointCodeData) == null) {
        PickupPointResponse pickupPointData =
            this.pickupPointRepository.findByPickupPointCode(pickupPointCodeData);
        pickupPointDatas.put(pickupPointCodeData, pickupPointData);
      }
    }
    return pickupPointDatas;
  }

  private ProductAndItemActivationRequest productAndItemActivationRequest(String businessPartnerCode,
      Product product, ProductBusinessPartner productBusinessPartner,
      Map<String, AttributeResponse> attributes, boolean forceReview) throws Exception {
    Map<String, ProductItem> productItems = new HashMap<String, ProductItem>();
    for (ProductItem productItem : product.getProductItems()) {
      productItems.put(productItem.getId(), productItem);
    }
    Integer productType =
        productBusinessPartner.getProductItemBusinessPartners().get(0).getProductType();
    Boolean installationRequired =
        productBusinessPartner.getProductItemBusinessPartners().get(0).isInstallation();
    ProductAndItemActivationRequest productLevel3Request = new ProductAndItemActivationRequest();
    productLevel3Request.setProduct(new ProductDTO());
    productLevel3Request.getProduct().setProductCode(product.getProductCode());
    productLevel3Request.getProduct().setProductType(
      GdnBaseLookup.PRODUCT_TYPE_REGULAR.equals(productType) ? ProductType.REGULAR
            : GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT.equals(productType) ? ProductType.BIG_PRODUCT
            : ProductType.BOPIS);
    productLevel3Request.getProduct().setInstallationRequired(installationRequired);
    productLevel3Request.getProduct().setMerchantCode(businessPartnerCode);
    productLevel3Request.setItems(new ArrayList<>());
    productLevel3Request.getProduct().setProductSpecialAttributes(new ArrayList<>());
    PreOrderDTO preOrderDTO = PreOrderDTO.builder().isPreOrder(productBusinessPartner.isPreOrder())
        .preOrderType(productBusinessPartner.getPreOrderType()).preOrderValue(productBusinessPartner.getPreOrderValue())
        .preOrderDate(productBusinessPartner.getPreOrderDate()).build();
    productLevel3Request.getProduct().setPreOrder(preOrderDTO);
    productLevel3Request.getProduct().setOff2OnChannelActive(productBusinessPartner.isOff2OnChannelActive());
    productLevel3Request.getProduct().setOnline(productBusinessPartner.isOnline());
    Map<String, List<ProductItemBusinessPartner>> productItemBusinessPartnerGroup =
        productBusinessPartner.getProductItemBusinessPartners().stream().filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete))
            .collect(Collectors.groupingBy(ProductItemBusinessPartner::getGdnProductItemSku));
    for (List<ProductItemBusinessPartner> productItemBusinessPartners : productItemBusinessPartnerGroup.values()) {
      ProductItemBusinessPartner firstProductItemBusinessPartner = productItemBusinessPartners.get(0);
      ProductItem productItem = productItems.get(firstProductItemBusinessPartner.getProductItemId());
      ItemActivationRequest itemRequest = new ItemActivationRequest();
      itemRequest.setForceReview(false);
      itemRequest.setMerchantSku(firstProductItemBusinessPartner.getMerchantSku());
      itemRequest.setIsLateFulfillment(
          !GdnBaseLookup.PRODUCT_TYPE_REGULAR.equals(firstProductItemBusinessPartner.getProductType()));
      if (Objects.nonNull(productItem)) {
        itemRequest.setItemCode(productItem.getSkuCode());
      }
      itemRequest.setItemPickupPoints(new ArrayList<>());
      for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartners) {
        ItemPickupPointActivationRequest itemPickupPointActivationRequest = new ItemPickupPointActivationRequest();
        itemPickupPointActivationRequest.setPickupPointCode(productItemBusinessPartner.getPickupPointId());
        itemPickupPointActivationRequest.setPrice(new HashSet<>());
        itemPickupPointActivationRequest.setItemViewConfigs(new HashSet<>());
        PriceRequest priceRequest = new PriceRequest();
        priceRequest.setOfferPrice(productItemBusinessPartner.getSalePrice());
        priceRequest.setListPrice(productItemBusinessPartner.getPrice());
        priceRequest.setChannel(ChannelName.DEFAULT.name());
        itemPickupPointActivationRequest.getPrice().add(priceRequest);
        ItemViewConfigRequest viewConfigRequest = new ItemViewConfigRequest();
        ItemViewConfigRequest b2bViewConfigRequest = new ItemViewConfigRequest();
        ItemViewConfigRequest cncViewConfigRequest = new ItemViewConfigRequest();
        if (forceReview) {
          viewConfigRequest.setBuyable(false);
          viewConfigRequest.setDiscoverable(false);
          b2bViewConfigRequest.setBuyable(false);
          b2bViewConfigRequest.setDiscoverable(false);
          cncViewConfigRequest.setBuyable(false);
          cncViewConfigRequest.setDiscoverable(false);
        } else {
          viewConfigRequest.setBuyable(productItemBusinessPartner.isBuyable());
          viewConfigRequest.setDiscoverable(productItemBusinessPartner.isDisplay());
          b2bViewConfigRequest.setBuyable(productItemBusinessPartner.isB2bBuyable());
          b2bViewConfigRequest.setBuyable(productItemBusinessPartner.isB2bDiscoverable());
          cncViewConfigRequest.setBuyable(productItemBusinessPartner.isCncBuyable());
          cncViewConfigRequest.setDiscoverable(productItemBusinessPartner.isCncDiscoverable());
        }
        viewConfigRequest.setChannel(ChannelName.DEFAULT.name());
        b2bViewConfigRequest.setChannel(Constants.B2B_CHANNEL);
        cncViewConfigRequest.setChannel(Constants.CNC_CHANNEL);
        itemPickupPointActivationRequest.getItemViewConfigs().addAll(Arrays.asList(viewConfigRequest, b2bViewConfigRequest, cncViewConfigRequest));
        B2bFieldsRequest b2bFieldsRequest = new B2bFieldsRequest();
        b2bFieldsRequest.setBasePrice(productItemBusinessPartner.getB2bPrice());
        b2bFieldsRequest.setManaged(productItemBusinessPartner.isB2bManaged());
        itemPickupPointActivationRequest.setB2bFields(b2bFieldsRequest);
        itemRequest.getItemPickupPoints().add(itemPickupPointActivationRequest);
      }
      productLevel3Request.getItems().add(itemRequest);
    }
    for (ProductBusinessPartnerAttribute productBusinessPartnerAttribute : productBusinessPartner
        .getProductBusinessPartnerAttributes()) {
      AttributeResponse attribute =
          attributes.get(productBusinessPartnerAttribute.getAttributeId());
      if (Objects.nonNull(attribute)) {
        ProductSpecialAttributeDTO attributeRequest = new ProductSpecialAttributeDTO();
        attributeRequest.setAttributeCode(attribute.getAttributeCode());
        attributeRequest.setAttributeValue(productBusinessPartnerAttribute.getValue());
        productLevel3Request.getProduct().getProductSpecialAttributes().add(attributeRequest);
      }
    }
    return productLevel3Request;
  }

  private Map<String, AttributeResponse> generateAttribute(
      ProductBusinessPartner productBusinessPartner) throws Exception {
    Map<String, AttributeResponse> attributes = new HashMap<String, AttributeResponse>();
    for (ProductBusinessPartnerAttribute productBusinessPartnerAttribute : productBusinessPartner
        .getProductBusinessPartnerAttributes()) {
      AttributeResponse attribute =
          this.attributeRepository.findDetailById(productBusinessPartnerAttribute.getAttributeId());
      if (attribute != null) {
        attributes.put(attribute.getId(), attribute);
      }
    }
    return attributes;
  }

  @Override
  public Page<ProductLevel3Summary> findSummaryByCategoryAndBrandFilter(
      BrandAndCategoryItemSummaryRequest request, PageRequest pageRequest, SortOrder sort)
      throws Exception {
    Page<ItemSummaryResponse> productDatas;
    Map<String, List<CategoryResponse>> categoriesData = new HashMap<>();
    Map<String, PickupPointResponse> pickupPointDatas = new HashMap<>();
    Map<String, ProductLevel3Inventory> inventoryDatas = new HashMap<>();
    com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest productRequest = new com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest();
    BeanUtils.copyProperties(request, productRequest);
    productDatas = this.productLevel3Repository.findSummaryByCategoryAndBrandFilter(productRequest, pageRequest, sort);
    if (!productDatas.getContent().isEmpty()) {
      categoriesData = generateCategoriesData(productDatas);
      pickupPointDatas = generatePickupPointDatas(productDatas);
      List<String> gdnSkus = this.productLevel3Converter
          .convertItemSummaryResponseToListOfGdnSku(productDatas.getContent());
      List<ProductLevel3Inventory> inventoryList = this.productLevel3InventoryService
          .findInventoryByBusinessPartnerCodeAndListOfGdnSku(request.getMerchantCode(), gdnSkus);
      inventoryDatas =
          this.productLevel3Converter.convertProductLevel3InventoryToMapOfGdnSku(inventoryList);
    }
    return generateProductLevel3Summary(productDatas, categoriesData, pickupPointDatas,
        inventoryDatas, pageRequest, null);
  }

  @Override
  public Page<ProductLevel3Summary> findSummaryByGdnSkuList(String businessPartnerCode,
      List<String> gdnSku, PageRequest pageRequest) throws Exception {
    Map<String, List<CategoryResponse>> categoriesData = null;
    Map<String, PickupPointResponse> pickupPointDatas = null;
    Map<String, ProductLevel3Inventory> inventoryDatas = new HashMap<>();
    Map<String, CampaignPriceSkuResponse> campaignPriceResponseMap = new HashMap<>();
    Page<ItemSummaryResponse> productDatas =
        this.productLevel3Repository.findSummaryByFilter(businessPartnerCode, null, gdnSku, null,
            null, null, null, null, pageRequest, null, null);
    Map<String, String> itemSkuAndPickupCodeMap = new HashMap<>();
    if (!productDatas.getContent().isEmpty()) {
      // get list of category by categoryCode and pickup point and pickupPointCode
      categoriesData = generateCategoriesData(productDatas);
      pickupPointDatas = generatePickupPointDatas(productDatas);
      List<String> gdnSkus =
          this.productLevel3Converter.convertItemSummaryResponseToListOfGdnSku(
              productDatas.getContent());
      itemSkuAndPickupCodeMap =
          this.productLevel3Converter.convertItemSummaryResponseToItemSkuAndPickupCodeMap(productDatas.getContent());
      Map<String, Double> l5AndOfferPriceMap = productDatas.getContent().stream().collect(Collectors
          .toMap(item -> CommonUtils.toL5Id(item.getItemSku(), item.getPickupPointCode()),
              item -> item.getPrice().stream().findFirst().orElse(new PriceDTO()).getOfferPrice(), (a, b) -> a));
      List<ProductLevel3Inventory> inventoryList;
      if (StringUtils.isEmpty(businessPartnerCode)) {
        Map<String, String> gdnSkuMap = new HashMap<>();
        for (ItemSummaryResponse product : productDatas) {
          gdnSkuMap.put(product.getItemSku(), product.getMerchantCode());
        }
        inventoryList = this.productLevel3InventoryService.findInventoryByGdnSkuMap(gdnSkuMap);
      } else {
        inventoryList = this.productLevel3InventoryService
            .findInventoryByBusinessPartnerCodeAndListOfGdnSku(businessPartnerCode, gdnSkus);
      }
      inventoryDatas =
          this.productLevel3Converter.convertProductLevel3InventoryToMapOfGdnSku(inventoryList);
      String categoryCode = categoriesData.keySet().stream().findFirst().orElse(StringUtils.EMPTY);
      campaignPriceResponseMap = getItemSkuCampaignPriceResponseMap(gdnSkus, categoryCode, itemSkuAndPickupCodeMap, l5AndOfferPriceMap);
    }
    return generateProductLevel3Summary(productDatas, categoriesData, pickupPointDatas,
        inventoryDatas, pageRequest, campaignPriceResponseMap);
  }

  private void createAuditLogs(String businessPartnerCode, String sku,
      ProductAndItemsResponse savedProductData, ProductAndItemsResponse updatedProductData, String accessChannel,
      String username)
      throws Exception {
    UpdateProductItemLevel3Model savedProductValue =
        updateProductItemLevel3ModelConverter.convertFromProductAndItemsResponse(savedProductData);
    UpdateProductItemLevel3Model updatedProductValue =
        updateProductItemLevel3ModelConverter
            .convertFromProductAndItemsResponse(updatedProductData);
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(businessPartnerCode, sku, savedProductValue,
        updatedProductValue, accessChannel, updatedProductData.getProduct().getProductSku(),
        updatedProductData.getItems().get(0).getMasterDataItem().getGeneratedItemName(), false, username);
  }

  private void createAuditLogs(String businessPartnerCode, String sku, ProductAndItemsResponse savedProductData,
      ProductAndItemsResponse updatedProductData, String accessChannel, String oldWholesaleRules,
      String newWholesaleRules) throws Exception {
    UpdateProductItemLevel3Model savedProductValue =
        updateProductItemLevel3ModelConverter.convertFromProductAndItemsResponse(savedProductData);
    UpdateProductItemLevel3Model updatedProductValue =
        updateProductItemLevel3ModelConverter.convertFromProductAndItemsResponse(updatedProductData);
    savedProductValue.setWholesaleRules(oldWholesaleRules);
    updatedProductValue.setWholesaleRules(newWholesaleRules);
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(businessPartnerCode, sku, savedProductValue, updatedProductValue, accessChannel,
            updatedProductData.getProduct().getProductSku(),  updatedProductData.getItems().get(0).getMasterDataItem().getGeneratedItemName(),
            false, StringUtils.EMPTY);
  }

  private void createAuditLogs(String businessPartnerCode, String sku, ProductLevel3Inventory savedInventoryData,
      ProductLevel3Inventory updatedInventoryData, String accessChannel, String productSku, String name)
      throws Exception {
    UpdateProductItemLevel3Model savedProductValue =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Inventory(savedInventoryData);
    UpdateProductItemLevel3Model updatedProductValue =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Inventory(updatedInventoryData);
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(businessPartnerCode, sku, savedProductValue, updatedProductValue, accessChannel,
            productSku, name, false, StringUtils.EMPTY);
  }

  @Override
  public ApiErrorCode updateItemPrice(String storeId, ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest,
      String itemSku) throws Exception {
    ApiErrorCode apiErrorCode = null;
    boolean wholesaleRulesUpdated = false;
    ProductAndItemsResponse savedProductData =
        this.productLevel3Repository.findDetailByGdnSku(itemSku);
    checkVersionForUpdate(productPriceAndWholesaleRequest.getVersion(),
        savedProductData.getItems().get(0).getVersion());
    apiErrorCode = variantEditValidationService.checkArchivedSuspendedRejectedFaultyImageForUpdate(savedProductData);
    if (Objects.nonNull(apiErrorCode)) {
      return apiErrorCode;
    }
    String categoryCode = ofNullable(savedProductData.getProduct().getMasterCatalog())
        .map(MasterCatalogDTO::getCategory).map(CategoryDTO::getCategoryCode).orElse(StringUtils.EMPTY);
    if (isSalesPriceChangedOrPriceEditDisabled(savedProductData.getItems().get(0).getPrice(),
        productPriceAndWholesaleRequest.getOfferPrice(),
        savedProductData.getItems().get(0).isPriceEditDisabled())) {
      validateDiscountPrice(itemSku, productPriceAndWholesaleRequest.getOfferPrice(), categoryCode,
          savedProductData.getItems().get(0).getPickupPointCode());
    }
    WholesalePriceSkuResponse currentWholesalePriceSkuResponse = new WholesalePriceSkuResponse();
    WholesalePriceRequest newWholesalePriceRequest = new WholesalePriceRequest();
    if(Objects.nonNull(productPriceAndWholesaleRequest.getWholesalePriceActivated())) {
      currentWholesalePriceSkuResponse = this.productPricingOutbound
          .getWholesalePrice(itemSku, savedProductData.getItems().get(0).getPickupPointCode());
      newWholesalePriceRequest =
          ConverterUtil.toWholesalePriceRequest(productPriceAndWholesaleRequest, savedProductData);
      apiErrorCode = this.wholesaleValidationUtil
          .validateWholesalePriceRequestForUpdate(storeId, productPriceAndWholesaleRequest, itemSku, savedProductData,
              this.getMinimumPrice(storeId));
      if (!newWholesalePriceRequest.getWholesalePriceSkuRequests().get(0).getWholesaleRules()
          .equals(currentWholesalePriceSkuResponse.getWholesaleRules())) {
        this.productPricingOutbound.upsertWholesalePrice(newWholesalePriceRequest);
        wholesaleRulesUpdated = true;
      }
      validateAndActivateDeactivateWholesalePrice(productPriceAndWholesaleRequest.getWholesalePriceActivated(), itemSku,
          currentWholesalePriceSkuResponse, savedProductData.getItems().get(0).getPickupPointCode());
    }
    PriceRequest priceRequest = ConverterUtil.toPriceRequest(productPriceAndWholesaleRequest);
    boolean isArchived = savedProductData.getItems().get(0).getArchived();
    GdnPreconditions.checkArgument(!isArchived, UPDATE_ARCHIVED_ERROR);
    try {
      this.productLevel3Repository.updateItemPrice(priceRequest, itemSku);
    } catch (Exception e) {
      LOGGER.error("Error when updating price in x-prouct for itemSku : {} ", itemSku, e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, e.getMessage());
    }
    updateDiscountPriceInCampaign(itemSku, productPriceAndWholesaleRequest.getOfferPrice(), categoryCode,
        savedProductData.getItems().get(0).getPickupPointCode());
    generateHistoryForPriceAndWholesaleUpdate(itemSku, wholesaleRulesUpdated, newWholesalePriceRequest,
        currentWholesalePriceSkuResponse, savedProductData, productPriceAndWholesaleRequest.getOfferPrice());
    return apiErrorCode;
  }

  @Override
  public void updateDiscountPriceInCampaign(String itemSku, Double offerPrice, String categoryCode,
      String pickupPointCode)
      throws Exception {
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    UpdateDiscountDTO updateDiscountDTO = new UpdateDiscountDTO();
    updateDiscountDTO.setCategoryCode(categoryCode);
    updateDiscountDTO.setItemSku(itemSku);
    updateDiscountDTO.setSellingPrice(offerPrice);
    updateDiscountDTO.setPickUpPointCode(pickupPointCode);
    campaignUpdateDiscountRequest.setDiscountDTOList(Collections.singletonList(updateDiscountDTO));
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
      campaignOutbound.validateAndUpdateDiscountPrice(true, campaignUpdateDiscountRequest);
    if (Objects.nonNull(campaignUpdateDiscountResponse) && MapUtils.isNotEmpty(
      campaignUpdateDiscountResponse.getItemSkuStatusMap())
      && campaignUpdateDiscountResponse.getItemSkuStatusMap().containsKey(itemSku)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          campaignUpdateDiscountResponse.getItemSkuStatusMap().get(itemSku));
    }
  }

  private void validateDiscountPrice(String itemSku, Double offerPrice, String categoryCode, String pickupPointCode) throws Exception {
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    UpdateDiscountDTO updateDiscountDTO = new UpdateDiscountDTO();
    updateDiscountDTO.setCategoryCode(categoryCode);
    updateDiscountDTO.setItemSku(itemSku);
    updateDiscountDTO.setSellingPrice(offerPrice);
    updateDiscountDTO.setPickUpPointCode(pickupPointCode);
    campaignUpdateDiscountRequest.setDiscountDTOList(Collections.singletonList(updateDiscountDTO));
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        campaignOutbound.validateDiscountPrice(true, campaignUpdateDiscountRequest);
    if (Objects.nonNull(campaignUpdateDiscountResponse) && MapUtils.isNotEmpty(
      campaignUpdateDiscountResponse.getItemSkuStatusMap())
      && campaignUpdateDiscountResponse.getItemSkuStatusMap().containsKey(itemSku)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          campaignUpdateDiscountResponse.getItemSkuStatusMap().get(itemSku));
    }
  }

  private void validateAndActivateDeactivateWholesalePrice(Boolean wholesalePriceActivated, String itemSku,
      WholesalePriceSkuResponse currentWholesalePriceSkuResponse, String pickupPointCode) throws Exception {
    if(checkIfFlagUpdateRequired(currentWholesalePriceSkuResponse, wholesalePriceActivated)) {
      if ((ACTIVE_STATUS.equals(currentWholesalePriceSkuResponse.getSkuStatus()) && Boolean.FALSE
          .equals(wholesalePriceActivated)) || (
          INACTIVE_STATUS.equals(currentWholesalePriceSkuResponse.getSkuStatus()) && Boolean.TRUE
              .equals(wholesalePriceActivated)) || StringUtils
          .isEmpty(currentWholesalePriceSkuResponse.getSkuStatus())) {
        updateWholesaleActivatedForPricing(itemSku, wholesalePriceActivated, pickupPointCode);
      }
    }
  }

  private boolean checkIfFlagUpdateRequired(WholesalePriceSkuResponse currentWholesalePriceSkuResponse,
      Boolean wholesalePriceActivated) {
    if(Boolean.TRUE.equals(currentWholesalePriceSkuResponse.getPromoActive())) {
      return false;
    }
    if(MapUtils.isEmpty(currentWholesalePriceSkuResponse.getWholesaleRules()) && Boolean.TRUE.equals(wholesalePriceActivated)) {
      return false;
    }
    return true;
  }

  public void generateHistoryForPriceAndWholesaleUpdate(String itemSku, boolean wholesaleRulesUpdated,
      WholesalePriceRequest newWholesalePriceRequest, WholesalePriceSkuResponse currentWholesalePriceSkuResponse,
      ProductAndItemsResponse savedProductData, double offerPrice) throws Exception {
    ProductAndItemsResponse updatedProductData =
        this.productLevel3Repository.findDetailByGdnSku(itemSku);
    if(wholesaleRulesUpdated) {
      List<ProductItemHistoryForWholesale> productItemHistoryForWholesaleList = new ArrayList<>();
      ofNullable(newWholesalePriceRequest.getWholesalePriceSkuRequests().get(0).getWholesaleRules())
          .orElse(new HashMap<>()).forEach((key, value) -> {
        productItemHistoryForWholesaleList.add(
            new ProductItemHistoryForWholesale(key, value, Math.floor((offerPrice * (1 - value / 100) * 1000)) / 1000));
      });
      String newWholesaleRules = this.mapperUtil.mapRequestToString(productItemHistoryForWholesaleList);
      productItemHistoryForWholesaleList.clear();
      ofNullable(currentWholesalePriceSkuResponse.getWholesaleRules()).orElse(new HashMap<>())
          .forEach((key, value) -> {
            productItemHistoryForWholesaleList.add(new ProductItemHistoryForWholesale(key, value,
                Math.floor((offerPrice * (1 - value / 100) * 1000)) / 1000));
          });
      String oldWholesaleRules = this.mapperUtil.mapRequestToString(productItemHistoryForWholesaleList);
      createAuditLogs(savedProductData.getProduct().getMerchantCode(), itemSku, savedProductData, updatedProductData,
          StringUtils.EMPTY, oldWholesaleRules, newWholesaleRules);
    } else {
      createAuditLogs(savedProductData.getProduct().getMerchantCode(), itemSku, savedProductData, updatedProductData,
          StringUtils.EMPTY, StringUtils.EMPTY);
    }
  }

  private void updateWholesaleActivatedForPricing(String itemSku, Boolean wholesalePriceActivated,
      String pickupPointCode) throws Exception {
    if(Boolean.TRUE.equals(wholesalePriceActivated)) {
      productPricingOutbound.setWholesaleActivated(itemSku, ACTIVE_STATUS, pickupPointCode);
    } else if (Boolean.FALSE.equals(wholesalePriceActivated)) {
      productPricingOutbound.setWholesaleActivated(itemSku, INACTIVE_STATUS, pickupPointCode);
    }
  }

  private void checkVersionForUpdate(Long priceAndWholesaleRequestVersion, Long savedProductDataVersion) {
    if (Objects.nonNull(priceAndWholesaleRequestVersion) && Objects.nonNull(savedProductDataVersion)
        && priceAndWholesaleRequestVersion <= savedProductDataVersion) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.INVALID_VERSION_ERROR);
    }
  }

  @Override
  public void updateItemViewConfig(ItemViewConfigRequest itemViewConfigRequest, String itemSku, String username)
      throws Exception {
    updateItemViewConfigToHideItemSku(itemViewConfigRequest, itemSku, username);
  }

  @Override
  public boolean updateItemViewConfigToHideItemSku(ItemViewConfigRequest itemViewConfigRequest, String itemSku,
      String username)
      throws Exception {
    ProductAndItemsResponse savedProductData =
        this.productLevel3Repository.findDetailByGdnSku(itemSku);
    boolean isArchived = savedProductData.getItems().get(0).getArchived();
    boolean isFreeSample = savedProductData.getItems().get(0).getFreeSample();
    if(isFreeSample && (itemViewConfigRequest.isBuyable() || itemViewConfigRequest.isDiscoverable())){
      LOGGER.error("Error updating free sample product : {} ",
        savedProductData.getItems().get(0).getItemSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ErrorMessages.FREE_SAMPLE_MUST_NOT_BE_BUYABLE_OR_DISCOVERABLE);
    }
    if (isArchived) {
      return false;
    } else {
      boolean viewConfigChanges =
          checkViewConfigChanges(itemViewConfigRequest, savedProductData.getItems().get(0).getItemViewConfigs());
      if (viewConfigChanges) {
        this.productLevel3Repository.updateItemViewConfig(itemViewConfigRequest, itemSku);
        ProductAndItemsResponse updatedProductData = this.productLevel3Repository.findDetailByGdnSku(itemSku);
        createAuditLogs(savedProductData.getProduct().getMerchantCode(), itemSku, savedProductData, updatedProductData,
            itemViewConfigRequest.getChannel(), username);
        return true;
      }
      return false;
    }
  }

  private boolean checkViewConfigChanges(ItemViewConfigRequest itemViewConfigRequest,
      Set<ItemViewConfigDTO> itemViewConfigs) {
    return itemViewConfigs.stream().anyMatch(
        itemViewConfigDTO -> itemViewConfigDTO.isBuyable() != itemViewConfigRequest.isBuyable()
            || itemViewConfigDTO.isDiscoverable() != itemViewConfigRequest.isDiscoverable() || Objects
            .nonNull(itemViewConfigRequest.getItemBuyableSchedules()) || Objects
            .nonNull(itemViewConfigRequest.getItemDiscoverableSchedules()));
  }

  @Override
  public ApiErrorCode synchronizeProduct(String productSku, String itemSku) throws Exception {
    ProductL3Response savedProductData = xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
    ApiErrorCode apiErrorCode = checkProductStatus(savedProductData);
    if (Objects.nonNull(apiErrorCode)) {
      LOGGER.warn("Product sku not in correct state : {} , error message : {}", productSku, apiErrorCode.getDesc());
      return apiErrorCode;
    }
    if (savedProductData.isSynchronized()) {
      return null;
    }
    ProductAndItemsResponse updatedProductData = this.productLevel3Repository.synchronizeProduct(productSku);
    this.updatedProductHistoryService.createProductL3AuditLog(savedProductData.getMerchantCode(), Constants.DEFAULT, productSku,
        savedProductData.getMasterDataProduct().getProductName(), UpdateProductActivity.SYNC_CONTENT.getDesc(),
        String.valueOf(false), String.valueOf(true), false, StringUtils.EMPTY);
    return null;
  }

  @Override
  public ApiErrorCode unsynchronizeProduct(String productSku, String itemSku) throws Exception {
    ProductL3Response savedProductData = xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
    ApiErrorCode apiErrorCode = checkProductStatus(savedProductData);
    if (Objects.nonNull(apiErrorCode)) {
      LOGGER.warn("Product sku not in correct state : {} , error message : {}", productSku, apiErrorCode.getDesc());
      return apiErrorCode;
    }
    if (!savedProductData.isSynchronized()) {
      return null;
    }
    ProductAndItemsResponse updatedProductData = this.productLevel3Repository.unsynchronizeProduct(productSku);
    this.updatedProductHistoryService.createProductL3AuditLog(savedProductData.getMerchantCode(), Constants.DEFAULT, productSku,
        savedProductData.getMasterDataProduct().getProductName(), UpdateProductActivity.SYNC_CONTENT.getDesc(),
        String.valueOf(true), String.valueOf(false), false, StringUtils.EMPTY);
    return null;
  }

  @Override
  public void updateItemStock(String businessPartnerCode, String gdnSku, Integer deltaStock, Integer minimumStock)
      throws Exception {
    checkItemState(gdnSku);
    ProductLevel3Inventory savedInventory =
        this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndGdnSku(businessPartnerCode, gdnSku);
    this.productLevel3InventoryService.updateStock(businessPartnerCode, gdnSku, deltaStock);
    if (minimumStock != null) {
      this.productLevel3InventoryService.updateMinimumStockAlert(businessPartnerCode, gdnSku, minimumStock);
      this.productBusinessPartnerService.updateMinimumStockByGdnProductItemSku(gdnSku, minimumStock);
    }
    ProductLevel3Inventory updatedInventory =
        this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndGdnSku(businessPartnerCode, gdnSku);
    ItemsSummaryDetailRequest itemFilterRequest = new ItemsSummaryDetailRequest();
    itemFilterRequest.setItemSku(gdnSku);
    Page<ItemSummaryDetailResponse> productDatas =
        xProductOutbound.findSummaryDetailsByFilter(itemFilterRequest, PageRequest.of(0, 1));
    if (CollectionUtils.isNotEmpty(productDatas.getContent())) {
      ItemSummaryDetailResponse itemData = productDatas.getContent().get(0);
      createAuditLogs(businessPartnerCode, gdnSku, savedInventory, updatedInventory, StringUtils.EMPTY,
          itemData.getProductSku(), itemData.getGeneratedItemName());
    }
  }

  private void checkItemState(String gdnSku) {
    String productCode = productItemBusinessPartnerService.findReviewPendingProductCodeByItemSku(gdnSku);
    if (Objects.nonNull(productCode)) {
      ProductImageQcProcessingResponse response =
          productImageQcProcessingResponseService.findByStoreIdAndProductCode(DEFAULT_STORE_ID, productCode);
      if (Objects.nonNull(response)) {
        checkState(!response.isForceReview(), ErrorMessages.ITEM_NOT_EDITABLE);
      }
    }
  }

  private void generateProductBusinessPartner(Product product,
      ProductBusinessPartner productBusinessPartner,
      AddProductAndItemsResponse addProductAndItemsResponse) throws Exception {
    Map<String, ProductItem> productItems = new HashMap<String, ProductItem>();
    for (ProductItem productItem : product.getProductItems()) {
      productItems.put(productItem.getId(), productItem);
    }
    productBusinessPartner.setGdnProductSku(addProductAndItemsResponse.getProductSku());
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
        .getProductItemBusinessPartners()) {
      ProductItem productItem = productItems.get(productItemBusinessPartner.getProductItemId());
      String gdnProductItemSku = null;
      if (productItem != null) {
        gdnProductItemSku =
            addProductAndItemsResponse.getMapOfItemSkuByItemCode().get(productItem.getSkuCode());
      }
      productItemBusinessPartner.setGdnProductItemSku(gdnProductItemSku);
    }
  }

  @Override
  @Deprecated
  public void create(String businessPartnerCode, Product product,
      ProductBusinessPartner productBusinessPartner) throws Exception {
    Map<String, AttributeResponse> attributes = generateAttribute(productBusinessPartner);
    boolean forceReview =
        checkProductQcReviewResponse(product.getStoreId(), product.getProductCode());
    ProductAndItemActivationRequest productLevel3Request =
        productAndItemActivationRequest(businessPartnerCode, product, productBusinessPartner,
            attributes, forceReview);
    AddProductAndItemsResponse addProductAndItemsResponse =
        this.xProductOutbound.addProductAndItems(productBusinessPartner.getStoreId(), Constants.WEB_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, productBusinessPartner.getCreatedBy(),
            productLevel3Request);
    generateProductBusinessPartner(product, productBusinessPartner, addProductAndItemsResponse);
  }

  private UpdateItemSummaryRequest generateProductLevel3UpdateSummaryRequest(
      ProductLevel3UpdateSummary productLevel3UpdateSummary) throws Exception {
    UpdateItemSummaryRequest productLevel3UpdateSummaryRequest = new UpdateItemSummaryRequest();
    productLevel3UpdateSummaryRequest.setMerchantSku(productLevel3UpdateSummary.getMerchantSku());
    productLevel3UpdateSummaryRequest.setPickupPointCode(productLevel3UpdateSummary
        .getPickupPointCode());
    productLevel3UpdateSummaryRequest.setIsLateFulfillment(productLevel3UpdateSummary
        .getLateFulfillment());
    productLevel3UpdateSummaryRequest.setPrice(new HashSet<>());
    productLevel3UpdateSummaryRequest.setItemViewConfigs(new HashSet<>());
    for (ProductLevel3Price productLevel3Price : productLevel3UpdateSummary.getPrices()) {
      PriceDTO price = new PriceDTO();
      price.setOfferPrice(productLevel3Price.getSalePrice());
      price.setListPrice(productLevel3Price.getPrice());
      price.setChannel(productLevel3Price.getChannelId());
      productLevel3UpdateSummaryRequest.getPrice().add(price);
    }
    for (ProductLevel3ViewConfig productLevel3ViewConfig : productLevel3UpdateSummary
        .getViewConfigs()) {
      ItemViewConfigDTO viewConfig = new ItemViewConfigDTO();
      viewConfig.setBuyable(productLevel3ViewConfig.getBuyable());
      viewConfig.setDiscoverable(productLevel3ViewConfig.getDisplay());
      viewConfig.setChannel(productLevel3ViewConfig.getChannelId());
      productLevel3UpdateSummaryRequest.getItemViewConfigs().add(viewConfig);
    }
    productLevel3UpdateSummaryRequest.setOff2OnChannelActive(productLevel3UpdateSummary.getOff2OnActiveFlag());
    return productLevel3UpdateSummaryRequest;
  }

  @Override
  public void update(ProductRequest product, boolean isOnlyExternal) throws Exception {
    this.xProductOutbound.updateProduct(isOnlyExternal, product);
  }

  @Override
  public ItemResponse getItem(String itemSku) throws Exception {
    return this.productLevel3Repository.getItem(itemSku);
  }

  @Override
  public ProductAndItemsResponse getProduct(String productSku) throws Exception {
    return this.productLevel3Repository.getProduct(productSku);
  }

  @SuppressWarnings("unchecked")
  private boolean validateUpdateSummary(UpdateItemSummaryRequest productLevel3UpdateSummaryRequest)
      throws Exception {
    for (PropertyDescriptor pd : Introspector.getBeanInfo(UpdateItemSummaryRequest.class)
        .getPropertyDescriptors()) {
      if (pd.getReadMethod().getReturnType() != Class.class) {
        if (pd.getReadMethod().getReturnType() == Set.class) {
          Collection<Object> sizeCheck =
              (Collection<Object>) pd.getReadMethod().invoke(productLevel3UpdateSummaryRequest);
          if (sizeCheck.size() != 0) {
            return true;
          }
        } else {
          if (pd.getReadMethod().invoke(productLevel3UpdateSummaryRequest) != null) {
            return true;
          }
        }
      }
    }
    return false;
  }

  private boolean validateSynchronizeStock(String businessPartnerCode) throws Exception {
    ProfileResponse businessPartner =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    return GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI.equals(businessPartner.getCompany()
        .getInventoryFulfillment());
  }

  private boolean isPurchaseOrderPurchaseTerm(ProfileResponse businessPartner) throws Exception {
    return GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER.equals(businessPartner.getCompany()
        .getPurchaseTerm());
  }

  @Override
  public ProductLevel3Summary updateSummary(String businessPartnerCode, String gdnSku,
      ProductLevel3UpdateSummary request) throws Exception {
    UpdateItemSummaryRequest productLevel3UpdateSummaryRequest =
        generateProductLevel3UpdateSummaryRequest(request);
    ItemSummaryResponse savedProductData = this.productLevel3Repository.findSummaryByGdnSku(gdnSku);
    if (savedProductData.getArchived()) {
      throw new InvalidDataStateException(UPDATE_ARCHIVED_ERROR);
    }
    if (Objects.nonNull(savedProductData.getVersion()) && Objects.nonNull(request.getVersion())
        && request.getVersion() <= savedProductData.getVersion()) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.INVALID_VERSION_ERROR);
    }
    if (savedProductData.isFreeSample() && (request.getViewConfigs().get(0).getBuyable() || request
      .getViewConfigs().get(0).getDisplay())) {
      LOGGER.error("Error updating free sample product : {} ", savedProductData.getItemSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ErrorMessages.FREE_SAMPLE_MUST_NOT_BE_BUYABLE_OR_DISCOVERABLE);
    }
    boolean priceValidated = false;
    String categoryCode = ofNullable(savedProductData.getMasterCatalog()).map(MasterCatalogDTO::getCategory)
        .map(CategoryDTO::getCategoryCode).orElse(StringUtils.EMPTY);
    if (CollectionUtils.isNotEmpty(request.getPrices()) && (savedProductData.getPrice().stream()
      .noneMatch(price -> request.getPrices().iterator().next().getSalePrice()
        .equals(price.getOfferPrice()))))
    {
      validateDiscountPrice(gdnSku, request.getPrices().get(0).getSalePrice(), categoryCode,
          productLevel3UpdateSummaryRequest.getPickupPointCode());
      priceValidated = true;
    }
    ItemSummaryResponse updatedProductData = null;
    ProductLevel3Summary savedProductLevel3Summary = null;
    try {
      savedProductLevel3Summary = generateProductLevel3SummaryForUpdateHistory(businessPartnerCode, savedProductData);
      WholesalePriceSkuResponse wholesalePriceSkuResponse =
          productPricingOutbound.getWholesalePrice(savedProductData.getItemSku(), savedProductData.getPickupPointCode());
      if (Objects.nonNull(request.getWholesalePriceActivated()) && request.getWholesalePriceActivated()) {
        Boolean isInSameThreshold =
            isSameThreshold(request, savedProductData.getMasterCatalog().getCategory().getCategoryCode(),
                savedProductData.getItemSku(), wholesalePriceSkuResponse);
        productLevel3UpdateSummaryRequest.setWholesalePriceActivated(isInSameThreshold);
      } else {
        productLevel3UpdateSummaryRequest.setWholesalePriceActivated(request.getWholesalePriceActivated());
      }

      updateWholesaleFlagInPricing(savedProductData.getItemSku(),
          productLevel3UpdateSummaryRequest.getWholesalePriceActivated(), wholesalePriceSkuResponse, savedProductData.getPickupPointCode());

      if (validateUpdateSummary(productLevel3UpdateSummaryRequest) && checkIfItemSummaryChanged(
          productLevel3UpdateSummaryRequest, savedProductData)) {
        updatedProductData =
            this.productLevel3Repository.updateSummary(businessPartnerCode, gdnSku, productLevel3UpdateSummaryRequest);
        if (priceValidated) {
          updateDiscountPriceInCampaign(gdnSku,
              updatedProductData.getPrice().stream().findFirst().get().getOfferPrice(), categoryCode,
              productLevel3UpdateSummaryRequest.getPickupPointCode());
        }
      }
    } catch (Exception e) {
      if (priceValidated) {
        LOGGER.error("Exception while updating details in xproduct , itemSku : {} ", gdnSku, e);
      }
      throw e;
    }
    if (StringUtils.isNotBlank(request.getPickupPointCode()) && isPickupPointChanged(
        savedProductData.getPickupPointCode(), request.getPickupPointCode())) {
        List<WebInventoryUpdatePickupPointRequestDTO> webInventoryUpdatePickupPointRequestDTOList =
            getWebInventoryUpdatePickupPointRequestDTOS(request, savedProductData);
        inventoryOutbound.updatePickupPoint(generateMandatoryRequestParam(),
            new ListRequestDTO<>(webInventoryUpdatePickupPointRequestDTOList));
    }
    if (request.getDeltaStock() != null) {
      this.productLevel3InventoryService.updateStock(businessPartnerCode, gdnSku,
          request.getDeltaStock());
    }
    if (request.getSynchronizeStock() != null && validateSynchronizeStock(businessPartnerCode)) {
      this.productLevel3InventoryService.updateSyncStockByBusinessPartnerCodeAndGdnSku(
        businessPartnerCode, gdnSku, request.getSynchronizeStock(), Collections.singletonList(
          ItemSkuPickupPointSyncStockDto.builder().itemSku(gdnSku)
            .syncStock(request.getSynchronizeStock()).pickupPointCode(request.getPickupPointCode())
            .build()));
    }
    updatedProductData = this.productLevel3Repository.findSummaryByGdnSku(gdnSku);
    ProductLevel3Summary updatedProductLevel3Summary =
        generateProductLevel3SummarySingle(businessPartnerCode, gdnSku, updatedProductData);
    updatedProductLevel3Summary
        .setWholesalePriceActivated(productLevel3UpdateSummaryRequest.getWholesalePriceActivated());
    UpdateProductItemLevel3Model savedData = updateProductItemLevel3ModelConverter
        .convertFromProductLevel3Summary(savedProductLevel3Summary);
    UpdateProductItemLevel3Model updatedData = updateProductItemLevel3ModelConverter
        .convertFromProductLevel3Summary(updatedProductLevel3Summary);
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(businessPartnerCode, gdnSku, savedData, updatedData, request.getAccessChannel(),
            updatedProductLevel3Summary.getProductSku(), updatedProductLevel3Summary.getItemName(), false,
            StringUtils.EMPTY);
    return updatedProductLevel3Summary;
  }

  private List<WebInventoryUpdatePickupPointRequestDTO> getWebInventoryUpdatePickupPointRequestDTOS(
      ProductLevel3UpdateSummary request, ItemSummaryResponse savedProductData) {
    List<WebInventoryUpdatePickupPointRequestDTO> webInventoryUpdatePickupPointRequestDTOList = new ArrayList<>();
    WebInventoryUpdatePickupPointRequestDTO webInventoryUpdatePickupPointRequestDTO =
        new WebInventoryUpdatePickupPointRequestDTO();
    webInventoryUpdatePickupPointRequestDTO.setWebItemSku(savedProductData.getItemSku());
    webInventoryUpdatePickupPointRequestDTO.setWebMerchantCode(savedProductData.getMerchantCode());
    webInventoryUpdatePickupPointRequestDTO.setPickupPointCode(savedProductData.getPickupPointCode());
    webInventoryUpdatePickupPointRequestDTO.setNewPickupPointCode(request.getPickupPointCode());
    webInventoryUpdatePickupPointRequestDTOList.add(webInventoryUpdatePickupPointRequestDTO);
    return webInventoryUpdatePickupPointRequestDTOList;
  }

  @Override
  public ApiErrorCode productQuickEdit(String storeId, String productSku,
      ProductLevel3QuickEditRequest productLevel3QuickEditRequest, List<String> stockUpdateFailedItemSkus) throws Exception {
    Map<String, ItemSummaryResponse> itemSummaryResponseMap = new HashMap<>();
    Map<String, Boolean> wholeSaleActivatedMap = new HashMap<>();
    Map<String, ItemSummaryResponse> itemSkuResponseMap = new HashMap<>();
    CommonUtils.sanitizeSellerSku(sanitizeProductNameAndSellerSku, productLevel3QuickEditRequest);
    for (QuickEditRequest quickEditRequest : productLevel3QuickEditRequest.getQuickEditRequests()) {
      ItemSummaryResponse savedProductData =
          this.productLevel3Repository.findSummaryByGdnSku(quickEditRequest.getItemSku());
      if (Objects.nonNull(savedProductData.getVersion()) && Objects.nonNull(quickEditRequest.getVersion())
          && quickEditRequest.getVersion() <= savedProductData.getVersion()) {
        LOGGER.error("Version mismatch error. oldVersion : {}, newVersion : {} ", savedProductData.getVersion(),
            quickEditRequest.getVersion());
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.INVALID_VERSION_ERROR);
      }
      if ((savedProductData.isFreeSample()) && ((ProductLevel3Status.ONLINE
        .equals(quickEditRequest.getStatus())) || (
        Objects.nonNull(quickEditRequest.getOff2OnActiveFlag()) && Boolean.TRUE
          .equals(quickEditRequest.getOff2OnActiveFlag())))) {
        LOGGER.error("Error updating free sample product : {} ", savedProductData.getProductSku());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.FREE_SAMPLE_CANNOT_BE_SET.getDesc());
      }
      Double minimumPrice = getMinimumPrice(storeId).doubleValue();
      if (Double.compare(quickEditRequest.getPrice().getSalePrice(), minimumPrice) < 0
        || Double.compare(quickEditRequest.getPrice().getPrice(), minimumPrice) < 0) {
        log.error("Price lesser than allowed minimum price : {}, itemSku : {} ", minimumPrice,
          quickEditRequest.getItemSku());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.MINIMUM_PRICE_VALUE_INVALID.getDesc() + minimumPrice);
      }
      itemSummaryResponseMap.put(savedProductData.getItemSku(), savedProductData);
      if (isSalesPriceChangedOrPriceEditDisabled(savedProductData.getPrice(),
          quickEditRequest.getPrice().getSalePrice(), savedProductData.isPriceEditDisabled())) {
        try {
          this.validateDiscountPrice(quickEditRequest.getItemSku(), quickEditRequest.getPrice().getSalePrice(),
              savedProductData.getMasterCatalog().getCategory().getCategoryCode(),
              quickEditRequest.getPickupPointCode());
          itemSkuResponseMap.put(quickEditRequest.getItemSku(), savedProductData);
        } catch (ApplicationRuntimeException ex) {
          LOGGER.error("Error on validate of price lock feature for campaign, item : {}, error : {}, error - ",
              quickEditRequest.getItemSku(), ex.getErrorMessage(), ex);
          return ApiErrorCode.PRICE_UPDATE_FAILED;
        }
      }
    }
    try {
      updateItemListing(itemSummaryResponseMap, wholeSaleActivatedMap, productLevel3QuickEditRequest, productSku,
          storeId, stockUpdateFailedItemSkus);
    } catch (Exception e) {
      LOGGER.error("Exception while updating details in x-product , productSku : {} ", productSku, e);
      throw e;
    }
    return null;
  }

  private void updateCampaignPriceOnFailureCase(Map<String, ItemSummaryResponse> itemSummaryResponseMap) throws Exception {
    for (Map.Entry<String, ItemSummaryResponse> itemSku : itemSummaryResponseMap.entrySet()) {
      this.updateDiscountPriceInCampaign(itemSku.getKey(),
          itemSku.getValue().getPrice().stream().findFirst().get().getOfferPrice(),
          itemSku.getValue().getMasterCatalog().getCategory().getCategoryCode(), itemSku.getValue().getPickupPointCode());
    }
  }

  private void updateItemListing(Map<String, ItemSummaryResponse> itemSummaryResponseMap,  Map<String, Boolean> wholeSaleActivatedMap,
      ProductLevel3QuickEditRequest productLevel3QuickEditRequest, String productSku, String storeId, List<String> stockUpdateFailedItemSkus) throws Exception {
    List<QuickEditUpdateRequest> quickEditUpdateRequests = new ArrayList<>();
    ProfileResponse businessPartner = this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
        itemSummaryResponseMap.entrySet().iterator().next().getValue().getMerchantCode());
    Map<String, ProductLevel3Summary> productLevel3SummaryMap = new HashMap<>();
    ProductType productType = null;
    for (QuickEditRequest quickEditRequest : productLevel3QuickEditRequest.getQuickEditRequests()) {
      ItemSummaryResponse itemSummaryResponse = itemSummaryResponseMap.get(quickEditRequest.getItemSku());
      if (Objects.nonNull(itemSummaryResponse)) {
        productLevel3SummaryMap.put(itemSummaryResponse.getItemSku(),
            generateProductLevel3SummaryForUpdateHistory(itemSummaryResponse.getMerchantCode(), itemSummaryResponse));
        Boolean wholesaleActivated = validateAndUpdateWholesaleFlag(storeId, quickEditRequest, itemSummaryResponse);
        if (productLevel3Helper
            .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, wholesaleActivated)) {
          QuickEditUpdateRequest quickEditUpdateRequest = ConverterUtil.toQuickEditRequest(quickEditRequest);
          quickEditUpdateRequest.setWholeSaleActivated(wholesaleActivated);
          quickEditUpdateRequests.add(quickEditUpdateRequest);
          productType = itemSummaryResponse.getProductType();
          wholeSaleActivatedMap.put(quickEditRequest.getItemSku(), wholesaleActivated);
        }
      }
    }
    if (CollectionUtils.isNotEmpty(quickEditUpdateRequests)) {
      xProductOutbound.updateItemListing(productSku, productType, quickEditUpdateRequests);

      quickEditUpdateRequests.forEach(request -> {
        if (isSalesPriceChangedOrPriceEditDisabled(
          itemSummaryResponseMap.get(request.getItemSku()).getPrice(),
          request.getPrice().getOfferPrice(),
          itemSummaryResponseMap.get(request.getItemSku()).isPriceEditDisabled())) {
          try {
            updateDiscountPriceInCampaign(request.getItemSku(), request.getPrice().getOfferPrice(),
              itemSummaryResponseMap.get(request.getItemSku()).getMasterCatalog().getCategory()
                .getCategoryCode(), request.getPickupPointCode());
          } catch (Exception e) {
            log.error("Error While updating Price in campaign for Item : {} ", request.getItemSku(),
              e);
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorCategory.VALIDATION.getCode());
          }
        }
      });
      if (Objects.nonNull(quickEditUpdateRequests.get(0).getOff2OnActiveFlag())) {
        ItemSummaryResponse itemSummary = itemSummaryResponseMap.get(quickEditUpdateRequests.get(0).getItemSku());
        if (Objects.nonNull(itemSummary) && itemSummary.isOff2OnChannelActive() != quickEditUpdateRequests.get(0)
            .getOff2OnActiveFlag().booleanValue()) {
          updateOff2OnActiveFlag(itemSummary.getMerchantCode(), itemSummary.getProductSku(),
              itemSummary.getProductName(), quickEditUpdateRequests.get(0).getOff2OnActiveFlag());
        }
      }
    }

    for (QuickEditRequest quickEditRequest : productLevel3QuickEditRequest.getQuickEditRequests()) {
      ItemSummaryResponse itemSummaryResponse = itemSummaryResponseMap.get(quickEditRequest.getItemSku());
      if (Objects.nonNull(itemSummaryResponse)) {
        validateAndUpdateStockAndPickupPoint(quickEditRequest, itemSummaryResponse, businessPartner, stockUpdateFailedItemSkus);
        generateProductHistoryForSummary(itemSummaryResponse, wholeSaleActivatedMap.get(quickEditRequest.getItemSku()),
            productLevel3SummaryMap.get(itemSummaryResponse.getItemSku()));
        if (ConverterUtil.isProductStatusChange(itemSummaryResponse.isBuyable(), itemSummaryResponse.isDiscoverable(),
            quickEditRequest.getStatus().name())) {
          LOGGER.debug("Send notification if product status is changed");
          productNotificationService.sendNotificationForProductStatus(businessPartner.getBusinessPartnerCode(),
              itemSummaryResponse.getGeneratedItemName(), quickEditRequest.getStatus().name());
        }
      }
    }
  }

  private Boolean validateAndUpdateWholesaleFlag(String storeId, QuickEditRequest quickEditRequest,
      ItemSummaryResponse savedProductData) throws Exception {
    WholesalePriceSkuResponse wholesalePriceSkuResponse =
        productPricingOutbound.getWholesalePrice(quickEditRequest.getItemSku(), savedProductData.getPickupPointCode());
    if (Boolean.TRUE.equals(quickEditRequest.getWholeSaleActivated())) {
      ItemRequest itemRequest = new ItemRequest();
      itemRequest.setWholesalePriceActivated(quickEditRequest.getWholeSaleActivated());
      itemRequest.setPrice(ConverterUtil.setOfferPrice(Arrays.asList(quickEditRequest.getPrice())));
      List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests =
          wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(
              key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
              .collect(Collectors.toList());
      wholesaleValidationUtil
          .validateWholesaleConfigOnUpdate(savedProductData.getMasterCatalog().getCategory().getCategoryCode(),
              productItemWholesalePriceRequests, itemRequest, getMinimumPrice(storeId), quickEditRequest.getItemSku(),
              quickEditRequest.getWholeSaleActivated(), null);
      updateWholesaleFlagInPricing(savedProductData.getItemSku(), itemRequest.getWholesalePriceActivated(),
          wholesalePriceSkuResponse, savedProductData.getPickupPointCode());
      return itemRequest.getWholesalePriceActivated();
    } else {
      updateWholesaleFlagInPricing(savedProductData.getItemSku(), quickEditRequest.getWholeSaleActivated(),
          wholesalePriceSkuResponse, savedProductData.getPickupPointCode());
      return quickEditRequest.getWholeSaleActivated();
    }
  }

  private void updateOff2OnActiveFlag(String merchantCode, String productSku, String productName,
      Boolean off2OnActiveFlag) throws Exception {
    Map<String, Boolean> productSkuOff2OnMap = new HashMap<>();
    productSkuOff2OnMap.put(productSku, off2OnActiveFlag);
    xProductOutbound.updateOff2OnActiveFlagByProductSku(productSkuOff2OnMap);
    updatedProductHistoryService.createProductL3AuditLog(merchantCode, Constants.DEFAULT, productSku, productName,
        UpdateProductActivity.OFFLINE_TO_ONLINE.getDesc(), String.valueOf(!off2OnActiveFlag.booleanValue()),
        String.valueOf(off2OnActiveFlag.booleanValue()), false, StringUtils.EMPTY);
  }

  private void validateAndUpdateStockAndPickupPoint(QuickEditRequest quickEditRequest,
      ItemSummaryResponse savedProductData, ProfileResponse profileResponse, List<String> stockUpdateFailedItemSkus) throws Exception {
    if (Objects.nonNull(quickEditRequest.getDeltaStock())) {
      ApiErrorCode apiErrorCode = this.productLevel3InventoryService.updateStockV2(savedProductData.getMerchantCode(), quickEditRequest.getItemSku(),
          quickEditRequest.getDeltaStock());
      if (Objects.nonNull(apiErrorCode)) {
        stockUpdateFailedItemSkus.add(quickEditRequest.getItemSku());
      }
    }
    if (Objects.nonNull(quickEditRequest.getUseWarehouseStock()) && GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI
        .equals(profileResponse.getCompany().getInventoryFulfillment())) {
      this.productLevel3InventoryService.updateSyncStockByBusinessPartnerCodeAndGdnSku(
        savedProductData.getMerchantCode(), quickEditRequest.getItemSku(),
        quickEditRequest.getUseWarehouseStock(), Collections.singletonList(
          ItemSkuPickupPointSyncStockDto.builder().itemSku(quickEditRequest.getItemSku())
            .pickupPointCode(quickEditRequest.getPickupPointCode())
            .syncStock(quickEditRequest.getUseWarehouseStock()).build()));
    }
    if (StringUtils.isNotEmpty(quickEditRequest.getPickupPointCode()) && isPickupPointChanged(
        savedProductData.getPickupPointCode(), quickEditRequest.getPickupPointCode())) {
      this.productLevel3InventoryService
          .updatePickupPoint(savedProductData.getMerchantCode(), quickEditRequest.getItemSku(),
              quickEditRequest.getPickupPointCode());
    }
  }

  private boolean isPickupPointChanged(String oldPickupPointCode, String newPickupPointCode) {
    return !StringUtils.equals(oldPickupPointCode,newPickupPointCode);
  }


  private void generateProductHistoryForSummary(ItemSummaryResponse savedProductData, Boolean wholeSaleFlagChange,
      ProductLevel3Summary savedProductLevel3Summary) throws Exception {
    ItemSummaryResponse updatedProductData =
        this.productLevel3Repository.findSummaryByGdnSku(savedProductData.getItemSku());
    ProductLevel3Summary updatedProductLevel3Summary =
        generateProductLevel3SummarySingle(savedProductData.getMerchantCode(), savedProductData.getItemSku(),
            updatedProductData);
    updatedProductLevel3Summary.setWholesalePriceActivated(wholeSaleFlagChange);
    UpdateProductItemLevel3Model savedData =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Summary(savedProductLevel3Summary);
    UpdateProductItemLevel3Model updatedData =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Summary(updatedProductLevel3Summary);
    updatedData.setOff2OnActiveFlag(savedData.getOff2OnActiveFlag());
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(savedProductData.getMerchantCode(), savedProductData.getItemSku(), savedData,
            updatedData, Constants.DEFAULT, savedProductData.getProductSku(), savedProductData.getGeneratedItemName(),
            false, StringUtils.EMPTY);
  }

  private Boolean isSameThreshold(ProductLevel3UpdateSummary productLevel3UpdateSummary, String categoryCode,
      String itemSku, WholesalePriceSkuResponse wholesalePriceSkuResponse) throws Exception {
    Boolean response = null;
    WholesaleMappingResponse wholesaleMappingResponse = pcbFeign
        .getWholesaleConfigToCategory(DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, categoryCode)
        .getValue();

    if (Objects.nonNull(wholesaleMappingResponse)) {
      if (!wholesaleMappingResponse.isWholesalePriceConfigEnabled()) {
        response = productLevel3UpdateSummary.getWholesalePriceActivated();
      } else {
        try {
          if (Objects.nonNull(wholesaleMappingResponse) && Objects.nonNull(wholesalePriceSkuResponse.getWholesaleRules())) {
            WholesalePriceSkuResponse finalWholesalePriceSkuResponse = wholesalePriceSkuResponse;
            List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests =
                wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(
                    key -> new ProductItemWholesalePriceRequest(key,
                        finalWholesalePriceSkuResponse.getWholesaleRules().get(key))).collect(Collectors.toList());
            CommonUtils.validateWholesalePrice(productItemWholesalePriceRequests,
                productLevel3UpdateSummary.getPrices().get(0).getPrice(), getMinimumPrice(DEFAULT_STORE_ID),
                maxWholesalePriceRequests, itemSku);
            CommonUtils.validateWholesalePriceConfig(productItemWholesalePriceRequests,
                productLevel3UpdateSummary.getPrices().get(0).getPrice(), wholesaleMappingResponse);
          }
          response = true;
        } catch (Exception e) {
          LOGGER.error(String.format(ErrorMessages.WHOLE_SALE_VALIDATION_FAILED, itemSku), e);
          response = false;
        }
      }
    }
    return response;
  }

  private void updateWholesaleFlagInPricing(String itemSku, Boolean response,
      WholesalePriceSkuResponse wholesalePriceSkuResponse, String pickupPointCode) throws Exception {
    if (Objects.nonNull(wholesalePriceSkuResponse) && checkIsStatusChanged(response,
        wholesalePriceSkuResponse.getSkuStatus())) {
      productPricingOutbound
          .setWholesaleActivated(itemSku, Boolean.TRUE.equals(response) ? ACTIVE_STATUS : INACTIVE_STATUS,
              pickupPointCode);
    }
  }

  private boolean checkIsStatusChanged(Boolean flag, String status) {
    return (INACTIVE_STATUS.equals(status)
        && Boolean.TRUE.equals(flag)) || (ACTIVE_STATUS.equals(status) && Boolean.FALSE.equals(flag));
  }

  private boolean checkIfItemSummaryChanged(
      UpdateItemSummaryRequest updateItemSummaryRequest, ItemSummaryResponse existingItemSummary) {
    boolean isItemChanged = false;
    if(Objects.nonNull(updateItemSummaryRequest.getIsLateFulfillment()) &&
        existingItemSummary.isLateFulfillment() != updateItemSummaryRequest.getIsLateFulfillment()) {
      isItemChanged = true;
    }
    if(CollectionUtils.isNotEmpty(updateItemSummaryRequest.getItemViewConfigs()) &&
        !updateItemSummaryRequest.getItemViewConfigs().equals(existingItemSummary.getItemViewConfigs())) {
      isItemChanged = true;
    }
    if (Objects.nonNull(updateItemSummaryRequest.getMerchantSku()) && !StringUtils.equals(
        updateItemSummaryRequest.getMerchantSku(),
        ofNullable(existingItemSummary.getMerchantSku()).orElse(StringUtils.EMPTY))) {
      isItemChanged = true;
    }
    if(StringUtils.isNotBlank(updateItemSummaryRequest.getPickupPointCode()) &&
        !StringUtils.equals(updateItemSummaryRequest.getPickupPointCode(), existingItemSummary.getPickupPointCode())) {
      isItemChanged = true;
    }
    if (CollectionUtils.isNotEmpty(updateItemSummaryRequest.getPrice())) {
      Iterator priceDTOIteratorUpdate = updateItemSummaryRequest.getPrice().iterator();
      Iterator priceDTOIteratorExisting = existingItemSummary.getPrice().iterator();
      if (priceDTOIteratorExisting.hasNext()) {
        PriceDTO priceDTOUpdate = (PriceDTO) priceDTOIteratorUpdate.next();
        PriceDTO priceDTOExisting = (PriceDTO) priceDTOIteratorExisting.next();
        if (priceDTOUpdate.getListPrice() != priceDTOExisting.getListPrice()
            || priceDTOUpdate.getOfferPrice() != priceDTOExisting.getOfferPrice()) {
          isItemChanged = true;
        }
      }
    }
    if(Objects.nonNull(updateItemSummaryRequest.getOff2OnChannelActive()) &&
        existingItemSummary.isOff2OnChannelActive() != updateItemSummaryRequest.getOff2OnChannelActive()) {
      isItemChanged = true;
    }
    if (existingItemSummary.getWholesalePriceActivated() != updateItemSummaryRequest.getWholesalePriceActivated()) {
      isItemChanged = true;
    }
    return isItemChanged;
  }

  /**
   * Generate product level 3 aggregation model without category hierarchy and pickup point
   *
   * @param businessPartnerCode Business partner code to get inventory level2 data
   * @param productItem product item response from x-product
   * @return ProductLeve3Summary (aggregation model)
   * @throws Exception if occurs
   */
  private ProductLevel3Summary generateProductLevel3SummaryForUpdateHistory(
      String businessPartnerCode, ItemSummaryResponse productItem) throws Exception {
    PickupPointResponse pickupPointData = new PickupPointResponse();
    pickupPointData.setCode(productItem.getPickupPointCode());
    ProductLevel3Inventory inventory =
        this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndGdnSku(
            businessPartnerCode, productItem.getItemSku());
    if (inventory == null) {
      LOGGER.error("Not found inventory data for businessPartnerCode: {}, productItem: {}",
          businessPartnerCode, productItem);
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Not found inventory data for this sku:" + productItem.getItemSku());
    }
    return generateProductLevel3SummarySingle(productItem, null, pickupPointData, inventory);
  }

  private ProductLevel3Summary generateProductLevel3SummarySingle(String businessPartnerCode,
      String gdnSku, ItemSummaryResponse productData) throws Exception {
    List<CategoryResponse> categoryData =
        this.categoryRepository.findHierarchyByCategoryCode(
            productData.getMasterCatalog().getCategory().getCategoryCode());
    PickupPointResponse pickupPointData =
        this.pickupPointRepository.findByPickupPointCode(productData.getPickupPointCode());
    ProductLevel3Inventory inventory =
        this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndGdnSku(
            businessPartnerCode, gdnSku);
    if (inventory == null) {
      LOGGER.error(
          "Not found inventory data for businessPartnerCode: {}, gdnSku: {},\n productLevel3: {}",
          businessPartnerCode, gdnSku, productData);
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Not found inventory data for this sku:" + gdnSku);
    }
    return generateProductLevel3SummarySingle(productData, categoryData, pickupPointData, inventory);
  }

  /***
   * Method for aggregating data from X-Product, PCB, XBP, and X-Inventory X-Product data is
   * mandatory, the others will be set to null if not present
   *
   * @param productData Product Item data from X-Product
   * @param categoryData Category data from PCB
   * @param pickupPointData Pickup Point data from XBP
   * @param inventory inventory data from x-inventory
   * @return ProductLevel3Summary Product aggregation result
   * @throws Exception if there's NullPointerException occured
   */
  private ProductLevel3Summary generateProductLevel3SummarySingle(ItemSummaryResponse productData,
      List<CategoryResponse> categoryData, PickupPointResponse pickupPointData,
      ProductLevel3Inventory inventory) throws Exception {
    ProductLevel3Summary product = new ProductLevel3Summary();
    product.setItemSku(productData.getItemSku());
    product.setSkuCode(productData.getItemCode());
    product.setMerchantSku(productData.getMerchantSku());
    product.setItemName(productData.getGeneratedItemName());
    product.setCategoryCode(productData.getMasterCatalog().getCategory().getCategoryCode());
    product.setProductType(productData.getProductType().getCode());
    product.setLateFulfillment(productData.isLateFulfillment());
    product.setCreatedDate(productData.getCreatedDate());
    product.setCreatedBy(productData.getCreatedBy());
    product.setUpdatedDate(productData.getUpdatedDate());
    product.setUpdatedBy(productData.getUpdatedBy());
    product.setIsArchived(productData.getArchived());
    product.setPromoBundling(productData.isPromoBundling());
    product.setMerchantPromoDiscount(productData.isMerchantPromoDiscount());
    product.setMerchantPromoDiscountActivated(productData.isMerchantPromoDiscountActivated());
    product.setWholesalePriceActivated(productData.getWholesalePriceActivated());
    product.setForceReview(productData.isForceReview());
    product.setProductName(productData.getProductName());
    product.setProductSku(productData.getProductSku());

    if (CollectionUtils.isNotEmpty(categoryData)) {
      String[] categoryNameAndHierarchy = generateCategoryNameIdAndHierarchy(categoryData);
      product.setCategoryName(categoryNameAndHierarchy[0]);
      product.setCategoryHierarchy(categoryNameAndHierarchy[1]);
    }

    if (pickupPointData != null) {
      product.setPickupPointCode(pickupPointData.getCode());
      product.setPickupPointName(pickupPointData.getName());
    }
    product.setAvailableStockLevel1(inventory.getWarehouseAvailable());
    product.setReservedStockLevel1(inventory.getWarehouseReserved());
    product.setAvailableStockLevel2(inventory.getWebAvailable());
    product.setReservedStockLevel2(inventory.getWebReserved());
    product.setMinimumStockLevel2(inventory.getWebMinAlert());
    product.setSynchronizeStock(inventory.isWebSyncStock());
    product.setPrices(new ArrayList<>());
    product.setViewConfigs(new ArrayList<>());
    product.setImages(new ArrayList<>());
    product.setOff2OnActiveFlag(productData.isOff2OnChannelActive());
    for (PriceDTO priceData : productData.getPrice()) {
      ProductLevel3Price price = new ProductLevel3Price();
      price.setChannelId(priceData.getChannel());
      price.setPrice(priceData.getListPrice());
      price.setSalePrice(priceData.getOfferPrice());
      if (priceData.getListOfDiscountPrices() != null
          && !priceData.getListOfDiscountPrices().isEmpty()) {
        price.setDiscountAmount(priceData.getListOfDiscountPrices().get(0).getDiscountPrice());
        price.setDiscountStartDate(priceData.getListOfDiscountPrices().get(0).getStartDateTime());
        price.setDiscountEndDate(priceData.getListOfDiscountPrices().get(0).getEndDateTime());
        price.setPromotionName(priceData.getListOfDiscountPrices().get(0).getAdjustmentName());
      }
      product.getPrices().add(price);
    }
    for (ItemViewConfigDTO viewConfigData : productData.getItemViewConfigs()) {
      ProductLevel3ViewConfig viewConfig = new ProductLevel3ViewConfig();
      viewConfig.setChannelId(viewConfigData.getChannel());
      viewConfig.setDisplay(viewConfigData.isDiscoverable());
      viewConfig.setBuyable(viewConfigData.isBuyable());
      product.getViewConfigs().add(viewConfig);
    }
    for (MasterDataItemImageDTO imageData : productData.getMasterDataItemImages()) {
      ProductLevel3Image image = new ProductLevel3Image();
      image.setMainImage(imageData.isMainImage());
      image.setSequence(imageData.getSequence());
      image.setLocationPath(imageData.getLocationPath());
      product.getImages().add(image);
    }
    return product;
  }

  @Override
  public ProductLevel3Summary findSummaryByGdnSku(String businessPartnerCode, String gdnSku)
      throws Exception {
    ItemSummaryResponse productData = this.productLevel3Repository.findSummaryByGdnSku(gdnSku);
    return generateProductLevel3SummarySingle(businessPartnerCode, gdnSku, productData);
  }

  private ProductRequest generateProductRequest(ProductLevel3 product) {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setMerchantCode(product.getBusinessPartnerCode());
    productRequest.setProductCode(product.getProductCode());
    productRequest.setProductSku(product.getProductSku());
    if (product.getProductType() == 1) {
      productRequest.setProductType(ProductType.REGULAR);
    } else if (product.getProductType() == 2) {
      productRequest.setProductType(ProductType.BIG_PRODUCT);
    } else if (product.getProductType() == 3) {
      productRequest.setProductType(ProductType.BOPIS);
    }
    productRequest.setInstallationRequired(product.getInstallationRequired());
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setBrand(product.getBrand());
    masterDataProductDTO.setDescription(product.getDescription());
    masterDataProductDTO.setProductName(product.getProductName());
    masterDataProductDTO.setProductStory(product.getProductStory());
    masterDataProductDTO.setShippingWeight(product.getItems().get(0).getShippingWeight());
    masterDataProductDTO.setSpecificationDetail(product.getSpecificationDetail());
    masterDataProductDTO.setUniqueSellingPoint(product.getUniqueSellingPoint());
    masterDataProductDTO.setUrl(product.getUrl());
    masterDataProductDTO
        .setMasterDataProductAttributes(new ArrayList<MasterDataProductAttributeDTO>());
    masterDataProductDTO.setMasterDataProductImages(new ArrayList<MasterDataProductImageDTO>());
    for (ProductLevel3Image image : product.getImages()) {
      MasterDataProductImageDTO imageDTO = new MasterDataProductImageDTO();
      imageDTO.setLocationPath(image.getLocationPath());
      imageDTO.setMainImage(
        ofNullable(image).map(ProductLevel3Image::getMainImage).orElse(false));
      imageDTO.setProductCode(product.getProductCode());
      imageDTO.setSequence(image.getSequence());
      masterDataProductDTO.getMasterDataProductImages().add(imageDTO);
    }
    productRequest.setProductSpecialAttributes(new ArrayList<ProductSpecialAttributeDTO>());
    for (ProductLevel3Attribute attribute : product.getAttributes()) {
      if (Boolean.FALSE.equals(attribute.getSkuValue())) {
        if (MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE.equals(MasterDataAttributeType
            .valueOf(attribute.getAttributeType()))
            || MasterDataAttributeType.PREDEFINED_ATTRIBUTE.equals(MasterDataAttributeType
            .valueOf(attribute.getAttributeType()))) {
          MasterDataProductAttributeDTO attributeDTO = new MasterDataProductAttributeDTO();
          attributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
          attributeDTO.getMasterDataAttribute().setAttributeCode(attribute.getAttributeCode());
          attributeDTO.getMasterDataAttribute().setAttributeType(
              MasterDataAttributeType.valueOf(attribute.getAttributeType()));
          attributeDTO.getMasterDataAttribute().setAttributeName(attribute.getAttributeName());
          attributeDTO.getMasterDataAttribute().setSkuValue(attribute.getSkuValue());
          attributeDTO
              .setMasterDataProductAttributeValues(new ArrayList<MasterDataProductAttributeValueDTO>());
          for (String value : Optional.ofNullable(attribute.getValues()).orElse(Collections.emptyList())) {
            MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO =
                new MasterDataProductAttributeValueDTO();
            if (StringUtils.isBlank(value) && !attribute.isMandatory()) {
              masterDataProductAttributeValueDTO.setDescriptiveAttributeValue(Constants.DELIMITER_DASH);
            } else {
              masterDataProductAttributeValueDTO.setDescriptiveAttributeValue(value);
            }
            if (attributeDTO.getMasterDataAttribute().getAttributeType()
                .equals(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE)) {
              masterDataProductAttributeValueDTO
                  .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
            } else if (attributeDTO.getMasterDataAttribute().getAttributeType()
                .equals(MasterDataAttributeType.PREDEFINED_ATTRIBUTE)) {
              PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO =
                  new PredefinedAllowedAttributeValueDTO();
              if (StringUtils.isBlank(value) && !attribute.isMandatory()) {
                predefinedAllowedAttributeValueDTO.setValue(Constants.DELIMITER_DASH);
              } else {
                predefinedAllowedAttributeValueDTO.setValue(value);
              }
              masterDataProductAttributeValueDTO
                  .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
              masterDataProductAttributeValueDTO
                  .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO);
            }
            attributeDTO.getMasterDataProductAttributeValues().add(
                masterDataProductAttributeValueDTO);
          }
          masterDataProductDTO.getMasterDataProductAttributes().add(attributeDTO);
        }
      } else {
        List<String> attributeValues =
          ofNullable(attribute.getValues()).filter(CollectionUtils::isNotEmpty).stream().findFirst()
            .orElse(Collections.singletonList(StringUtils.EMPTY));
        ProductSpecialAttributeDTO productSpecialAttributeDTO = new ProductSpecialAttributeDTO();
        productSpecialAttributeDTO.setAttributeCode(attribute.getAttributeCode());
        if (StringUtils.isBlank(attributeValues.get(0)) && Boolean.FALSE.equals(attribute.isMandatory())) {
          productSpecialAttributeDTO.setAttributeValue(Constants.DELIMITER_DASH);
        } else {
          productSpecialAttributeDTO.setAttributeValue(attributeValues.get(0));
        }
        productSpecialAttributeDTO.setAttributeName(attribute.getAttributeName());
        productRequest.getProductSpecialAttributes().add(productSpecialAttributeDTO);
      }
    }
    productRequest.setMasterDataProduct(masterDataProductDTO);
    productRequest.setFreeSample(product.isFreeSample());
    productRequest.setOff2OnChannelActive(product.isOff2OnChannelActive());
    CommonUtils.setMissingFieldsForPureInstoreProduct(product, productRequest, instoreNewFlowEnabled);
    RequestHelper.setVideoAddEditRequestForXProduct(product, productRequest);
    return productRequest;
  }

  private ItemRequest generateItemRequest(ProductItemLevel3 item, String productSku,
      List<ProductLevel3Attribute> attributes) {
    ItemRequest itemRequest = new ItemRequest();
    itemRequest.setItemCode(item.getSkuCode());
    itemRequest.setItemSku(item.getItemSku());
    itemRequest.setLateFulfillment(item.getLateFulfillment());
    itemRequest.setMerchantSku(item.getMerchantSku());
    itemRequest.setPickupPointCode(item.getPickupPointCode());
    itemRequest.setProductSku(productSku);
    itemRequest.setShippingWeight(item.getShippingWeight());
    itemRequest.setOff2OnChannelActive(item.getOff2OnActiveFlag());
    itemRequest.setWholesalePriceActivated(item.getWholesalePriceActivated());
    itemRequest.setFreeSample(item.getFreeSample());
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    masterDataItemDTO.setDangerousLevel(item.getDangerousGoodsLevel());
    masterDataItemDTO.setGeneratedItemName(item.getItemName());
    masterDataItemDTO.setItemDeliveryWeight(item.getShippingWeight());
    masterDataItemDTO.setItemWeight(item.getWeight());
    masterDataItemDTO.setItemHeight(item.getHeight());
    masterDataItemDTO.setItemLength(item.getLength());
    masterDataItemDTO.setItemWidth(item.getWidth());
    masterDataItemDTO.setSkuCode(item.getSkuCode());
    masterDataItemDTO.setUpcCode(item.getUpcCode());
    masterDataItemDTO
        .setMasterDataItemAttributeValues(new ArrayList<MasterDataItemAttributeValueDTO>());
    masterDataItemDTO.setMasterDataItemImages(new ArrayList<MasterDataItemImageDTO>());
    if(CollectionUtils.isNotEmpty(item.getPrices())) {
      itemRequest.setPrice(new HashSet<>());
      for (ProductLevel3Price price : item.getPrices()) {
        PriceRequest priceRequest = new PriceRequest();
        priceRequest.setListPrice(price.getPrice());
        priceRequest.setOfferPrice(price.getSalePrice());
        priceRequest.setChannel(price.getChannelId());
        itemRequest.getPrice().add(priceRequest);
      }
    }
    for (ProductLevel3Attribute attribute : attributes) {
      if (Objects.nonNull(attribute.getSkuValue()) && !attribute.getSkuValue()) {
        if (MasterDataAttributeType.DEFINING_ATTRIBUTE.equals(MasterDataAttributeType
            .valueOf(attribute.getAttributeType()))) {
          MasterDataItemAttributeValueDTO attributeDTO = new MasterDataItemAttributeValueDTO();
          attributeDTO.setAttributeValue(attribute.getValues().get(0));
          attributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
          attributeDTO.getMasterDataAttribute().setAttributeCode(attribute.getAttributeCode());
          attributeDTO.getMasterDataAttribute().setAttributeType(
              MasterDataAttributeType.valueOf(attribute.getAttributeType()));
          attributeDTO.getMasterDataAttribute().setAttributeName(attribute.getAttributeName());
          attributeDTO.getMasterDataAttribute().setSkuValue(attribute.getSkuValue());
          masterDataItemDTO.getMasterDataItemAttributeValues().add(attributeDTO);
        }
      }
    }
    for (ProductLevel3Image image : item.getImages()) {
      MasterDataItemImageDTO img = new MasterDataItemImageDTO();
      img.setLocationPath(image.getLocationPath());
      img.setMainImage(image.getMainImage());
      img.setSequence(image.getSequence());
      masterDataItemDTO.getMasterDataItemImages().add(img);
    }
    itemRequest.setMasterDataItem(masterDataItemDTO);
    itemRequest.setItemViewConfigs(new HashSet<ItemViewConfigRequest>());
    for (ProductLevel3ViewConfig viewConfig : item.getViewConfigs()) {
      ItemViewConfigRequest itemViewConfig = new ItemViewConfigRequest();
      itemViewConfig.setBuyable(viewConfig.getBuyable());
      itemViewConfig.setDiscoverable(viewConfig.getDisplay());
      itemViewConfig.setChannel(viewConfig.getChannelId());
      itemRequest.getItemViewConfigs().add(itemViewConfig);
    }
    return itemRequest;
  }

  @Override
  public ProductLevel3DTO update(ProductLevel3 request, Integer deltaStock, Integer minimumStock, boolean isOnlyExternal, boolean hasOrder, boolean updateLogistics)
      throws Exception {
    ProductLevel3DTO productLevel3DTO = new ProductLevel3DTO();
    ProductLevel3DetailResponse savedProduct = this.getL3DetailByProductSku(request.getProductSku(), false, true, new ArrayList<>(), false);
    ApiErrorCode apiErrorCode = checkProductStatus(savedProduct.getProductL3Response());
    if (Objects.nonNull(apiErrorCode)) {
      productLevel3DTO.setApiErrorCode(apiErrorCode);
      return productLevel3DTO;
    }
    request.setFreeSample(savedProduct.isFreeSample());
    request.setOff2OnChannelActive(savedProduct.isOff2OnChannelActive());
    if (hasOrder && !StringUtils.equals(request.getProductName(), savedProduct.getProductName())) {
      LOGGER.warn("Product has an order so product name change not allowed for product sku : {}",
          request.getProductSku());
      request.setProductName(savedProduct.getProductName());
    }
    String businessPartnerCode = request.getBusinessPartnerCode();
    ProfileResponse profileResponse =
      businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    if (CommonUtils.validateStockIncrementForSellerPenalty(profileResponse,
      sellerPenaltyEnabledPhase2, deltaStock)) {
      log.error("Stock update failed:Not allowed to increase stock due to seller penalty "
        + "restrictions for item sku : {} ", deltaStock);
      productLevel3DTO.setApiErrorCode(ApiErrorCode.SELLER_PENALTY_RESTRICTION);
      return productLevel3DTO;
    }
    boolean checkProductForReview = Boolean.valueOf(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.CHECK_PRODUCT_FOR_REVIEW).getValue());
    if (!checkProductForReview) {
      return update(request, deltaStock, minimumStock, isOnlyExternal);
    }
    String gdnSku = request.getItems().get(0).getItemSku();
    request.setProductEditable(savedProduct.isProductEditable());
    ProductLevel3UpdateRequest product = toProductLevel3UpdateRequest(request);
    if (!updateLogistics) {
      List<ProductLevel3Logistics> productLevel3LogisticsRequest = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(savedProduct.getProductLevel3Logistics())) {
        for (ProductItemLevel3LogisticResponse productItemLevel3LogisticResponse : savedProduct
            .getProductLevel3Logistics()) {
          ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
          BeanUtils.copyProperties(productItemLevel3LogisticResponse, productLevel3Logistics);
          productLevel3LogisticsRequest.add(productLevel3Logistics);
        }
        product.setProductLevel3LogisticsRequest(productLevel3LogisticsRequest);
      }
    } else {
      if (CollectionUtils.isNotEmpty(request.getItems().get(0).getLogistics())) {
        product.setProductLevel3LogisticsRequest(request.getItems().get(0).getLogistics());
      }
    }
    apiErrorCode = this.updateLogistics(product, isOnlyExternal, savedProduct, false, false, migrateProductInOtherLogisticUpdateFlow);
    if (Objects.nonNull(apiErrorCode)) {
      productLevel3DTO.setApiErrorCode(apiErrorCode);
      return productLevel3DTO;
    }

    ProductAndItemsResponse productData = this.productLevel3Repository.findDetailByGdnSku(gdnSku);
    ProductLevel3 savedProductData =
        generateProductLevel3(productData, null, null, null, null, null, new HashMap<>(), new ArrayList<>(), false);
    savedProductData.getItems().get(0).setPickupPointCode(productData.getItems().get(0).getPickupPointCode());

    if (!StringUtils.equals(request.getItems().get(0).getPickupPointCode(),
        savedProductData.getItems().get(0).getPickupPointCode())) {
      this.productLevel3InventoryService
          .updatePickupPoint(request.getBusinessPartnerCode(), request.getItems().get(0).getItemSku(),
              request.getItems().get(0).getPickupPointCode());
      this.xProductOutbound.updatePickupPointCodes(
          new com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest(request.getProductSku(), true, null,
              Arrays.asList(new PickupPointUpdateItemRequest(gdnSku, request.getItems().get(0).getPickupPointCode()))));
      this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, gdnSku, request.getProductSku(),
          request.getItems().get(0).getItemName(), UpdateProductActivity.PICK_POINT_CODE.getDesc(),
          String.valueOf(savedProductData.getItems().get(0).getPickupPointCode()),
          String.valueOf(request.getItems().get(0).getPickupPointCode()), false, StringUtils.EMPTY);
    }
    if (checkItemChange(request, savedProductData)) {
      ItemRequest itemRequest =
          generateItemRequest(savedProductData.getItems().get(0), savedProductData.getProductSku(),
              savedProductData.getAttributes());
      itemRequest.setMerchantSku(request.getItems().get(0).getMerchantSku());
      itemRequest.setItemViewConfigs(new HashSet<ItemViewConfigRequest>());
      boolean initialBuyable = false;
      boolean initialDisplayable = false;
      boolean finalBuyable = false;
      boolean finalDisplayable= false;
      for (ProductLevel3ViewConfig viewConfig : request.getItems().get(0).getViewConfigs()) {
        ItemViewConfigRequest itemViewConfig = new ItemViewConfigRequest();
        finalBuyable = viewConfig.getBuyable();
        itemViewConfig.setBuyable(viewConfig.getBuyable());
        finalDisplayable = viewConfig.getDisplay();
        itemViewConfig.setDiscoverable(viewConfig.getDisplay());
        itemViewConfig.setChannel(viewConfig.getChannelId());
        itemRequest.getItemViewConfigs().add(itemViewConfig);
      }
      for (ProductLevel3ViewConfig viewConfig : savedProductData.getItems().get(0).getViewConfigs()) {
        initialBuyable = viewConfig.getBuyable();
        initialDisplayable = viewConfig.getDisplay();
      }
      ItemResponse itemResponse = this.xProductOutbound.updateItem(isOnlyExternal, false, false, itemRequest);
      String existingMerchantSku =
          StringUtils.isNotEmpty(savedProductData.getItems().get(0).getMerchantSku()) ? savedProductData.getItems().get(0).getMerchantSku() : StringUtils.EMPTY;
      String requestMerchantSku =
          StringUtils.isNotEmpty(request.getItems().get(0).getMerchantSku()) ? request.getItems().get(0).getMerchantSku() : StringUtils.EMPTY;
      if (!StringUtils.equals(existingMerchantSku, requestMerchantSku)) {
        this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, gdnSku, request.getProductSku(),
            request.getItems().get(0).getItemName(), UpdateProductActivity.MERCHANT_SKU.getDesc(),
            savedProductData.getItems().get(0).getMerchantSku(), request.getItems().get(0).getMerchantSku(), false,
            StringUtils.EMPTY);
      }
      if (initialBuyable != finalBuyable) {
        this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, gdnSku, request.getProductSku(),
            request.getItems().get(0).getItemName(), UpdateProductActivity.BUYABLE.getDesc(),
            String.valueOf(initialBuyable), String.valueOf(finalBuyable), false, StringUtils.EMPTY);
      }
      if (initialDisplayable != finalDisplayable) {
        this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, gdnSku, request.getProductSku(),
            request.getItems().get(0).getItemName(), UpdateProductActivity.DISPLAYABLE.getDesc(),
            String.valueOf(initialDisplayable), String.valueOf(finalDisplayable), false, StringUtils.EMPTY);
      }
    }

    request.setOff2OnChannelActive(savedProduct.isOff2OnChannelActive());
    request.setVersion(null);
    ProductEditValidationDTO productEditValidationDTO =
        productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(request, new EditProductResponse(), profileResponse,
            false, null, new ArrayList<>(), new ProductL3UpdateRequest(),false, isOnlyExternal);
    updateEditInfo(request, isOnlyExternal, true, false, profileResponse, false,
      Optional.ofNullable(productEditValidationDTO).map(ProductEditValidationDTO::getProductL3Response).orElse(null)
      , false, null, null);
    return productLevel3DTO;
  }

  private ApiErrorCode updateLogisticsOption(List<ProductLevel3Logistics> logistics,
      ProductLevel3DetailResponse savedProduct, String productName) throws Exception {
    ApiErrorCode apiErrorCode;
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(savedProduct.getProfileResponse().getBusinessPartnerCode());
    Set<String> uniquePickupPointCodes = addFbbPickupPointCodes(savedProduct);
    pickupPointFilterRequest.setCodes(uniquePickupPointCodes);
    CommonUtils.setWaitingDeletionForDeletePickupPoint(setWaitingDeletionForDeletePickupPoint,
        pickupPointFilterRequest);
    List<PickupPointResponse> pickupPointResponses =
        Optional.ofNullable(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
            .orElse(Collections.emptyList());

    List<PickupPointResponse> fbbPickupPointResponses = new ArrayList<>();
    if (fbbPickupPointCodesLogisticsUpdateValidationChange) {
      fbbPickupPointResponses =
          pickupPointResponses.stream().filter(PickupPointResponse::isFbbActivated).collect(Collectors.toList());
      pickupPointResponses = pickupPointResponses.stream().filter(Predicate.not(PickupPointResponse::isFbbActivated))
          .collect(Collectors.toList());
    }

    boolean hasPickupPointWithoutPinPoint = pickupPointResponses.stream().anyMatch(
        pickupPointDTO -> Objects.isNull(pickupPointDTO.getGeolocation()) || StringUtils.isBlank(
            pickupPointDTO.getGeolocation().getPlaceId()));
    boolean fbbPickupPointWithoutPinPoint = false;

    if (CollectionUtils.isNotEmpty(fbbPickupPointResponses)) {
      fbbPickupPointWithoutPinPoint = RequestHelper.isFbbPickupPointWithoutPinPoint(fbbPickupPointResponses);
      log.debug("Fbb pickup point without pinpoint : {} for productSku : {} ", fbbPickupPointWithoutPinPoint,
          savedProduct.getProductSku());
    }

    List<ProductItemLevel3LogisticResponse> productLevel3Logistics = savedProduct.getProductLevel3Logistics();
    List<String> existingLogisticsOptions = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productLevel3Logistics)) {
      existingLogisticsOptions =
          productLevel3Logistics.stream().filter(ProductItemLevel3LogisticResponse::isSelected)
              .map(ProductItemLevel3LogisticResponse::getLogisticProductCode).collect(Collectors.toList());
    }
    List<String> updatedLogisticsOptions = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(logistics)) {
      updatedLogisticsOptions =
          logistics.stream().filter(ProductLevel3Logistics::isSelected)
              .map(ProductLevel3Logistics::getLogisticProductCode).collect(Collectors.toList());
    }
    if (hasPickupPointWithoutPinPoint || fbbPickupPointWithoutPinPoint) {
      boolean hasSelectedPickupPointWithoutPinPoint =
          productLevel3Logistics.stream().filter(ProductItemLevel3LogisticResponse::isRequiredLongLat)
              .map(ProductItemLevel3LogisticResponse::getLogisticProductCode).anyMatch(
              updatedLogisticsOptions::contains);
      if (hasSelectedPickupPointWithoutPinPoint) {
        apiErrorCode = ApiErrorCode.INVALID_LOGISTICS_OPTION_SELECTED;
        return apiErrorCode;
      }
    }
    if (isLogisticOptionChanged(updatedLogisticsOptions, existingLogisticsOptions)) {
      int size = Integer.valueOf(productSystemParameterService
          .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE).getValue());
      List<List<String>> itemSkuPartition = Lists.partition(savedProduct.getItemSkus(), size);
      for (List<String> itemSkus : itemSkuPartition) {
        boolean isSuccess = this.productLevel3LogisticsService
            .saveLogisticsByItemSku(itemSkus, savedProduct.getBusinessPartnerCode(), logistics, true);
        if (!isSuccess) {
          apiErrorCode = ApiErrorCode.LOGISTICS_DATA_NOT_SAVED;
          return apiErrorCode;
        }
        updatedProductHistoryService.createProductL3AuditLog(savedProduct.getBusinessPartnerCode(), Constants.DEFAULT,
            savedProduct.getProductSku(), productName, UpdateProductActivity.LOGISTIC.getDesc(),
            String.valueOf(existingLogisticsOptions), String.valueOf(updatedLogisticsOptions), false, StringUtils.EMPTY);
      }
    }
    return null;
  }

  private Set<String> addFbbPickupPointCodes(ProductLevel3DetailResponse savedProduct) {
    Set<String> uniquePickupPointCodes = new HashSet<>(savedProduct.getPickupPointCodes());
    if (addFbbPickupPointCodesForLogisticsUpdateCheck) {
      if (CollectionUtils.isNotEmpty(savedProduct.getFbbPickupPointCodes())) {
        uniquePickupPointCodes.addAll(savedProduct.getFbbPickupPointCodes());
      }
    }
    return uniquePickupPointCodes;
  }

  @Override
  public ProductLevel3DTO update(ProductLevel3 product, Integer deltaStock, Integer minimumStock, boolean isOnlyExternal)
      throws Exception {
    String businessPartnerCode = product.getBusinessPartnerCode();
    ApiErrorCode apiErrorCode = null;
    ProductItemLevel3 productItem = product.getItems().get(0);
    String gdnSku = productItem.getItemSku();

    ProductLevel3 savedProductData = this.findDetailByGdnSkuForUpdateHistory(businessPartnerCode, gdnSku);
    if (Objects.nonNull(product.getVersion()) && Objects.nonNull(savedProductData.getVersion())
        && product.getVersion() <= savedProductData.getVersion()) {
      throw new ApplicationException(ErrorCategory.INVALID_STATE, ErrorMessages.INVALID_VERSION_ERROR);
    }
    if (savedProductData.isSuspended()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.ITEM_IS_SUSPENDED.getDesc());
    }
    if (savedProductData.isForceReview()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.FAULTY_IMAGE_FORCE_REVIEW.getDesc());
    }
    for (ProductItemLevel3 productItemLevel3 : savedProductData.getItems()) {
      if (Objects.nonNull(productItemLevel3) && productItemLevel3.getItemSku()
          .equals(product.getItems().get(0).getItemSku()) && Boolean.TRUE.equals(productItemLevel3.getArchived())) {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "Item Sku is archived");
      }
    }
    Boolean lateFulfillment = null  ;
    boolean productTypeAndLateFulfillmentChanged = false;
    if (savedProductData.getProductType() != ProductType.REGULAR.getCode()
        && product.getProductType() == ProductType.REGULAR.getCode()) {
      lateFulfillment = false;
      productTypeAndLateFulfillmentChanged = true;
    } else if (savedProductData.getProductType() == ProductType.REGULAR.getCode()
        && product.getProductType() != ProductType.REGULAR.getCode()) {
      lateFulfillment = true;
      productTypeAndLateFulfillmentChanged = true;
    }

    if (Objects.nonNull(lateFulfillment)) {
      for (ProductItemLevel3 item : product.getItems()) {
        item.setLateFulfillment(lateFulfillment);
      }
    }
    ItemRequest itemRequest = generateItemRequest(product.getItems().get(0),
        product.getProductSku(), product.getAttributes());
    boolean contentChanged = checkContentChange(product, savedProductData);
    LOGGER.info("Setting content change {} for item sku : {}", contentChanged, itemRequest.getItemSku());
    itemRequest.setContentChanged(contentChanged);
    if (checkPriceOrStockChange(product, savedProductData, deltaStock, minimumStock)) {
      apiErrorCode = validateAndUpdateWholesalePrice(itemRequest, product.getCategoryCode(),
          savedProductData.getItems().get(0).getWholesalePriceActivated(),
          savedProductData.getItems().get(0).getPickupPointCode());
    }
    if (CollectionUtils.isNotEmpty(product.getItems()) && Objects.isNull(itemRequest.getOff2OnChannelActive())) {
      itemRequest.setOff2OnChannelActive(savedProductData.getItems().get(0).getOff2OnActiveFlag());
    }
    ProductRequest productRequest = generateProductRequest(product);
    ProductResponse productResponse = this.xProductOutbound.updateProduct(isOnlyExternal, productRequest);
    ItemResponse itemResponse =
        this.xProductOutbound.updateItem(isOnlyExternal, productTypeAndLateFulfillmentChanged, false, itemRequest);
    if (Objects.nonNull(deltaStock)) {
      this.productLevel3InventoryService.updateStock(businessPartnerCode, gdnSku, deltaStock);
    }
    if (Objects.nonNull(minimumStock)) {
      this.productLevel3InventoryService.updateMinimumStockAlert(product.getBusinessPartnerCode(),
          productItem.getItemSku(), minimumStock);
      this.productBusinessPartnerService.updateMinimumStockByGdnProductItemSku(
          product.getItems().get(0).getItemSku(), minimumStock);
    }
    this.productLevel3InventoryService.updatePickupPoint(product.getBusinessPartnerCode(),
        product.getItems().get(0).getItemSku(), product.getItems().get(0).getPickupPointCode());
    boolean isSuccess = this.productLevel3LogisticsService.saveLogisticsByItemSku(
        Arrays.asList(product.getItems().get(0).getItemSku()), product.getBusinessPartnerCode(),
        product.getItems().get(0).getLogistics(), true);
    if (!isSuccess) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          ErrorMessages.LOGISTICS_UPDATE_FAILED);
    }
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    productAndItemsResponse.setProduct(productResponse);
    productAndItemsResponse.setItems(new ArrayList<>());
    productAndItemsResponse.getItems().add(itemResponse);
    ProductLevel3 updatedProductData =
        this.findDetailByGdnSkuToUpdateHistory(businessPartnerCode, gdnSku, productAndItemsResponse);
    if (Objects.isNull(productResponse.getMasterDataProduct())) {
      updatedProductData.setDescription(product.getDescription());
      updatedProductData.setUniqueSellingPoint(product.getUniqueSellingPoint());
    }
    UpdateProductItemLevel3Model savedData =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3(savedProductData, false);
    UpdateProductItemLevel3Model updatedData =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3(updatedProductData, false);
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(businessPartnerCode, gdnSku, savedData,
        updatedData, product.getAccessChannel(), product.getProductSku(), product.getProductName(), false,
        StringUtils.EMPTY);
    return ProductLevel3DTO.builder().apiErrorCode(apiErrorCode).productLevel3(updatedProductData).build();
  }

  @Override
  public EditProductResponse updateEditInfo(ProductLevel3  product, boolean isOnlyExternal,
   boolean toTakeDown,
      boolean combineContentAndLogisticsPcbUpdate, ProfileResponse profileResponse, boolean combinePreOrderUpdate,
      ProductL3Response savedProductData, boolean newImagesAdded,
      ProductCollection productCollection, ProductL3UpdateRequest productL3UpdateRequest) throws Exception {
    EditProductResponse editProductResponse = new EditProductResponse();
    boolean isTrustedSeller = profileResponse.isTrustedSeller();
    ProductDetailEditDTO productDetailEditDTO = null;
    productCollection = Optional.ofNullable(productCollection)
        .orElseGet(() -> productCollectionRepository.findByStoreIdAndProductCode(DEFAULT_STORE_ID, product.getProductCode()));
    ProductCollectionDTO productCollectionDTO =
      ConverterUtil.convertProductCollectionToDto(productCollection);
    String gdnSku = savedProductData.getDefaultItemSku();
    if(addDeleteVariantSwitch) {
      gdnSku = product.getItems().stream().findFirst().filter(productItemLevel3 -> !productItemLevel3.isMarkForDelete())
          .map(ProductItemLevel3::getItemSku).orElse(savedProductData.getDefaultItemSku());
    }
    ProductAndItemsResponse productData = this.productLevel3Repository.findDetailByGdnSku(gdnSku);
    PickupPointResponse pickupPointData = new PickupPointResponse();
    pickupPointData.setCode(productData.getItems().get(0).getPickupPointCode());
    ProductLevel3 productLevel3 =
        generateProductLevel3(productData, null, pickupPointData, null, null, null, new HashMap<>(), new ArrayList<>(), false);
    productLevel3.setVersion(productData.getItems().get(0).getVersion());
    ProductLevel3DetailResponse savedDataForHistory =
        generateProductLevel3Detail(savedProductData, new ArrayList<>(), new ArrayList<>(), null);
    if (Boolean.TRUE.equals(product.getProductEditable())) {
      productDetailEditDTO =
        generateProductDetailEditDTO(product, productLevel3, combineContentAndLogisticsPcbUpdate,
          combinePreOrderUpdate, savedProductData, savedDataForHistory, productCollection);
      validateFreeSampleWholesaleAndPromoBundling(editProductResponse, productLevel3, product,
        savedProductData);
      if(Objects.nonNull(editProductResponse.getApiErrorCode())){
        return editProductResponse;
      }
      if (CommonUtils.isDataUpdated(productDetailEditDTO, product))
      {
        LOGGER.info("Updating the content change for product sku : {}, contentChanged : {}", product.getProductSku(),
            productDetailEditDTO.isContentChanged());
        AutoApprovalType autoApprovalType = null;
        autoApprovalType =
          fetchAutoApprovalResponse(product, productCollection, productDetailEditDTO,
            autoApprovalType, editProductResponse);
        // By default set post-live as true and only change it when content has been changed
        ConfigurationStatusResponse configurationStatusResponse = new ConfigurationStatusResponse();
        configurationStatusResponse.setReviewConfig(POST_LIVE);
        List<ConfigurationStatusResponse> configurationStatusResponseList = Collections.singletonList(configurationStatusResponse);
        if (productDetailEditDTO.isContentChanged()) {
          Pair<List<ConfigurationStatusResponse>, Boolean> configurationStatusPair = getConfigurationStatus(product);
          configurationStatusResponseList = configurationStatusPair.getKey();
          boolean isPostLive = configurationStatusPair.getValue();
          productCollection.setPostLive(isPostLive);
        }
        editProductResponse.setPostLive(
            Optional.ofNullable(configurationStatusResponseList)
                .flatMap(list -> list.stream().filter(Objects::nonNull).findFirst())
                .map(ConfigurationStatusResponse::getReviewConfig)
                .map(POST_LIVE::equals)
                .orElse(false)
        );
        processRestrictedKeywords(productCollection, productDetailEditDTO, editProductResponse,
          profileResponse, configurationStatusResponseList);
        List<String> reviewTypeList = new ArrayList<>();
        if (Objects.nonNull(productCollection.getReviewType())) {
          reviewTypeList = Stream.of(productCollection.getReviewType().split(",")).collect(Collectors.toList());
        }
        if(Boolean.TRUE.equals(profileResponse.isTrustedSeller()) && productCollection.isPostLive()){
          toTakeDown = false;
        }
        String contentType = StringUtils.EMPTY;
        if (AutoApprovalType.CONTENT_AND_IMAGE.equals(autoApprovalType)) {
          handleContentAndImageAutoApproval(productDetailEditDTO, productCollection, product.getUpdatedBy());
        } else {
          handleNonContentAndImageAutoApproval(productDetailEditDTO, productCollection,
            editProductResponse, reviewTypeList, configurationStatusResponseList, newImagesAdded);
        }
        boolean takeDownProduct = productDetailEditDTO.isTakeDownProduct();
        contentType = productDetailEditDTO.getContentType();
        updateDimensionsAtL3(product, productDetailEditDTO.isDimensionUpdated());

        // pcb call for l1 update
        Pair<com.gdn.x.productcategorybase.dto.request.ProductRequest,
          List<NewlySavedItemResponse>> productRequestNewlySavedItemResponsePair =
            copyProductLevel3ToProductAndUpdateToPCB(productCollection, product,
                productDetailEditDTO.isContentChanged(),
                !CollectionUtils.isEmpty(productDetailEditDTO.getRestrictedKeywordsByFieldList()),
                productDetailEditDTO.isAutoApproved(), productDetailEditDTO.getCategoryResponse(), editProductResponse,
                productDetailEditDTO, savedProductData, productL3UpdateRequest);
        editProductResponse.setNewlySavedItemResponseList(productRequestNewlySavedItemResponsePair.getRight());
        productDetailEditDTO.setAccessChannel(productLevel3.getAccessChannel());
        productDetailEditDTO.setProductRequestForPCB(productRequestNewlySavedItemResponsePair.getLeft());
        if (POST_LIVE.equals(configurationStatusResponseList.get(0).getReviewConfig())) {
          productBusinessPartnerService.updateProductMasterData(product.getProductSku(), product.getProductName(),
            productCollection.getCategoryCode(), productCollection.getCategoryName(), product.getSizeChartCode(),
            product.getUpdatedBy(), product.isSizeChartChanged(), product.getBrand());
        } else if(product.isSizeChartChanged() || product.isBrandUpdated()){
          productBusinessPartnerService.updateSizeChartDetailsAndBrandDetails(product.getProductSku(),
            product.getSizeChartCode(), product.getUpdatedBy(), product.isSizeChartChanged(), product.isBrandUpdated(),
              product.getBrand());
        }
        editProductResponse.setToTakeDown(takeDownProduct);
        takeDownProduct = toTakeDown && takeDownProduct;
        productDetailEditDTO.setTakeDownProduct(takeDownProduct);
        if(takeDownProduct) {
          productDetailEditDTO.setPublishImageQcForContentChange(false);
        }

        updateProductCollectionAndSolrAndPublishHistoryEvent(product.getProductName(), productCollection,
          reviewTypeList, productDetailEditDTO, product.getOldBrandName());
        CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, productLevel3);
        // X-prod l3 update call
        updateEditedProduct(product, productLevel3, productDetailEditDTO, savedProductData,
          productDetailEditDTO.getPreOrderRequest());
        product.setItems(new ArrayList<>());
        if (productDetailEditDTO.isPreOrderChange() && shouldUpdatePreOrderDateOnInventory(product.isSellerOmg())) {
          updatePreOrderDateOnInventory(product.getProductSku(),
              com.gdn.partners.pbp.commons.util.CommonUtils.resolvePreOrderDate(product.getPreOrder().getIsPreOrder(), product.getPreOrder().getPreOrderDate()));
        }
        if (StringUtils.isNotEmpty(contentType)) {
          productDetailEditDTO.setContentType(contentType);
          if (ResponseHelper.isProductEligibleForVendorPublish(editProductResponse.getAction(),
            false, isTrustedSeller)) {
            List<String> allModifiedFields = generateModifiedFields(product, savedDataForHistory);
            productDetailEditDTO.setAllModifiedFields(allModifiedFields);
            publishEditedProductEventToPDT(DEFAULT_STORE_ID, contentType, productCollection, allModifiedFields);
          }
        }
      } else {
        editProductResponse.setProductReview(false);
      }
      editProductResponse.setReviewType(productCollection.isPostLive() ? POST_LIVE_REVIEW_TYPE : PRE_LIVE_REVIEW_TYPE);
      editProductResponse.setPublishImageQcForContentChange(productDetailEditDTO.isPublishImageQcForContentChange());
      editProductResponse.setRestrictedKeywordsByFieldAndActionType(productDetailEditDTO.getRestrictedKeywordsByFieldAndActionType());
    } else {
      updateProductAndItem(product, null, isOnlyExternal, productLevel3, false);
      editProductResponse.setProductReview(false);
      editProductResponse.setReviewType(StringUtils.EMPTY);
    }
    if (Objects.nonNull(productDetailEditDTO)) {
      productDetailEditDTO.setProductCollectionDTO(productCollectionDTO);
      productDetailEditDTO.setProductLevel3(productLevel3);
      productDetailEditDTO.setProductLevel3DetailResponse(savedDataForHistory);
      productDetailEditDTO.setProductSku(productLevel3.getProductSku());
    }
    editProductResponse.setProductDetailEditDTO(productDetailEditDTO);
    this.createAuditLogsForEdit(savedDataForHistory, product.getAccessChannel(), product.isSizeChartChanged());
    if (product.isCategoryUpdated()) {
      AuditTrailListRequest auditTrailListRequest =
          CommonUtils.getAuditTrailRequestForL3History(productCollection.getBusinessPartnerCode(),
              product.getProductSku(), product.getProductName(),
              UpdateProductActivity.EXTERNAL_CATEGORY_CHANGE.getDesc(), Constants.CATEGORY,
              product.getOldCategoryName(), product.getCategoryName(), GdnMandatoryRequestParameterUtil.getUsername(),
              GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getClientId(),
              Constants.DEFAULT, Constants.HYPHEN);
      auditTrailListRequest.setUpdateDirectlyToDB(true);
      kafkaProducer.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, product.getProductSku(),
          auditTrailListRequest);
    }
    return editProductResponse;
  }

  private AutoApprovalType fetchAutoApprovalResponse(ProductLevel3 product,
    ProductCollection productCollection, ProductDetailEditDTO productDetailEditDTO,
    AutoApprovalType autoApprovalType, EditProductResponse editProductResponse) throws Exception {
    if (CollectionUtils.isEmpty(productDetailEditDTO.getRestrictedKeywordsByFieldAndActionType()
      .getRestrictedKeywordsByFieldList()) && productDetailEditDTO.isContentChanged()) {
      Pair<AutoApprovalType, List<CategoryResponse>> autoApprovalTypeAndCategoryResponse =
        processAutoApproval(productCollection, product.getProductSku(), product.getUpdatedBy());
      autoApprovalType = autoApprovalTypeAndCategoryResponse.getKey();
      if(productCollection.isPostLive() && Objects.nonNull(autoApprovalType)){
        editProductResponse.setCategoryResponses(autoApprovalTypeAndCategoryResponse.getValue());
      }
      productDetailEditDTO.setAutoApprovalType(autoApprovalType);
      log.info("Auto Approval type for product : {} is  {} ", product.getProductSku(),
        autoApprovalType);
    }
    return autoApprovalType;
  }

  @Override
  public void updateDimensionsAtL3(DimensionHolder product, boolean dimensionsUpdated)
    throws Exception {
    if (dimensionsUpdated) {
      if (CommonUtils.isPureInstoreProduct(product.isInstore(),
        product.getB2cActivated(), instoreNewFlowEnabled) && CommonUtils.hasZeroDimension(product.getLength(),
        product.getWidth(),
        product.getHeight(),
        product.getWeight(), product.getShippingWeight())) {
          product.setLength(Constants.DOUBLE_ZERO);
          product.setWidth(Constants.DOUBLE_ZERO);
          product.setHeight(Constants.DOUBLE_ZERO);
          product.setWeight(Constants.DOUBLE_ZERO);
          product.setShippingWeight(Constants.DOUBLE_ZERO);
      } else {
        product.setShippingWeight(generatorService
          .generateShippingWeight(product.getLength(), product.getWidth(), product.getHeight(), product.getWeight(),
            product.getCategoryCode()));
      }
    }
  }

  @Override
  public void updateProductCollectionAndSolrAndPublishHistoryEvent(String productName,
    ProductCollection productCollection, List<String> reviewTypeList,
    ProductEditContext productEditContext, String oldBrandName) throws Exception {
    if(CollectionUtils.isNotEmpty(reviewTypeList)) {
      productCollection.setReviewType(String.join(",", reviewTypeList));
    }
    log.info(
      "Updating product collection and publishing History events and update Solr for product : {} , review type list : {} ",
      productCollection.getProductCode(), reviewTypeList);
    if(StringUtils.isNotEmpty(productName)) {
      productCollection.setProductName(productName);
    }
    productService.saveProductCollectionForUPC(productCollection);
    if (productEditContext.isAutoCategoryChange() && StringUtils.isNotEmpty(
      productEditContext.getOldCategoryName())) {
      String keyword = StringUtils.EMPTY;
      if (Objects.nonNull(productEditContext.getRestrictedKeywordsByFieldAndActionType())) {
        keyword = productEditContext.getRestrictedKeywordsByFieldAndActionType().getKeyword();
      }
      publishHistoryEventsForContentEdit(productCollection, productEditContext, keyword);
    }
    updateSolrProductCollectionDocument(productCollection);
  }

  private void publishHistoryEventsForContentEdit(ProductCollection productCollection,
    ProductEditContext productEditContext, String keyword) {
    InternalProductHistoryEventModel internalProductHistoryEventModel =
        ConverterUtil.createInternalProductHistoryEventModel(
          productCollection, productEditContext.getOldCategoryName(), keyword);
    kafkaProducer.send(DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE, productCollection.getProductCode(),
        internalProductHistoryEventModel);
    List<ProductBusinessPartner> productBusinessPartners = this.productBusinessPartnerService
        .findByStoreIdAndProductId(Constants.DEFAULT_STORE_ID, productCollection.getProductId());
    AuditTrailListRequest auditTrailListRequest = CommonUtils
        .getAuditTrailRequestForL3History(productCollection.getBusinessPartnerCode(),
            productBusinessPartners.get(0).getGdnProductSku(), productCollection.getProductName(),
            UpdateProductActivity.AUTO_CATEGORY_CHANGE.name(), StringUtils.EMPTY, productEditContext.getOldCategoryName(),
            productCollection.getCategoryName(), SaveHistoryConstants.AUTO_CATEGORY_CHANGE,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_CLIENT_ID, DEFAULT, HYPHEN);
    kafkaProducer.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY,
        productBusinessPartners.get(0).getGdnProductSku(), auditTrailListRequest);
  }

  private boolean isPreOrderChange(ProductLevel3 product, ProductL3Response savedProductData,
      ProductLevel3 productLevel3, PreOrderRequest preOrderRequest, boolean combinePreOrderUpdate) throws Exception {
    if(Objects.nonNull(product.getPreOrder())) {
      BeanUtils.copyProperties(product.getPreOrder(), preOrderRequest);
    }
    boolean isPreOrderChange = false;
    if (combinePreOrderUpdate) {
      if (isPreOrderChange(preOrderRequest, savedProductData.getPreOrderDTO())) {
        isPreOrderChange = true;
        String oldValue = null;
        oldValue = getOldValueForPreOrderHistory(savedProductData, oldValue);
        String newValue = String.valueOf(product.getPreOrder()).substring(15);
        PreOrderRequest preOrderRequestForHistory = new PreOrderRequest();
        BeanUtils.copyProperties(product.getPreOrder(), preOrderRequestForHistory);
        newValue = generateNewValueFieldForPreOrderHistory(preOrderRequestForHistory, newValue);
        updatedProductHistoryService.createProductL3AuditLog(savedProductData.getMerchantCode(), Constants.DEFAULT,
            savedProductData.getProductSku(), productLevel3.getProductName(), UpdateProductActivity.PRE_ORDER.getDesc(),
            oldValue, newValue, false, StringUtils.EMPTY);
      } else if (Objects.nonNull(savedProductData.getPreOrderDTO())) {
        BeanUtils.copyProperties(savedProductData.getPreOrderDTO(), preOrderRequest);
      }
    }
    return isPreOrderChange;
  }

  public static String getOldValueForPreOrderHistory(ProductL3Response savedProductData, String oldValue) {
    if (Objects.nonNull(savedProductData.getPreOrderDTO())) {
      String preOrderString = String.valueOf(savedProductData.getPreOrderDTO());
      preOrderString =
          preOrderString.substring(11, preOrderString.indexOf(Constants.CONVERT_TO_JKT_VARIABLE_FOR_HISTORY));
      oldValue = preOrderString + Constants.CLOSE_PARENTHESIS;
    }
    return oldValue;
  }

  public EditProductResponse saveNeedCorrectionChangesInPCB(ProductLevel3 product,
      List<PickupPointDeleteRequest> pickupPointDeleteRequests, boolean addingPickupPoints,
      ProductL3Response savedProductData, ProductL3UpdateRequest productL3UpdateRequest,
      ProductCollection updatedProductCollection) throws Exception {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    if (Objects.nonNull(productL3UpdateRequest) && (
        MapUtils.isNotEmpty(productL3UpdateRequest.getDistributionInfoRequest()) || CollectionUtils.isNotEmpty(
            productL3UpdateRequest.getDistributionAndUOMRequest()))) {
      DistributionInfoRequest distributionInfoRequest = new DistributionInfoRequest();
      distributionInfoRequest.setSellerCode(product.getBusinessPartnerCode());
      distributionInfoRequest.setProductItems(productL3UpdateRequest.getDistributionAndUOMRequest());
      distributionInfoRequest.setDistributionInfoRequest(productL3UpdateRequest.getDistributionInfoRequest());
      distributionInfoServiceBean.validateUomInfo(product.getProductCode(), distributionInfoRequest, auditTrailDtoList);
    }
    ProductLevel3DetailResponse savedProductLevel3DetailResponse =
        getL3DetailByProductSku(product.getProductSku(), true, true, pickupPointDeleteRequests, addingPickupPoints);
    ProductDetailResponse productDetailResponse = ConverterUtil.toProductDetailResponse(product);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList =
        productLevel3Helper.getRestrictedKeywordsInProductDetails(productDetailResponse, product.getCategoryCode());
    LOGGER.info("save need revision changes for product code : {}, restrictedKeywordList :{}", product.getProductCode(),
        restrictedKeywordsByFieldList);
    updatedProductCollection.setRestrictedKeywordsPresent(!CollectionUtils.isEmpty(restrictedKeywordsByFieldList));
    updatedProductCollection.setRestrictedKeywordsDetected(objectMapper.writeValueAsString(restrictedKeywordsByFieldList));
    copyProductLevel3ToProductAndUpdateToPCB(updatedProductCollection, product, true,
        !CollectionUtils.isEmpty(restrictedKeywordsByFieldList), false, null, new EditProductResponse(), null,
      savedProductData, productL3UpdateRequest);
    LOGGER.info("saved the product info changes of product code : {}", product.getProductCode());
    updatedProductCollection.setProductName(product.getProductName());
    List<String> allModifiedFields = new ArrayList();
    allModifiedFields.addAll(generateModifiedFields(product, savedProductLevel3DetailResponse));
    if (CollectionUtils.isNotEmpty(allModifiedFields)) {
      allModifiedFields.add(Constants.CONTENT);
    }
    updateNeedRevisionNotes(updatedProductCollection, allModifiedFields);
    this.productCollectionRepository.save(updatedProductCollection);
    updateSolrProductCollectionDocument(updatedProductCollection);
    productBusinessPartnerService.updateNeedRevisionL3Details(product.getProductSku(), product);
    this.createAuditLogsForNeedRevision(savedProductLevel3DetailResponse, product, product.isNeedCorrection());
    if (product.isCategoryUpdated()) {
      AuditTrailDto auditTrailDto = new AuditTrailDto(product.getBusinessPartnerCode(), DEFAULT,
          UpdateProductActivity.EXTERNAL_CATEGORY_CHANGE.getDesc(), product.getOldCategoryName(),
          product.getCategoryName(), Constants.CATEGORY, product.getProductSku(), product.getProductName(), DEFAULT,
          false);
      auditTrailDtoList.add(auditTrailDto);
    }
    publishUomHistoryIfNotEmpty(product, auditTrailDtoList);
    return EditProductResponse.builder().productReview(false).reviewType(StringUtils.EMPTY).build();
  }

  public void publishUomHistoryIfNotEmpty(ProductLevel3 product, List<AuditTrailDto> auditTrailDtoList) {
    if (CollectionUtils.isNotEmpty(auditTrailDtoList)) {
      auditTrailDtoList.forEach(auditLog -> {
        auditLog.setActionKey(auditLog.getActionKey() + Constants.NEED_REVISION);
      });
      productLevel3V2Service.publishProductLevelHistoryToPcbForDistributionUpdate(product.getProductCode(),
          product.getBusinessPartnerCode(), auditTrailDtoList);
    }
  }

  private void updateNeedRevisionNotes(ProductCollection productCollection, List<String> allModifiedFields)
      throws IOException {
    VendorNotesRequest vendorNotesRequest = new VendorNotesRequest();
    if (Objects.nonNull(productCollection.getNeedCorrectionNotes())) {
      vendorNotesRequest = objectMapper.readValue(productCollection.getNeedCorrectionNotes(), VendorNotesRequest.class);
    }
    List<String> existingModifiedFields =
        ofNullable(vendorNotesRequest.getAllModifiedFields()).orElse(new ArrayList<>());
    existingModifiedFields.addAll(allModifiedFields.stream()
        .filter(modifiedFields -> !existingModifiedFields.contains(modifiedFields)).collect(Collectors.toList()));
    vendorNotesRequest.setAllModifiedFields(existingModifiedFields);

    List<String> reviewTypeList = new ArrayList();
    if (Objects.nonNull(productCollection.getReviewType())) {
      reviewTypeList = Stream.of(productCollection.getReviewType().split(",")).collect(Collectors.toList());
    }

    if (existingModifiedFields.contains(Constants.CONTENT) && !reviewTypeList.contains(CONTENT)) {
      reviewTypeList.add(CONTENT);
    }
    if (existingModifiedFields.contains(Constants.IMAGE) && !reviewTypeList.contains(IMAGE)) {
      reviewTypeList.add(IMAGE);
    }
    productCollection.setReviewType(String.join(",", reviewTypeList));
    productCollection.setNeedCorrectionNotes(objectMapper.writeValueAsString(vendorNotesRequest));
  }

  private List<String> generateModifiedFields(ProductLevel3 modifiedProduct,
      ProductLevel3DetailResponse savedProductLevel3DetailResponse) throws Exception {
    UpdateProductItemLevel3Model savedModel =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Detail(savedProductLevel3DetailResponse, true);
    UpdateProductItemLevel3Model updatedModel =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3(modifiedProduct, true);
    return updatedProductHistoryService.getMerchantModifiedFields(savedModel, updatedModel);
  }


 private void createAuditLogsForNeedRevision(ProductLevel3DetailResponse savedProductLevel3DetailResponse,
      ProductLevel3 request, boolean needCorrection) throws Exception {
    ProductLevel3DetailResponse updatedProductLevel3DetailResponse =
        getL3DetailByProductSku(request.getProductSku(), true, true, new ArrayList<>(), false);
    UpdateProductItemLevel3Model savedModel =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Detail(savedProductLevel3DetailResponse, false);
    UpdateProductItemLevel3Model updatedModel =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Detail(updatedProductLevel3DetailResponse, false);
    updatedProductHistoryService.saveUpdateProductLevel3Audit(request.getBusinessPartnerCode(), Constants.DEFAULT, savedModel,
        updatedModel, request.getAccessChannel(), request.getProductSku(), request.getProductName(), needCorrection,
        StringUtils.EMPTY);
  }

  private void updateEditedProduct(ProductLevel3 product, ProductLevel3 productLevel3,
    ProductDetailEditDTO productDetailEditDTO, ProductL3Response savedProductData,
    PreOrderRequest preOrderRequest) throws Exception {
    boolean contentUpdateForSpecialAttributes = productDetailEditDTO.isContentUpdateForSpecialAttributes();
      boolean takeDownProduct = productDetailEditDTO.isTakeDownProduct();
      boolean contentChanged = productDetailEditDTO.isContentChanged();
      boolean freeSampleFlagChanged = productDetailEditDTO.isFreeSampleFlagChanged();
      boolean off2OnChannelFlagChanged = productDetailEditDTO.isOff2OnChannelFlagChanged();
      boolean isDimensionUpdated = productDetailEditDTO.isDimensionUpdated();
      boolean isProductTypeChanged = productDetailEditDTO.isProductTypeChanged();
      Boolean lateFulfillment = productDetailEditDTO.getLateFulfillment();
      boolean isPreOrderChange = productDetailEditDTO.isPreOrderChange();
    boolean isCategoryUpdated =
        Objects.nonNull(productDetailEditDTO.getCategoryResponse()) || product.isCategoryUpdated();
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();

    ProductEditRequest productEditRequest = new ProductEditRequest();
    productEditRequest.setContentChanged(contentChanged);
    productEditRequest.setForceReview(takeDownProduct);
    productEditRequest.setProductSku(product.getProductSku());
    productEditRequest.setLatefulfillment(lateFulfillment);
    boolean videoUrlUpdated =
        Optional.ofNullable(productLevel3.getVideoUpdated()).orElse(Boolean.FALSE);
    if (CommonUtils.checkForChangedRequest(contentUpdateForSpecialAttributes, freeSampleFlagChanged,
      off2OnChannelFlagChanged, isDimensionUpdated, isProductTypeChanged, isPreOrderChange,
      product.isSizeChartChanged(), videoUrlUpdated) || contentChanged || productDetailEditDTO.isBrandChanged()) {
      BeanUtils.copyProperties(product, productLevel3, "items", "productType", "installationRequired");
      if(Objects.nonNull(product.getProductType())) {
        productLevel3.setProductType(product.getProductType());
      }
      product.setItems(productLevel3.getItems());
      ProductRequest productRequest = generateProductRequest(productLevel3);
      if (CommonUtils.isPreOrderChanged(isPreOrderChange, preOrderRequest)) {
        PreOrderDTO preOrderDTO = new PreOrderDTO();
        BeanUtils.copyProperties(preOrderRequest, preOrderDTO);
        productRequest.setPreOrder(preOrderDTO);
      }
      if (product.isSizeChartChanged()) {
        productRequest.setSizeChartCode(product.getSizeChartCode());
        productRequest.setSizeChartChanged(true);
      }
      productEditRequest.setProductRequest(productRequest);
      productDetailPageEditRequest.setEditChangeType(EditChangeType.CONTENT);
    }
    if (takeDownProduct)
      updateXProductAndPBPL3OnTakeDown(product, productLevel3, productDetailEditDTO, savedProductData, productEditRequest,
        isCategoryUpdated);
    else {
        xProductOutbound.updateEditedProduct(productEditRequest, isCategoryUpdated);
    }

    if (isDimensionUpdated || isProductTypeChanged) {
      productService.publishDimensionRefreshEventForReviewPendingProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
          product.getProductCode(), ConverterUtil
              .toDimensionRefreshRequest(productLevel3.getLength(), productLevel3.getWidth(), productLevel3.getHeight(),
                  productLevel3.getWeight(), productLevel3.getShippingWeight(), productLevel3.getDangerousGoodsLevel(),
                  productLevel3.getProductType()));
    }
  }

  @Override
  public void updateXProductAndPBPL3OnTakeDown(ProductLevel3 product, ProductLevel3 productLevel3,
    ProductDetailEditDTO productDetailEditDTO, ProductL3Response savedProductData,
    ProductEditRequest productEditRequest, boolean isCategoryUpdated) throws Exception {
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    int page = 0;
    int size = Integer.parseInt(productSystemParameterService
        .findByStoreIdAndVariable(product.getStoreId(), SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE).getValue());
    List<ItemSummaryDetailResponse> itemSummaryDetailResponses = getItemSummaryResponses(product.getProductSku(), page, size);
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerRepository.findFirstByGdnProductSku(product.getProductSku());
    productBusinessPartner.setProductName(product.getProductName());
    String categoryCode = productLevel3.getCategoryCode();
    productBusinessPartner = productBusinessPartnerService.updateProductBusinessPartnerState(
        productBusinessPartner, true, categoryCode, savedProductData);
    productLevel3Helper.setProductLevel3DetailsFromSummaryResponse(productBusinessPartner, product);
    ProductBusinessPartnerAndItemViewConfigDto productBusinessPartnerAndItemViewConfigDto = productBusinessPartnerService.
        updateProductItemBusinessPartnerStateTakeDownTrue(itemSummaryDetailResponses, itemViewConfigAndItemSkuRequests, productBusinessPartner);
    productBusinessPartner = productBusinessPartnerAndItemViewConfigDto.getProductBusinessPartner();
    itemViewConfigAndItemSkuRequests = productBusinessPartnerAndItemViewConfigDto.getItemViewConfigAndItemSkuRequests();
    productEditRequest.setForceReview(true);
    productEditRequest.setItemViewConfigAndItemSkuListRequest(itemViewConfigAndItemSkuRequests);
    productDetailEditDTO.setProductBusinessPartner(productBusinessPartner);
    productDetailEditDTO.setDimensionRefreshRequest(setDimensionRefreshRequest(productLevel3));
    productDetailEditDTO.setProductDetailEditRequestForXProduct(
      ProductDetailPageEditRequest.builder().productEditRequest(productEditRequest)
        .editChangeType(EditChangeType.CONTENT).build());
    xProductOutbound.updateEditedProduct(productEditRequest, isCategoryUpdated);
    productBusinessPartnerRepository.save(productBusinessPartner);
  }

  private DimensionRefreshRequest setDimensionRefreshRequest(ProductLevel3 productLevel3) {
    return DimensionRefreshRequest.builder().width(productLevel3.getWidth())
      .shippingWeight(productLevel3.getShippingWeight()).length(productLevel3.getLength())
      .height(productLevel3.getHeight()).weight(productLevel3.getWeight())
      .productType(productLevel3.getProductType())
      .dangerousGoodsLevel(productLevel3.getDangerousGoodsLevel()).build();
  }

  @Override
  public Pair<com.gdn.x.productcategorybase.dto.request.ProductRequest, List<NewlySavedItemResponse>> copyProductLevel3ToProductAndUpdateToPCB(
    ProductCollection productCollection, ProductLevel3 productLevel3, boolean contentChanged,
    boolean isKeywordPresent, boolean autoApproved, CategoryResponse categoryResponse,
    EditProductResponse editProductResponse, ProductDetailEditDTO productDetailEditDTO,
    ProductL3Response savedDataResponse, ProductL3UpdateRequest productL3UpdateRequest) throws Exception {
    LOGGER.info("Processing to update edit product info in PCB for productCode : {}",
        productCollection.getProductCode());
    Product product = editProductResponse.getProduct();
    if (Objects.isNull(product)) {
      product = productRepository.findOne(productCollection.getProductId());
    }
    Map<String, String> itemCodeToItemNameMapping = product.getProductItems().stream()
        .filter(item -> Objects.nonNull(item.getSkuCode()) && Objects.nonNull(item.getGeneratedItemName()))
        .collect(Collectors.toMap(ProductItem::getSkuCode, ProductItem::getGeneratedItemName, (a, b) -> b));
    editProductResponse.setItemCodeToItemNameMapping(itemCodeToItemNameMapping);
    editProductResponse.setVideoAddEditRequest(productLevel3.getVideoAddEditRequest());
    editProductResponse.setVideoUpdated(productLevel3.getVideoUpdated());
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueMap = new HashMap<>();
    List<String> deletedAttributeCodes = new ArrayList<>();
    if(eligibleForPCBContentUpdate(productLevel3)) {
        convertProductLevel3ToProduct(productLevel3, product, productItemAttributeValueMap,
          deletedAttributeCodes);
      if (Objects.nonNull(categoryResponse)) {
        // Replace existing product category mapping to MFD true
        product.getProductCategories().forEach(productCategory -> productCategory.setMarkForDelete(true));
        ProductCategory productCategory = new ProductCategory();
        productCategory.setProduct(product);
        Category category = new Category();
        category.setId(categoryResponse.getId());
        category.setCategoryCode(categoryResponse.getCategoryCode());
        productCategory.setCategory(category);
        product.getProductCategories().add(productCategory);
      }
      setContentChangeAndDangerousGoodsLevel(contentChanged, product);
      product.setUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
      product.setUpdatedDate(new Date());
      setReviewPending(contentChanged, isKeywordPresent, autoApproved, product);
    }
    if (Objects.nonNull(productL3UpdateRequest)) {
      editProductResponse.setDistributionInfoRequest(productL3UpdateRequest.getDistributionInfoRequest());
      editProductResponse.setDistributionAndUOMRequest(productL3UpdateRequest.getDistributionAndUOMRequest());
    }
    if(productLevel3.isCategoryUpdated()) {
      product.getProductCategories().forEach(productCategory -> productCategory.setMarkForDelete(true));
      ProductCategory productCategory = new ProductCategory();
      productCategory.setProduct(product);
      Category category = new Category();
      category.setId(productLevel3.getCategoryId());
      category.setCategoryCode(productCollection.getCategoryCode());
      productCategory.setCategory(category);
      product.getProductCategories().add(productCategory);
    }
    List<NewlySavedItemResponse> newlyAddedItemResponse =
        this.productRepository.update(product, null, false, false, true,
            productLevel3.getNewlyAddedItems(), productItemAttributeValueMap, productLevel3.getDeletedItems(),
            addDeleteVariantSwitch, productLevel3.isCategoryUpdated(), deletedAttributeCodes, savedDataResponse,
            editProductResponse);
    log.info("Response from PCB after update : {} for productCode : {}", newlyAddedItemResponse,
        productCollection.getProductCode());
    LOGGER.info("Product info update in PCB successful product-code :{}", product.getProductCode());
    return Pair.of(null, newlyAddedItemResponse);
  }

  private static void setReviewPending(boolean contentChanged, boolean isKeywordPresent, boolean autoApproved,
      Product product) {
    if (contentChanged || isKeywordPresent) {
      product.setReviewPending(!autoApproved);
    }
  }

  private static void setContentChangeAndDangerousGoodsLevel(boolean contentChanged, Product product) {
    for (ProductItem productItem : product.getProductItems()) {
      productItem.setContentChanged(contentChanged);
      if (Objects.isNull(productItem.getDangerousGoodsLevel())) {
        productItem.setDangerousGoodsLevel(0);
      }
    }
  }

  private boolean eligibleForPCBContentUpdate(ProductLevel3 productLevel3) {
    return Objects.nonNull(productLevel3.getProductName()) || CollectionUtils.isNotEmpty(
      productLevel3.getItems()) || CollectionUtils.isNotEmpty(productLevel3.getAttributes());
  }

  public void setMainImageFlagForProductImages(ProductLevel3 productLevel3,
    Product product) {
    Map<String, ProductLevel3Image> productLevel3ImageMap = productLevel3.getImages().stream()
      .collect(Collectors.toMap(ProductLevel3Image::getLocationPath, Function.identity(), (v1,v2) -> v1));
    product.getProductImages().forEach(productImage -> {
      ProductLevel3Image productLevel3Image =
        productLevel3ImageMap.get(productImage.getLocationPath());
      if(Objects.nonNull(productLevel3Image)) {
        productImage.setMainImages(productLevel3Image.getMainImage());
        productImage.setCommonImage(true);
      }
    });

    Map<String, ProductImage> productImageMap =
      product.getProductImages().stream().filter(ProductImage::isCommonImage)
        .collect(Collectors.toMap(ProductImage::getLocationPath, Function.identity()));
    //Copy main Image and common image flag for Item Images for Common MFD false Images
    for (ProductItem productItem : product.getProductItems()) {
      List<ProductItemImage> productItemImages = productItem.getProductItemImages();
      for (ProductItemImage productItemImage : productItemImages) {
        if (productImageMap.containsKey(productItemImage.getLocationPath())) {
          ProductImage productImage = productImageMap.get(productItemImage.getLocationPath());
          productItemImage.setMainImages(productImage.isMainImages());
          productItemImage.setCommonImage(productImage.isCommonImage());
        }
      }
    }
  }

  private ProductDetailEditDTO generateProductDetailEditDTO(ProductLevel3 product,
    ProductLevel3 savedProductLevel3, boolean combineContentAndLogisticsPcbUpdate,
    boolean combinePreOrderUpdate, ProductL3Response productL3Response,
    ProductLevel3DetailResponse productLevel3DetailResponse, ProductCollection productCollection)
    throws Exception {
    boolean contentChanged = checkContentChange(product, savedProductLevel3);
    boolean contentUpdateForUrl = checkContentUpdateForUrl(product, savedProductLevel3);
    boolean contentUpdateForSpecialAttributes =
      checkContentUpdateForSpecialAttributes(product, savedProductLevel3);
    boolean freeSampleFlagChanged = checkIfFreeSampleChanged(product, savedProductLevel3);
    boolean off2OnChannelFlagChanged = checkIfOff2OnActiveChanged(product, savedProductLevel3);
    boolean publishImageQcForContentChange =
      isProductNameOrDescriptionChanged(product, savedProductLevel3) || product.isBrandUpdated();
    boolean isDimensionUpdated = false;
    boolean isProductTypeChanged = false;
    boolean isPreOrderChange = false;
    Boolean lateFulfillment = null;
    CategoryResponse categoryResponse = null;
    boolean isProductNameChanged = isProductNameChanged(product, savedProductLevel3);
    boolean brandChanged = product.isBrandUpdated();
    if (combineContentAndLogisticsPcbUpdate) {
      ProductLevel3UpdateRequest productLevel3UpdateRequest = new ProductLevel3UpdateRequest();
      BeanUtils.copyProperties(product, productLevel3UpdateRequest, "productEditable");
      isDimensionUpdated = isDimensionChanged(productLevel3DetailResponse.getProductL3Response(),
        productLevel3UpdateRequest);
      isProductTypeChanged =
        !Objects.equals(savedProductLevel3.getProductType(), product.getProductType());
      lateFulfillment =
        CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(product.getProductType(),
        isProductTypeChanged, lateFulfillment);
    }
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    isPreOrderChange =
      isPreOrderChange(product, productL3Response, savedProductLevel3, preOrderRequest,
        combinePreOrderUpdate);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList = new ArrayList<>();
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
      new RestrictedKeywordsByFieldAndActionType();
    ProductDetailResponse productDetailResponse = null;
    if (contentChanged || contentUpdateForSpecialAttributes) {
      productDetailResponse = ConverterUtil.toProductDetailResponse(product);
      restrictedKeywordsByFieldAndActionType =
        productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(
          productDetailResponse, product.getCategoryCode());
      if (skipDefinitiveAction && CommonUtils.isDefinitiveActionToBeSkipped(
        restrictedKeywordsByFieldAndActionType.getAction())) {
        productService.validateSkipDefinitiveAction(productL3Response.getStoreId(),
          productCollection, restrictedKeywordsByFieldAndActionType);
      }
      if (SKIP_ALL_ACTIONS != restrictedKeywordsByFieldAndActionType.getAction()) {
        // If skip all actions true then don't add restricted keyword list
        restrictedKeywordsByFieldList =
          restrictedKeywordsByFieldAndActionType.getRestrictedKeywordsByFieldList();
      }
    }

    boolean isAutoCategoryChange =
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType()
        == restrictedKeywordsByFieldAndActionType.getAction();

    String oldCategoryName = StringUtils.EMPTY;

    if (isAutoCategoryChange) {
      CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponse = null;
      categoryRestrictedKeywordResponse = productRepository.getCategoryRestrictedKeywordDetail(
        restrictedKeywordsByFieldAndActionType.getCategoryRestrictedKeywordId());
      log.info(
        "Product eligible for auto category change, productCode : {} destination category : {} ",
        productCollection.getProductCode(),
        categoryRestrictedKeywordResponse.getDestinationCategory());

      if (!product.getCategoryCode()
        .equals(categoryRestrictedKeywordResponse.getDestinationCategory())) {
        categoryResponse = productOutbound.getCategoryBasicDetailByCategoryCode(
          categoryRestrictedKeywordResponse.getDestinationCategory());
        oldCategoryName = productCollection.getCategoryName();
        product.setCategoryCode(categoryResponse.getCategoryCode());
        product.setCategoryName(categoryResponse.getName());
        productCollection.setCategoryName(categoryResponse.getName());
        productCollection.setCategoryCode(categoryResponse.getCategoryCode());
      }
      restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(new ArrayList<>());
      restrictedKeywordsByFieldList = new ArrayList<>();
    }

    LOGGER.info("Editing product sku : {}, contentChanged : {}, contentUpdateForUrl : {}, "
        + "contentUpdateForSpecialAttributes : {}, restrictedKeywordList :{} ,"
        + "freeSampleFlagChanged : {}, off2OnChannelFlagChanged : {} , dimensionUpdated : {}, productTypeChanged : {} , add variant list : {}, delete variant list {}",
      product.getProductSku(), contentChanged, contentUpdateForUrl,
      contentUpdateForSpecialAttributes, restrictedKeywordsByFieldList, freeSampleFlagChanged,
      off2OnChannelFlagChanged, isDimensionUpdated, isProductTypeChanged,
      product.getNewlyAddedItems(), product.getDeletedItems());

    if (CollectionUtils.isEmpty(
      restrictedKeywordsByFieldAndActionType.getRestrictedKeywordsByFieldList())) {
        productCollection.setRestrictedKeywordsPresent(false);
    }
    else {
      productCollection.setRestrictedKeywordsPresent(true);
    }

    return ProductDetailEditDTO.builder().contentChanged(contentChanged).contentUpdateForUrl(contentUpdateForUrl)
        .productEditable(product.getProductEditable())
        .contentUpdateForSpecialAttributes(contentUpdateForSpecialAttributes)
        .freeSampleFlagChanged(freeSampleFlagChanged).preOrderRequest(preOrderRequest)
        .off2OnChannelFlagChanged(off2OnChannelFlagChanged).dimensionUpdated(isDimensionUpdated)
        .productTypeChanged(isProductTypeChanged).preOrderChange(isPreOrderChange).categoryResponse(categoryResponse)
        .productDetailResponse(productDetailResponse).lateFulfillment(lateFulfillment).oldCategoryName(oldCategoryName)
        .autoCategoryChange(isAutoCategoryChange).publishImageQcForContentChange(publishImageQcForContentChange)
        .restrictedKeywordsByFieldList(restrictedKeywordsByFieldList).
        bopisEligible(Optional.ofNullable(categoryResponse).map(CategoryResponse::isBopisEligible).orElse(true))
        .restrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType)
        .productNameChanged(isProductNameChanged).videoUpdated(product.getVideoUpdated())
        .brandChanged(brandChanged).build();
  }

  @Override
  public Pair<AutoApprovalType, List<CategoryResponse>> processAutoApproval(
    ProductCollection productCollection, String categoryCode, String updatedBy) throws Exception {
    AutoApprovalType autoApprovalType = null;
    List<CategoryResponse> categoryResponses = null;
    if (productCollection.isPostLive()) {
     categoryResponses =
        productOutbound.filterCategoryHierarchyByCategoryCode(categoryCode);

      if (CollectionUtils.isNotEmpty(categoryResponses)) {
        AutoApprovalsDetailDto autoApprovalsDetail =
            AutoApprovalsDetailDto.builder().storeId(productCollection.getStoreId()).c1CategoryCode(
                    categoryResponses.get(categoryResponses.size() - 1).getCategoryCode())
                .merchantCode(productCollection.getBusinessPartnerCode())
                .productCode(productCollection.getProductCode()).updatedBy(updatedBy)
                .postLive(productCollection.isPostLive())
                .autoApprovalType(productCollection.getAutoApprovalType()).contentEditOnly(true)
                .revised(productCollection.isNeedRevision()).restrictedKeywordPresent(false)
                .productCreationType(productCollection.getProductCreationType()).build();

        autoApprovalType = productService.verifyAutoApprovalRules(autoApprovalsDetail);
      }
    }
    return Pair.of(autoApprovalType, categoryResponses);
  }

  private void processRestrictedKeywords(ProductCollection productCollection,
    ProductDetailEditDTO productDetailEditDTO, EditProductResponse editProductResponse,
    ProfileResponse profileResponse, List<ConfigurationStatusResponse> configurationStatusResponseList) throws JsonProcessingException {
    boolean hasRestrictedKeywords =
      !CollectionUtils.isEmpty(productDetailEditDTO.getRestrictedKeywordsByFieldList());
    boolean hasRestrictedKeywordsByFieldAndActionType = CollectionUtils.isNotEmpty(
      productDetailEditDTO.getRestrictedKeywordsByFieldAndActionType().getRestrictedKeywordsByFieldList());
    if (hasRestrictedKeywords) {
      boolean reviewConfiguration = profileResponse.isTrustedSeller() && configurationStatusResponseList.stream()
        .map(ConfigurationStatusResponse::getReviewConfig).findFirst().map(POST_LIVE::equals)
        .orElse(false);
      String setRestrictedKeywordsDetected =
        objectMapper.writeValueAsString(productDetailEditDTO.getRestrictedKeywordsByFieldList());
        productCollection.setPostLive(reviewConfiguration);
        productCollection.setRestrictedKeywordsPresent(true);
        productCollection.setRestrictedKeywordsDetected(setRestrictedKeywordsDetected);
      editProductResponse.setAction(productDetailEditDTO.getRestrictedKeywordsByFieldAndActionType().getAction());
      editProductResponse.setCategoryRestrictedKeywordId(productDetailEditDTO.getRestrictedKeywordsByFieldAndActionType().getCategoryRestrictedKeywordId());
      if (hasRestrictedKeywordsByFieldAndActionType) {
        for (RestrictedKeywordsByField restrictedKeywordsByField : productDetailEditDTO.getRestrictedKeywordsByFieldAndActionType()
          .getRestrictedKeywordsByFieldList()) {
          editProductResponse.getVendorErrorFields()
            .add(restrictedKeywordsByField.getFieldIdentifier());
        }
      }
    } else if (hasRestrictedKeywordsByFieldAndActionType) {
      String restrictedKeywordsDetected = objectMapper.writeValueAsString(
        productDetailEditDTO.getRestrictedKeywordsByFieldAndActionType()
          .getRestrictedKeywordsByFieldList());
        productCollection.setRestrictedKeywordsDetected(restrictedKeywordsDetected);
        productCollection.setRestrictedKeywordsPresent(true);
      }
    }


  private EditProductResponse validateFreeSampleWholesaleAndPromoBundling(EditProductResponse editProductResponse,
    ProductLevel3 productLevel3, ProductLevel3 product, ProductL3Response savedProductData)
    throws Exception {
    // Validate free sample and pre-order
    if (product.isFreeSample() && (product.isOff2OnChannelActive() || (
      Objects.nonNull(productLevel3.getPreOrder()) && Boolean.TRUE.equals(
        productLevel3.getPreOrder().getIsPreOrder())))) {
      LOGGER.error("Error updating free sample product to pre order : {}", product.getProductSku());
      editProductResponse.setApiErrorCode(ApiErrorCode.FREE_SAMPLE_CANNOT_BE_SET);
      return editProductResponse;
    }

    // Validate free sample and wholesale
    if (product.isFreeSample() && (Objects.nonNull(savedProductData.getPromoLabels()) && (
      Boolean.TRUE.equals(savedProductData.getPromoLabels().contains(WHOLESALE_PRICE))
        || Boolean.TRUE.equals(savedProductData.getPromoLabels().contains(WHOLESALE))))) {
      LOGGER.error("Error updating wholesale product to free sample : {}", product.getProductSku());
      editProductResponse.setApiErrorCode(ApiErrorCode.WHOLESALE_CANNOT_BE_SET_TO_FREE_SAMPLE);
      return editProductResponse;
    }

    // Validate free sample toggle change
    if (!product.isFreeSample() && savedProductData.isFreeSample()) {
      // Get all promo bundling summaries for the product
      PromoBundlingPricingSummaryRequest promoBundlingPricingSummaryRequest =
        PromoBundlingPricingSummaryRequest.builder()
          .merchantCode(savedProductData.getMerchantCode())
          .itemSkus(new HashSet<>(savedProductData.getItemSkus())).statusSet(
            ImmutableSet.of(PromoBundlingStatus.PENDING.name(), PromoBundlingStatus.ACTIVE.name()))
            .productSku(savedProductData.getProductSku()).build();

      List<PromoBundlingSummaryResponse> promoBundlingSummaryResponses =
        productPricingOutbound.getPromoBundlingSummaryResponse(promoBundlingPricingSummaryRequest);

      // Filter the free sample promo bundling summaries
      List<String> freeSampleProductList = promoBundlingSummaryResponses.stream()
        .map(PromoBundlingSummaryResponse::getPromoBundlingType).filter(
          (promoBundlingType) -> PromoBundlingType.FREE_SAMPLE.name().equals(promoBundlingType))
        .collect(Collectors.toList());

      // If there are any free sample promo bundling summaries, then the free sample toggle cannot be changed
      if (CollectionUtils.isNotEmpty(freeSampleProductList)) {
        ApiErrorCode freeSampleChangeError = ApiErrorCode.FREE_SAMPLE_STATUS_CHANGE_ERROR;
        editProductResponse.setApiErrorCode(freeSampleChangeError);
        LOGGER.error("Error updating free sample toggle : {}", product.getProductSku());
        return editProductResponse;
      }
    }

    return editProductResponse;
  }

  private Pair<List<ConfigurationStatusResponse>, Boolean> getConfigurationStatus(
    ProductLevel3 product) {
    ConfigurationStatusRequest configurationStatusRequest =
      ConfigurationStatusRequest.builder().categoryCode(product.getCategoryCode())
        .businessPartnerCode(product.getBusinessPartnerCode()).build();
    List<ConfigurationStatusResponse> configurationStatusResponseList;
    try {
      configurationStatusResponseList = productOutbound.getReviewConfiguration(
        Collections.singletonList(configurationStatusRequest));
    } catch (Exception e) {
      LOGGER.error(
        "Error while getting configuration for categoryCode : {} and businessPartnerCode : {}",
        configurationStatusRequest.getCategoryCode(),
        configurationStatusRequest.getBusinessPartnerCode(), e);
      configurationStatusResponseList = Collections.singletonList(
        new ConfigurationStatusResponse(product.getBusinessPartnerCode(), product.getCategoryCode(),
          PRE_LIVE));
    }
    return Pair.of(configurationStatusResponseList,
      POST_LIVE.equals(configurationStatusResponseList.get(0).getReviewConfig()));
  }

  private void handleContentAndImageAutoApproval(ProductDetailEditDTO productDetailEditDTO,
    ProductCollection productCollection, String userName) throws Exception {
    productDetailEditDTO.setPublishImageQcForContentChange(false);
    if (productHistoryUpdateThroughEvent) {
      publishInternalProductHistoryEvent(productCollection, userName);
    } else {
      saveProductHistory(productCollection, userName);
    }
    productDetailEditDTO.setAutoApproved(true);
    if (productCollection.isReviewPending()) {
      productDetailEditDTO.setContentType(CONTENT_REFRESH);
      productCollection.setEdited(true);
    }
  }

  @Override
  public void publishInternalProductHistoryEvent(ProductCollection productCollection,
    String userName) {
    InternalProductHistoryEventModel internalProductHistoryEventModel = ConverterUtil.generateInternalProductHistoryEventModel(productCollection.getProductCode(),
      ProductWorkflowLookup.STATE_AUTO_APPROVE_DESCRIPTION, userName,
      ProductWorkflowLookup.STATE_CONTENT_AUTO_APPROVE_NOTES);
    log.info("Publishing {} event for capturing history for productCode {} with message {} ",
      PRODUCT_INTERNAL_HISTORY_SAVE, productCollection.getProductCode(), internalProductHistoryEventModel);

    kafkaProducer.send(PRODUCT_INTERNAL_HISTORY_SAVE, productCollection.getProductCode(),
      internalProductHistoryEventModel);
  }

  private void saveProductHistory(ProductCollection productCollection, String userName) throws Exception {
    ProductHistory productHistory =
      new ProductHistory(productCollection.getProductId(), ProductWorkflowLookup.STATE_AUTO_APPROVE,
        ProductWorkflowLookup.STATE_AUTO_APPROVE_DESCRIPTION,
        ProductWorkflowLookup.STATE_CONTENT_AUTO_APPROVE_NOTES, userName, new Date(),
        productCollection.getStoreId());

    this.productService.saveProductHistory(productCollection.getProductCode(), productHistory);
  }

  private void handleNonContentAndImageAutoApproval(ProductDetailEditDTO productDetailEditDTO,
    ProductCollection productCollection, EditProductResponse editProductResponse,
    List<String> reviewTypeList, List<ConfigurationStatusResponse> configurationStatusResponseList,
    boolean newImagesAdded) {
    if (CommonUtils.publishPDTContentEditEvent(productDetailEditDTO.isContentChanged(),
      productDetailEditDTO.getRestrictedKeywordsByFieldList(), productDetailEditDTO.isBrandChanged())) {
      handleContentEditEvent(productCollection, editProductResponse, productDetailEditDTO,
        reviewTypeList);
      if(newImagesAdded && checkRestrictedKeywordsInEditedImage){
        productDetailEditDTO.setPublishImageQcForContentChange(false);
      }
    } else {
      handleNonContentEditEvent(productCollection, editProductResponse, productDetailEditDTO,
        configurationStatusResponseList);
    }
  }

  private void handleContentEditEvent(ProductCollection productCollection,
    EditProductResponse editProductResponse, ProductDetailEditDTO productDetailEditDTO,List<String> reviewTypeList) {
      productCollection.setReviewPending(true);
      productCollection.setEdited(true);
    productDetailEditDTO.setAutoApproved(false);
    productDetailEditDTO.setContentType(EditedReviewTypeConstants.CONTENT_EDIT);
    editProductResponse.setProductReview(true);
    if (!reviewTypeList.contains(EditedReviewTypeConstants.CONTENT_EDIT)) {
      reviewTypeList.add(EditedReviewTypeConstants.CONTENT_EDIT);
    }
    if(!productCollection.isPostLive()){
      productDetailEditDTO.setTakeDownProduct(true);
    }
  }

  public void handleNonContentEditEvent(ProductCollection productCollection, EditProductResponse editProductResponse,
      ProductDetailEditDTO productDetailEditDTO, List<ConfigurationStatusResponse> configurationStatusResponseList) {
    editProductResponse.setProductReview(false);
    // Only if product is review i.e reviewPending = true then need to publish CONTENT_REFRESH
    if (productCollection.isReviewPending()) {
      productCollection.setEdited(true);
      productDetailEditDTO.setContentType(CONTENT_REFRESH);
      if (!POST_LIVE.equals(configurationStatusResponseList.get(0).getReviewConfig())) {
        productDetailEditDTO.setTakeDownProduct(true);
      }
      productDetailEditDTO.setAutoApproved(false);
    }
  }


  @Override
  public void createAuditLogsForEdit(ProductLevel3DetailResponse savedData, String accessChannel, boolean sizeChartChanged) throws Exception {
    ProductLevel3DetailResponse updatedData = generateProductLevel3DetailForHistory(savedData.getProductSku());
    UpdateProductItemLevel3Model savedModel =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Detail(savedData, false);
    updatedData.setSizeChartChanged(sizeChartChanged);
    UpdateProductItemLevel3Model updatedModel =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Detail(updatedData, false);
    updatedProductHistoryService
        .saveUpdateProductLevel3Audit(savedData.getBusinessPartnerCode(), Constants.DEFAULT, savedModel, updatedModel,
            accessChannel, updatedData.getProductSku(), updatedData.getProductName(), false,
            StringUtils.EMPTY);
  }

  private ProductLevel3DetailResponse generateProductLevel3DetailForHistory (String productSku) throws Exception {
    ProductL3Response productL3Response = xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
    return generateProductLevel3Detail(productL3Response, new ArrayList<>(), new ArrayList<>(), null);
  }

  public void migrateUnsyncProductForEdit(ProductL3Response product, boolean syncProduct) throws Exception {
    ProductLevel3UpdateRequest productLevel3UpdateRequest =
        new ProductLevel3UpdateRequest(product.getProductSku(), product.getProductCode(), product.getMerchantCode(),
            product.isProductEditable(), product.isSynchronized());
    checkUnsyncUpdate(productLevel3UpdateRequest, syncProduct);
  }

  public ApiErrorCode checkProductVersion(ProductLevel3 product, ProductL3Response savedProductData) {
    if (Objects.nonNull(product.getVersion()) && Objects.nonNull(savedProductData.getVersion())
        && product.getVersion() <= savedProductData.getVersion()) {
      return ApiErrorCode.INVALID_STATE;
    }
    return null;
  }

  private boolean checkContentUpdateForUrl(ProductLevel3 product, ProductLevel3 savedProduct) {
    String existingURL = StringUtils.isNotEmpty(savedProduct.getUrl()) ? savedProduct.getUrl() : StringUtils.EMPTY;
    String newURL = StringUtils.isNotEmpty(product.getUrl()) ? product.getUrl() : StringUtils.EMPTY;
    if (!StringUtils.equals(existingURL, newURL)) {
      LOGGER.info("Change in product url for product sku : {} ", product.getProductSku());
      return true;
    }
    return false;
  }

  public boolean checkContentUpdateForSpecialAttributes(ProductLevel3 product,
   ProductLevel3 savedProduct) {
    try {
      Map<String, ProductLevel3Attribute> productAttributes =
          product.getAttributes().stream().filter(productAttribute -> !isVariantCreationOrBrandAttribute(productAttribute))
              .collect(Collectors.toMap(ProductLevel3Attribute::getAttributeCode, Function.identity()));

      Map<String, ProductLevel3Attribute> savedProductAttributes = savedProduct.getAttributes().stream()
          .filter(productAttribute -> !isVariantCreationOrBrandAttribute(productAttribute))
          .collect(Collectors.toMap(ProductLevel3Attribute::getAttributeCode, Function.identity()));

      if (MapUtils.isNotEmpty(productAttributes) && specialAttributesModified(product,
        productAttributes, savedProductAttributes)) {
        return true;
      }
      //Check for deleted special attributes
      if (ofNullable(product.getAttributes().stream().filter(
          productLevel3Attribute -> Boolean.TRUE.equals(productLevel3Attribute.getSkuValue()))
        .collect(Collectors.toList())).orElse(new ArrayList<>()).size() <
        ofNullable(savedProduct.getAttributes().stream().filter(
            productLevel3Attribute -> Boolean.TRUE.equals(productLevel3Attribute.getSkuValue()))
          .collect(Collectors.toList())).orElse(new ArrayList<>()).size()) {
        return true;
      }
    } catch (Exception e) {
      LOGGER.error("Error while checking the content update for the product sku : {} ", product.getProductSku(), e);
    }
    return false;
  }

  private static boolean specialAttributesModified(ProductLevel3 product,
    Map<String, ProductLevel3Attribute> productAttributes,
    Map<String, ProductLevel3Attribute> savedProductAttributes) {
    for (Map.Entry<String, ProductLevel3Attribute> entry : productAttributes.entrySet()) {
      ProductLevel3Attribute productLevel3Attribute = entry.getValue();
      LOGGER.info("Checking content change for special attribute : {}", productLevel3Attribute);
      if (Boolean.TRUE.equals(productLevel3Attribute.getSkuValue())) {
        ProductLevel3Attribute savedProductLevel3Attribute =
            savedProductAttributes.get(productLevel3Attribute.getAttributeCode());
        // Check for new attribute
        if (Objects.isNull(savedProductLevel3Attribute)) {
          return true;
        }
        if (CollectionUtils.isNotEmpty(productLevel3Attribute.getValues())
          && CollectionUtils.isNotEmpty(savedProductLevel3Attribute.getValues())
          && !StringUtils.equals(StringUtils.trimToEmpty(productLevel3Attribute.getValues().get(0)),
          StringUtils.trimToEmpty(savedProductLevel3Attribute.getValues().get(0)))) {
          LOGGER.info("Special attribute value change for attribute code {} detected for product sku : {} ",
              productLevel3Attribute.getAttributeCode(), product.getProductSku());
          return true;
        }
      }
    }
    return false;
  }

  private void updateProductAndItem(ProductLevel3 product,
      Boolean contentChanged, boolean isOnlyExternal, ProductLevel3 productLevel3, boolean isCategoryChanged) throws Exception {
    BeanUtils.copyProperties(product, productLevel3, "items", "productType", "installationRequired");
    product.setItems(productLevel3.getItems());
    ItemRequest itemRequest =
        generateItemRequest(productLevel3.getItems().get(0), product.getProductSku(), productLevel3.getAttributes());
    ProductRequest productRequest = generateProductRequest(productLevel3);
    if (Objects.isNull(contentChanged)) {
      contentChanged = checkContentChange(product, productLevel3);
    }
    itemRequest.setContentChanged(contentChanged);
    this.xProductOutbound.updateProduct(isOnlyExternal, productRequest);
    this.xProductOutbound.updateItem(isOnlyExternal, false, false, itemRequest);
    this.xProductOutbound.generateProductScoreByProductSkuOrProductCode(product.getProductSku(), null, isCategoryChanged);
  }

  private AddEditedProductToPDTEvent publishEditedProductEventToPDT(String storeId, String contentEdit,
      ProductCollection productCollection, List<String> allModifiedFields) throws Exception {
    return productService.publishAddEditedProductToPDTEvent(storeId, contentEdit, productCollection, allModifiedFields);
  }

  public void brandChangeCheck(String existingBrand, String brand) throws Exception {
    if (!existingBrand.equals(brand)) {
      throw new ApiIncorrectInputDataException(ApiErrorCode.BRAND_CANNOT_BE_CHANGED.getDesc(),
          ApiErrorCode.BRAND_CANNOT_BE_CHANGED);
    }
  }

  private boolean checkIfFreeSampleChanged(ProductLevel3 product, ProductLevel3 savedProduct) {
    return product.isFreeSample() != savedProduct.isFreeSample();
  }

  private boolean checkIfOff2OnActiveChanged(ProductLevel3 product, ProductLevel3 savedProduct) {
    return product.isOff2OnChannelActive() != savedProduct.isOff2OnChannelActive();
  }

  private boolean checkContentChange(ProductLevel3 product, ProductLevel3 savedProduct) {
    try {
      if (isProductNameOrDescriptionChanged(product, savedProduct)) {
        return true;
      }
      if ((StringUtils.isBlank(savedProduct.getUniqueSellingPoint()) && StringUtils
          .isNotBlank(product.getUniqueSellingPoint())) || (StringUtils.isNotBlank(savedProduct.getUniqueSellingPoint())
          && StringUtils.isBlank(product.getUniqueSellingPoint()))) {
        LOGGER.info("Change in USP for product sku : {}, savedUsp : {}, usp : {}", product.getProductSku(),
            savedProduct.getUniqueSellingPoint(), product.getUniqueSellingPoint());
        return true;
      }
      if (StringUtils.isNotBlank(savedProduct.getUniqueSellingPoint()) && StringUtils
          .isNotBlank(product.getUniqueSellingPoint())) {
        String savedUSP = getFilterUSP(savedProduct.getUniqueSellingPoint().trim());
        String usp = getFilterUSP(product.getUniqueSellingPoint().trim());
        if (!savedUSP.equals(usp)) {
          LOGGER
              .info("Change in USP for product sku : {}, savedUsp : {}, usp : {}", product.getProductSku(),
                  savedUSP, usp);
          return true;
        }
      }

      Map<String, ProductLevel3Attribute> productAttributes = product.getAttributes().stream()
          .filter(productAttribute -> !isVariantCreationOrBrandAttribute(productAttribute))
          .collect(Collectors.toMap(ProductLevel3Attribute::getAttributeCode, Function.identity()));

      Map<String, ProductLevel3Attribute> savedProductAttributes = savedProduct.getAttributes().stream()
          .filter(productAttribute -> !isVariantCreationOrBrandAttribute(productAttribute))
          .collect(Collectors.toMap(ProductLevel3Attribute::getAttributeCode, Function.identity()));

      if (MapUtils.isNotEmpty(productAttributes)) {
        for (Map.Entry<String, ProductLevel3Attribute> entry : productAttributes.entrySet()) {
          ProductLevel3Attribute productLevel3Attribute = entry.getValue();
          LOGGER.info("Checking content change for attribute : {}", productLevel3Attribute);
          if (Boolean.TRUE.equals(productLevel3Attribute.getSkuValue())) {
            continue;
          }
          ProductLevel3Attribute savedProductLevel3Attribute =
              savedProductAttributes.get(productLevel3Attribute.getAttributeCode());
          if (Objects.isNull(savedProductLevel3Attribute)) {
            LOGGER.info("Addition of new attribute {} detected for product sku : {} ",
                productLevel3Attribute.getAttributeCode(), product.getProductSku());
            return true;
          }
          if (CollectionUtils.isNotEmpty(productLevel3Attribute.getValues()) && CollectionUtils
              .isNotEmpty(savedProductLevel3Attribute.getValues()) &&  !StringUtils.equals(
                StringUtils.trimToEmpty(productLevel3Attribute.getValues().get(0)),
                StringUtils.trimToEmpty(savedProductLevel3Attribute.getValues().get(0)))){
            LOGGER.info("Attribute value change {} detected for product sku : {} ",
              productLevel3Attribute.getAttributeCode(), product.getProductSku());
            return true;
          }
        }
      }
    } catch (Exception e) {
      LOGGER.error("Error while setting the content change for the product sku : {} ", product.getProductSku(),
          e);
    }
    return false;
  }

  private boolean isProductNameOrDescriptionChanged(ProductLevel3 product, ProductLevel3 savedProduct) {
    if (isProductNameChanged(product, savedProduct)) {
      return true;
    }
    if (CommonUtils.eligibleToCheckIfDescriptionIsChanged(product, savedProduct, instoreNewFlowEnabled)) {
      return CommonUtils.isDescriptionChanged(product, savedProduct);
    }
    return false;
  }


  private static boolean isProductNameChanged(ProductLevel3 product, ProductLevel3 savedProduct) {
    if (StringUtils.isNotBlank(savedProduct.getProductName()) && StringUtils.isNotBlank(product.getProductName())
        && !savedProduct.getProductName().equals(product.getProductName())) {
      LOGGER.info("Change in product name for product sku : {} ", product.getProductSku());
      return true;
    }
    return false;
  }

  private boolean isVariantCreationOrBrandAttribute(ProductLevel3Attribute productLevel3Attribute) {
    return productLevel3Attribute.isVariantCreation() || "DEFINING_ATTRIBUTE"
        .equals(productLevel3Attribute.getAttributeType()) || BRAND
        .equalsIgnoreCase(productLevel3Attribute.getAttributeName());
  }

  private boolean isVariantCreationAttribute(ProductLevel3Attribute productLevel3Attribute) {
    return productLevel3Attribute.isVariantCreation() || "DEFINING_ATTRIBUTE".equals(
        productLevel3Attribute.getAttributeType());
  }

  private boolean isBrandAttribute(ProductLevel3Attribute productLevel3Attribute) {
    return BRAND.equalsIgnoreCase(productLevel3Attribute.getAttributeName());
  }

  private boolean checkPriceOrStockChange(ProductLevel3 productLevel3, ProductLevel3 savedProductData,
      Integer deltaStock, Integer minimumStock) {
    if (Objects.nonNull(minimumStock) && !minimumStock.equals(savedProductData.getItems().get(0).getMinimumStock())) {
      return true;
    }
    if (Objects.nonNull(deltaStock) && !deltaStock.equals(0)) {
      return true;
    }
    if (CollectionUtils.isNotEmpty(productLevel3.getItems().get(0).getPrices()) && CollectionUtils
        .isNotEmpty(savedProductData.getItems().get(0).getPrices())) {
      Iterator<ProductLevel3Price> productLevel3PriceIterator = productLevel3.getItems().get(0).getPrices().iterator();
      Iterator<ProductLevel3Price> savedProductDataPriceIterator =
          savedProductData.getItems().get(0).getPrices().iterator();
      while (productLevel3PriceIterator.hasNext() && savedProductDataPriceIterator.hasNext()) {
        if (!GdnObjects.equals(productLevel3PriceIterator.next(), savedProductDataPriceIterator.next())) {
          return true;
        }
      }
    }
    return false;
  }

  private boolean checkItemChange(ProductLevel3 productLevel3, ProductLevel3 savedProductData) {
    ProductItemLevel3 request = productLevel3.getItems().get(0);
    ProductItemLevel3 savedItem = savedProductData.getItems().get(0);

    String existingMerchantSku =
        StringUtils.isNotEmpty(savedItem.getMerchantSku()) ? savedItem.getMerchantSku() : StringUtils.EMPTY;
    String requestMerchantSku =
        StringUtils.isNotEmpty(request.getMerchantSku()) ? request.getMerchantSku() : StringUtils.EMPTY;
    if (!StringUtils.equals(existingMerchantSku, requestMerchantSku)) {
      return true;
    }
    if (CollectionUtils.isNotEmpty(request.getViewConfigs()) && CollectionUtils
        .isNotEmpty(savedItem.getViewConfigs())) {
      ProductLevel3ViewConfig itemViewConfig = request.getViewConfigs().get(0);
      ProductLevel3ViewConfig currItemViewConfig = savedItem.getViewConfigs().get(0);
      return (currItemViewConfig.getChannelId().equals(itemViewConfig.getChannelId()) && !(
          currItemViewConfig.getBuyable().equals(itemViewConfig.getBuyable())
              && currItemViewConfig.getDisplay().equals(itemViewConfig.getDisplay())));

    }
    return false;
  }

  private ApiErrorCode validateAndUpdateWholesalePrice(ItemRequest itemRequest, String categoryCode,
      Boolean wholesalePriceActivated, String pickupPointCode) throws Exception {
    ApiErrorCode apiErrorCode = null;
    if (Boolean.TRUE.equals(wholesalePriceActivated)) {
      WholesalePriceSkuResponse wholesalePrice =
          this.productPricingOutbound.getWholesalePrice(itemRequest.getItemSku(), pickupPointCode);
      WholesalePriceSkuResponse finalWholesalePriceSkuResponse = wholesalePrice;
      if (!MapUtils.isEmpty(wholesalePrice.getWholesaleRules())) {
        apiErrorCode = wholesaleValidationUtil.validateWholesaleConfigOnUpdate(categoryCode,
            wholesalePrice.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key,
                finalWholesalePriceSkuResponse.getWholesaleRules().get(key))).collect(Collectors.toList()), itemRequest,
            getMinimumPrice(DEFAULT_STORE_ID), itemRequest.getItemSku(), wholesalePriceActivated, null);
        if (Objects.nonNull(apiErrorCode)) {
          validateAndActivateDeactivateWholesalePrice(itemRequest.getWholesalePriceActivated(),
              itemRequest.getItemSku(), wholesalePrice, pickupPointCode);
        }
      }
    }
    return apiErrorCode;
  }

  @Override
  public void updateItem(ItemRequest request, boolean isOnlyExternal) throws Exception {
    ProductAndItemsResponse savedProductData =
        this.productLevel3Repository.findDetailByGdnSku(request.getItemSku());
    this.xProductOutbound.updateItem(isOnlyExternal, false, false, request);
    ProductAndItemsResponse updatedProductData =
        this.productLevel3Repository.findDetailByGdnSku(request.getItemSku());
    createAuditLogs(savedProductData.getProduct().getMerchantCode(), request.getItemSku(),
        savedProductData, updatedProductData, StringUtils.EMPTY, StringUtils.EMPTY);
  }

  @Override
  public ProductLevel3Dashboard findDashboard(String businessPartnerCode) throws Exception {
    PageRequest pageRequest = PageRequest.of(0, 1);
    SortOrder sort = new SortOrder("createdDate", "desc");
    Long totalReady =
        this.productLevel3Repository.findSummaryByFilter(businessPartnerCode, null, null, null,
            null, null, null, true, pageRequest, false, sort).getTotalElements();
    Long totalUnbuyable =
        this.productLevel3Repository.findSummaryByFilter(businessPartnerCode, null, null, null,
            null, null, null, false, pageRequest, false, sort).getTotalElements();
    Long totalUndisplay =
        this.productLevel3Repository.findSummaryByFilter(businessPartnerCode, null, null, null,
            null, null, false, null, pageRequest, false, sort).getTotalElements();
    Long totalProduct =
        this.productLevel3Repository.findSummaryByFilter(businessPartnerCode, null, null, null,
            null, null, null, null, pageRequest, false, sort).getTotalElements();
    return generateProductLevel3Dashboard(totalReady, totalUnbuyable, totalUndisplay, totalProduct);
  }

  private ProductLevel3Dashboard generateProductLevel3Dashboard(Long totalReady,
      Long totalUnbuyable, Long totalUndisplay, Long totalProduct) throws Exception {
    ProductLevel3Dashboard productLevel3Dashboard = new ProductLevel3Dashboard();
    productLevel3Dashboard.setTotalReady(totalReady);
    productLevel3Dashboard.setTotalUnbuyable(totalUnbuyable);
    productLevel3Dashboard.setTotalUndisplay(totalUndisplay);
    productLevel3Dashboard.setTotalProduct(totalProduct);
    return productLevel3Dashboard;
  }

  private Pair<List<ProductLevel3Inventory>, List<String>> generateInventoryForInsert(
      ProductDetailResponse productData, ProductBusinessPartner productBusinessPartner,
      String businessPartnerCode, ProfileResponse businessPartner) throws Exception {
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    List<String> itemSkus = new ArrayList<>();
    Map<String, ProductItemResponse> productItems = new HashMap<String, ProductItemResponse>();
    List<PickupPointResponse> pickupPointCodeResponses = pickupPointOutbound.getByPickupPointCodes(DEFAULT_REQUEST_ID,
        new ArrayList<>(productBusinessPartner.getProductItemBusinessPartners().stream()
            .map(ProductItemBusinessPartner::getPickupPointId).collect(Collectors.toSet())));
    Map<String, Boolean> pickupPointCodesWithFbbActivatedValue =
        CommonUtils.getPickupPointCodesWithFbbActivatedValue(pickupPointCodeResponses);
    for (ProductItemResponse productItem : productData.getProductItemResponses()) {
      productItems.put(productItem.getId(), productItem);
    }
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
        .getProductItemBusinessPartners()) {
      ProductItemResponse productItem =
          productItems.get(productItemBusinessPartner.getProductItemId());

      if (!productItemBusinessPartner.isMarkForDelete()) {
        ProductLevel3Inventory inventory = new ProductLevel3Inventory();

        boolean fbbActivated =
            pickupPointCodesWithFbbActivatedValue.getOrDefault(productItemBusinessPartner.getPickupPointId(), false);
        inventory.setWarehouseMerchantCode(businessPartnerCode);
        if (isPurchaseOrderPurchaseTerm(businessPartner)) {
          inventory.setWarehouseMerchantCode(GdnBaseLookup.DEFAULT_BUSINESS_PARTNER_CODE);
        }

        if (productItem != null) {
          inventory.setWarehouseItemSku(productItem.getSkuCode());
        }
        inventory.setWebMerchantCode(businessPartnerCode);
        inventory.setWebItemSku(productItemBusinessPartner.getGdnProductItemSku());
        inventory.setWebAvailable(productItemBusinessPartner.getStock());
        inventory.setInitialPreOrderQuota(productItemBusinessPartner.getPreOrderQuota());
        inventory.setWebMinAlert(productItemBusinessPartner.getMinimumStock());
        if (inventoryL5SyncStockEnabled) {
          inventory.setWebSyncStock(
            CommonUtils.getSyncStockValueForFASSMerchants(faasFeatureSwitch, fbbActivated,
              businessPartner));
        } else {
          inventory.setWebSyncStock(this.productInventoryService.isSyncedToLevel1Inventory(businessPartner));
        }
        inventory.setWebPickupPointCode(productItemBusinessPartner.getPickupPointId());
        inventory.setProductSku(productBusinessPartner.getGdnProductSku());
        inventory.setFbbPP(fbbActivated && mppForWhEnabled);
        inventory.setDistributionPickupPoint(productItemBusinessPartner.isDistribution());
        CommonUtils.setPreOrderFields(preOrderConfig.isPoQuotaFeatureSwitch(), businessPartner,
            productBusinessPartner.getPreOrderDate(), inventory,
            productItemBusinessPartner.getPreOrderQuota());
        inventories.add(inventory);
        itemSkus.add(productItemBusinessPartner.getGdnProductItemSku());
      }
    }
    return Pair.of(inventories, itemSkus);
  }

  @Override
  @Transactional(rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public boolean create(String businessPartnerCode, ProductDetailResponse productData,
      ProductBusinessPartner productBusinessPartner, boolean takeDownProduct,
      List<ProductLevel3Logistics> logistics) throws Exception {
    Map<String, AttributeResponse> attributes = generateAttribute(productBusinessPartner);
    ProfileResponse businessPartnerProfile =
        businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    if (Objects.nonNull(businessPartnerProfile) && !Constants.ACTIVE.equalsIgnoreCase(businessPartnerProfile.getMerchantStatus())) {
      LOGGER.info("reset inactive business partner :{} product status & stock", businessPartnerCode);
      productBusinessPartner.getProductItemBusinessPartners().stream().forEach(element -> {
        element.setDisplay(false);
        element.setStock(0);
      });
    }
    List<String> itemSkus = new ArrayList<>(productBusinessPartner.getProductItemBusinessPartners().stream().
        map(ProductItemBusinessPartner::getGdnProductItemSku).collect(Collectors.toSet()));
    List<ProductItemWholesalePrice> wholesalePriceList =
        this.productItemWholesalePriceService.findByStoreIdAndItemSkus(productBusinessPartner.getStoreId(), itemSkus);
    Map<String, ProductItemBusinessPartner> pickupPointToProductItemBusinessPartnerMap =
        productBusinessPartner.getProductItemBusinessPartners().stream()
            .filter(productItemBusinessPartner -> !productItemBusinessPartner.isMarkForDelete()).collect(
                Collectors.toMap(
                    productItemBusinessPartner -> productItemBusinessPartner.getGdnProductItemSku() + Constants.HYPHEN
                        + productItemBusinessPartner.getPickupPointId(),
                    productItemBusinessPartner -> productItemBusinessPartner, (a, b) -> b));
    wholesalePriceList = wholesalePriceList.stream().filter(
            productItemWholesalePrice -> pickupPointToProductItemBusinessPartnerMap.containsKey(
                productItemWholesalePrice.getItemSku() + Constants.HYPHEN + productItemWholesalePrice.getPickupPointCode()))
        .collect(Collectors.toList());
    Map<String, ProductItemWholesalePrice> itemSkuToWholesalePriceMap = new HashMap<>();
      itemSkuToWholesalePriceMap = wholesalePriceList.stream().
          collect(Collectors.toMap(
              productItemWholesalePrice -> productItemWholesalePrice.getItemSku() + Constants.HYPHEN
                  + productItemWholesalePrice.getPickupPointCode(), Function.identity()));

    // Pricing call to add wholesale Prices
    if (CollectionUtils.isNotEmpty(wholesalePriceList)) {
      List<List<ProductItemWholesalePrice>> productWholesalePriceSublist =
          Lists.partition(wholesalePriceList, wholesalePriceBatchsize);
      for (List<ProductItemWholesalePrice> subList : productWholesalePriceSublist) {
        WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest =
            getWholesalePriceRequest(productBusinessPartner, subList);
        WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = productPricingOutbound
            .bulkUpdateWholesalePriceV2(Collections.singletonList(wholesalePriceBulkUpdateRequest), businessPartnerCode,
                 productBusinessPartner.getCreatedBy());
        CommonUtils.generateWholesaleSkuStatusMap(itemSkuToWholesalePriceMap,
            wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatus());
      }
    }
    boolean forceReview = false;
    if (takeDownProduct) {
      forceReview = checkProductQcReviewResponse(productData.getStoreId(), productData.getProductCode());
    }
    ProductAndItemActivationRequest productAndItemActivationRequest =
        ProductItemCreationRequestHelper.generateProductLevel3Request(businessPartnerProfile, productData,
            productBusinessPartner, attributes, itemSkuToWholesalePriceMap, forceReview);

    Pair<List<ProductLevel3Inventory>, List<String>> inventories =
        generateInventoryForInsert(productData, productBusinessPartner, businessPartnerCode,
            businessPartnerProfile);
    LOGGER.info("Saving item data in X-inventory for product code in create api: {}", productData.getProductCode());
    if (inventoryPartitionSwitch) {
      List<List<ProductLevel3Inventory>> inventoryPartitonList =
        Lists.partition(inventories.getKey(), inventoryBatchSize);
      for (List<ProductLevel3Inventory> invSubList : inventoryPartitonList) {
        this.productLevel3InventoryService.insertInventory(invSubList);
      }
    } else {
      this.productLevel3InventoryService.insertInventory(inventories.getKey());
    }

    LOGGER.info("Items successfully saved in x-inventory for productCode in create api:: {}", productData.getProductCode());

    LOGGER.info("Saving item data in logistics service for product code in create api: {}",
        productData.getProductCode());
    boolean isLogisticsSaveSuccess = this.productLevel3LogisticsService.saveLogisticsByItemSku(
        productBusinessPartner.getProductItemBusinessPartners().stream()
            .map(ProductItemBusinessPartner::getGdnProductItemSku).collect(Collectors.toList()), businessPartnerCode,
        logistics, true);
    LOGGER.info("Items successfully saved in logistics service for productCode in create api:: {}",
        productData.getProductCode());

    // Save to X-Product
    LOGGER.info("Saving product data in X-Product for productCode : {} and request in create api : {}", productData.getProductCode(),
        productAndItemActivationRequest);
    this.xProductOutbound.addProductAndItems(productBusinessPartner.getStoreId(), Constants.WEB_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, productBusinessPartner.getCreatedBy(),
        productAndItemActivationRequest);
    LOGGER.info("Product successfully updated in X-Product for productCode in create api: : {}", productData.getProductCode());
    for (ProductItemBusinessPartner productLevel3ItemWip : productBusinessPartner.getProductItemBusinessPartners()) {
      if(!skipProductLevel3AggregatorCreation) {
      ProductLevel3Aggregator productLevel3Aggregator =
        this.productLevel3AggregatorService.findByGdnSku(productLevel3ItemWip.getGdnProductItemSku());
      if (Objects.isNull(productLevel3Aggregator)) {
        productLevel3Aggregator = new ProductLevel3Aggregator();
      }
      productLevel3Aggregator.setBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
      productLevel3Aggregator.setGdnSku(productLevel3ItemWip.getGdnProductItemSku());
      productLevel3Aggregator.setMinimumStock(productLevel3ItemWip.getStock() < productLevel3ItemWip.getMinimumStock());
      productLevel3Aggregator.setOos(productLevel3ItemWip.getStock() == 0);
      productLevel3Aggregator.setState(ProductLevel3AggregatorState.ACTIVE);
      this.productLevel3AggregatorService.create(productLevel3Aggregator);
      }
    }

    productLevel3Helper.addBundleRecipeInWMS(productAndItemActivationRequest);

    this.updatedProductHistoryService.saveCreateProductLevel3Audit(businessPartnerCode, productAndItemActivationRequest, productBusinessPartner);
    return isLogisticsSaveSuccess;
  }

  private WholesalePriceBulkUpdateRequest getWholesalePriceRequest(ProductBusinessPartner productBusinessPartner,
      List<ProductItemWholesalePrice> subList) {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    wholesalePriceBulkUpdateRequest.setProductSku(productBusinessPartner.getGdnProductSku());
    wholesalePriceBulkUpdateRequest.setNewProduct(true);
    wholesalePriceBulkUpdateRequest.setWholesalePriceSkuRequests(
        subList.stream().map(this::toWholesalePriceSkuRequest).collect(Collectors.toList()));
    return wholesalePriceBulkUpdateRequest;
  }

  private WholesalePriceSkuRequest toWholesalePriceSkuRequest(ProductItemWholesalePrice productItemWholesalePrice) {
    WholesalePriceSkuRequest wholesalePriceSkuRequest = new WholesalePriceSkuRequest();
    wholesalePriceSkuRequest.setItemCode(productItemWholesalePrice.getItemCode());
    wholesalePriceSkuRequest.setItemSku(productItemWholesalePrice.getItemSku());
    wholesalePriceSkuRequest.setPickUpPointCode(productItemWholesalePrice.getPickupPointCode());
    try {
      List<ProductItemWholesalePriceResponse> productItemWholesalePriceResponses =
          mapperUtil.mapStringToResponse(productItemWholesalePrice.getWholesaleRules());
      Map<Integer, Double> wholesaleRulesMap = productItemWholesalePriceResponses.stream().collect(
          Collectors.toMap(ProductItemWholesalePriceResponse::getQuantity, ProductItemWholesalePriceResponse::getWholesaleDiscount));
      wholesalePriceSkuRequest.setWholesalePriceActivated(productItemWholesalePrice.isWholesalePriceActivated());
      wholesalePriceSkuRequest.setWholesaleRules(wholesaleRulesMap);
    } catch (IOException e) {
      LOGGER.error("Exception caught while mapping the wholesaleprice response itemsku :{}",
          productItemWholesalePrice.getItemSku(), e);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
          ErrorMessages.ERROR_WHILE_CONVERTING_JSON_RESPONSE + productItemWholesalePrice.getItemSku());
    }
    return wholesalePriceSkuRequest;
  }

  private boolean checkProductQcReviewResponse(String storeId, String productCode) {
    ProductImageQcProcessingResponse response =
        productImageQcProcessingResponseService.findByStoreIdAndProductCode(storeId, productCode);
    if (Objects.nonNull(response)) {
      return response.isForceReview();
    }
    return false;
  }

  @Override
  public UsageStatus checkPickupPointCodeUsed(String pickupPointCode) throws Exception {
    UsageStatus usageStatus = new UsageStatus();
    SimpleBooleanResponse simpleBooleanResponse =
        this.productLevel3Repository.checkPickupPointCodeUsed(pickupPointCode);
    usageStatus.setUsed(simpleBooleanResponse.getResult());
    return usageStatus;
  }

  @Override
  public BulkDownloadProductLevel3Summary findProductSummaryForBulkDownload(
      String businessPartnerCode, PageRequest pageRequest, String requestId, String itemName,
      String gdnSku, String categoryCode, Double salePrice, String pickupPointCode, Integer stock,
      boolean isArchived) throws Exception {
    List<String> filterGdnSkus = new ArrayList<>();
    if (StringUtils.isNotBlank(gdnSku)) {
      filterGdnSkus.add(gdnSku);
    }
    try {
      Page<ItemSummaryResponse> productData = this.productLevel3Repository
          .findSummaryByFilter(businessPartnerCode, itemName, filterGdnSkus, categoryCode, salePrice, pickupPointCode,
              null, null, pageRequest, isArchived, new SortOrder(ORDER_BY_ITEM_SKU, SolrConstants.ASC));
      Map<String, List<CategoryResponse>> categoriesData = new HashMap<>();
      Map<String, PickupPointResponse> pickupPointData = new HashMap<>();
      Map<String, ProductLevel3Inventory> inventoryDatas = new HashMap<>();
      Map<String, String> exceptionMap = new HashMap<>();
      if (!productData.getContent().isEmpty()) {
        categoriesData = generateCategoriesData(productData);
        pickupPointData = generatePickupPointDatas(productData);
        List<String> gdnSkus =
            this.productLevel3Converter.convertItemSummaryResponseToListOfGdnSku(
                productData.getContent());
        try {
          List<ProductLevel3Inventory> inventoryList =
              this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
                  businessPartnerCode, gdnSkus);
          inventoryDatas =
              this.productLevel3Converter.convertProductLevel3InventoryToMapOfGdnSku(inventoryList);
        } catch (Exception e) {
          LOGGER
              .error(
                  "retrieval of inventory summary response failed. retry for retrieval of "
                      + "inventory response for each item in progress. businessPartnerCode: {}, pageRequest: {}",
                  businessPartnerCode, pageRequest, e);
          inventoryDatas = generateInventoryForEachItem(businessPartnerCode, gdnSkus, exceptionMap);
        }
      }
      Page<ProductLevel3Summary> productLevel3Summaries =
          generateProductLevel3Summary(productData, categoriesData, pickupPointData,
              inventoryDatas, pageRequest, null);
      return new BulkDownloadProductLevel3Summary(productLevel3Summaries, exceptionMap);
    } catch (Exception e) {
      LOGGER.error("error retrieving product summary at product service. requestId: {}, "
              + "businessPartnerCode: {}, pageRequest: {}", requestId, businessPartnerCode,
          pageRequest, e);
      throw e;
    }
  }

  @Override
  public BulkDownloadProductLevel3Summary findProductSummaryForBulkDownload(String businessPartnerCode,
      PageRequest pageRequest, String requestId, ProductLevel3SummaryFilter filterRequest) throws Exception {
    try {
      Page<ProductLevel3Summary> productData = productLevel3DirectAggregatorService
          .aggregateProductLevel3Summary(filterRequest, pageRequest, new SortOrder(CREATED_DATE, DESC));
      return new BulkDownloadProductLevel3Summary(productData, new HashMap<String, String>() {
      });
    } catch (Exception e) {
      LOGGER.error("error retrieving product summary at product service. requestId: {}, "
          + "businessPartnerCode: {}, pageRequest: {}", requestId, businessPartnerCode, pageRequest, e);
      throw e;
    }
  }

  @Override
  public BulkDownloadProductLevel3Response findProductSummaryForBulkDownloadByDb(
      ProductLevel3SummaryFilter filterRequest, boolean fetchB2bData,
      String fetchViewConfigByChannel) throws Exception {
    BulkDownloadProductLevel3Response productData = new BulkDownloadProductLevel3Response();
    try {
      productData =
          bulkProductLevel3AgrregatorService.aggregateProductLevel3SummaryByDb(filterRequest,
              fetchB2bData, fetchViewConfigByChannel);
    } catch (Exception e) {
      LOGGER.error("error retrieving product summary at product service.", e);
      throw e;
    }
    return productData;
  }

  private Map<String, ProductLevel3Inventory> generateInventoryForEachItem(
      String businessPartnerCode, List<String> gdnSkus, Map<String, String> exceptionMap) {
    Map<String, ProductLevel3Inventory> inventoryDatas = new HashMap<>();
    gdnSkus.stream().forEach(
        gdnSku -> {
          try {
            ProductLevel3Inventory inventory =
                this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndGdnSku(
                    businessPartnerCode, gdnSku);
            if (inventory == null) {
              LOGGER.error("retrieved inventory from inventory is null. "
                  + "businessPartnerCode: {}, itemSku: {}", businessPartnerCode, gdnSku);
              exceptionMap.put(gdnSku, ExceptionMsg.EXCEPTION_NULL_VALUED_FOR_ITEM_SKU.getValue());
            } else {
              inventoryDatas.put(gdnSku, inventory);
            }
          } catch (Exception e) {
            LOGGER.error("error retrieving inventory from inventory. "
                + "businessPartnerCode: {}, gdnSku: {}", businessPartnerCode, gdnSku, e);
            exceptionMap.put(gdnSku, ExceptionMsg.EXCEPTION_TO_FETCH_GIVEN_PRODUCT.getValue());
          }
        });
    return inventoryDatas;
  }

  @Override
  public void updateItemOff2On(Boolean off2OnActiveFlag, String itemSku,
      ItemSummaryResponse productData) throws Exception {
    if (off2OnActiveFlag != null) {
      if (off2OnActiveFlag) {
        this.productLevel3Repository.updateItemOff2OnActivate(itemSku);
      } else {
        this.productLevel3Repository.updateItemOff2OnDeactivate(itemSku);
      }
      if (productData != null) {
        productData.setOff2OnChannelActive(off2OnActiveFlag);
      }
    }
  }

  @Override
  public void updateItemOff2On(Boolean off2OnActiveFlag, String itemSku) throws Exception {
    updateItemOff2On(off2OnActiveFlag, itemSku, null);
  }

  private PickupPointResponse generateDefaultPickupPointResponse() {
    PickupPointResponse response = new PickupPointResponse();
    response.setName("-");
    return response;
  }

  @Override
  @Trace(dispatcher = true)
  public void toggleArchiveItem(String itemSku, boolean doArchive) throws Exception {

    ItemSummaryResponse savedItemData = new ItemSummaryResponse();
    if (doArchive) {
      savedItemData = this.productLevel3Repository.findSummaryByGdnSku(itemSku);
    } else {
      savedItemData = this.productLevel3Repository.findSummaryByArchivedGdnSku(itemSku);
    }
    checkState(!savedItemData.isForceReview(), ErrorMessages.ITEM_NOT_EDITABLE);
    boolean buyable = false;
    boolean discoverable = false;
    Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<>();
    itemViewConfigs = savedItemData.getItemViewConfigs();
    for (ItemViewConfigDTO itemViewConfigDTO : itemViewConfigs) {
      if (itemViewConfigDTO.isBuyable()) {
        buyable = true;
      }

      if (itemViewConfigDTO.isDiscoverable()) {
        discoverable = true;
      }
    }
    String merchantCode = savedItemData.getMerchantCode();

    this.productLevel3Repository.toggleArchiveItem(itemSku, doArchive);

    if (doArchive) {
      if (!skipProductLevel3AggregatorCreation) {
        productLevel3AggregatorService
            .updateState(itemSku, savedItemData.getMerchantCode(), ProductLevel3AggregatorState.ARCHIVED);
      }
      this.archiveProductStockAlert(merchantCode, itemSku, true, 0);
    } else {
      if (!skipProductLevel3AggregatorCreation) {
        productLevel3AggregatorService
            .updateState(itemSku, savedItemData.getMerchantCode(), ProductLevel3AggregatorState.ACTIVE);
      }
      this.archiveProductStockAlert(merchantCode, itemSku, false, 0);
    }

    if (buyable) {
      this.updatedProductHistoryService.createProductL3AuditLog(merchantCode, itemSku, savedItemData.getProductSku(),
          savedItemData.getGeneratedItemName(), UpdateProductActivity.BUYABLE.getDesc(), String.valueOf(buyable),
          String.valueOf(!buyable), false, StringUtils.EMPTY);
    }
    if (discoverable) {
      this.updatedProductHistoryService.createProductL3AuditLog(merchantCode, itemSku, savedItemData.getProductSku(),
          savedItemData.getGeneratedItemName(), UpdateProductActivity.DISPLAYABLE.getDesc(),
          String.valueOf(discoverable), String.valueOf(!discoverable), false, StringUtils.EMPTY);
    }
    this.updatedProductHistoryService.createProductL3AuditLog(merchantCode, itemSku, savedItemData.getProductSku(),
        savedItemData.getGeneratedItemName(), UpdateProductActivity.ARCHIVE.getDesc(), String.valueOf(!doArchive),
        String.valueOf(doArchive), false, StringUtils.EMPTY);
  }

  @Override
  public Long getProductsCountByBrand(String brand) throws Exception {
    return this.productLevel3Repository.getProductCountByBrand(brand);
  }

  @Override
  public EstimateItemPriceDTO estimatePriceForFlow2ProductCreation(String itemCode, double lowestPriceCoefficient,
      int maxInventoryRequest) throws Exception {
    List<ItemPriceResponse> itemPriceResponses = productLevel3Repository.getItemSkusToFetchEstimatedPrice(itemCode);
    EstimateItemPriceDTO estimateItemPriceDTO = new EstimateItemPriceDTO();
    estimateItemPriceDTO.setNormalPrice(getEstimatedNormalPrice(itemPriceResponses, maxInventoryRequest));
    estimateItemPriceDTO
        .setOfferPrice(estimateOfferPrice(itemPriceResponses, lowestPriceCoefficient, maxInventoryRequest));
    return estimateItemPriceDTO;
  }

  /**
   * estimate offer price of item present in stock, minimum but greater than  Q1 - x*IQR .
   * where
   * Q1 - first quartile of the distribution
   * Q3 - third quartile
   * IQR = Q3 - Q1
   *
   * @param itemPriceResponses list of items
   * @param lowestPriceCoefficient factor decides offer price selection, default = 1.5
   * @param maxInventoryRequest no of items in inventory request
   * @return estimated Offer price
   */
  private double estimateOfferPrice(List<ItemPriceResponse> itemPriceResponses, double lowestPriceCoefficient,
      int maxInventoryRequest) {
    List<ItemPriceResponse> itemPriceResponsesOrderByOfferPrice =
        itemPriceResponses.stream().sorted((itemPriceResponse1, itemPriceResponse2) -> ((int)(itemPriceResponse1.getOfferPrice() - itemPriceResponse2.getOfferPrice())))
            .collect(Collectors.toList());
    if (!itemPriceResponses.isEmpty()) {
      if (itemPriceResponses.size() <= 2) {
        return getItemInStockWithBestPrice(itemPriceResponsesOrderByOfferPrice, maxInventoryRequest, ZERO_INDEX, Boolean.FALSE);
      } else {
        return calculateOfferPrice(itemPriceResponsesOrderByOfferPrice, lowestPriceCoefficient, maxInventoryRequest);
      }
    } else
      return EstimateItemPriceDTO.ESTIMATED_PRICE_NOT_AVAILABLE;
  }

  /**
   * 1: calculate quatrile 1, quatrile 3 and IQR
   * 2: calculate least price
   * 3: get Item In Stock With BestPrice
   * 4: for 1st product fullfilling 3rd step criteria, return it's offer price
   *
   * @param itemPriceResponsesOrderByOfferPrice item list sort by offer price in ascending order
   * @param lowestPriceCoefficient factor decides offer price selection, default = 1.5
   * @param maxInventoryRequest no of items in inventory request
   * @return calculated offer price
   * @throws Exception
   */
  private double calculateOfferPrice(List<ItemPriceResponse> itemPriceResponsesOrderByOfferPrice,
      double lowestPriceCoefficient, int maxInventoryRequest) {
    double quartile1;
    double quartile3;
    double interQuartileRange;
    double lowestPrice;
    int index = 0;
    int splitCount = itemPriceResponsesOrderByOfferPrice.size() / 2;
    int quarter = splitCount/2;
    if (itemPriceResponsesOrderByOfferPrice.size() % 2 == 0) {
      if (splitCount % 2 == 0) {
        quartile1 = (itemPriceResponsesOrderByOfferPrice.get(quarter - 1).getOfferPrice()
            + itemPriceResponsesOrderByOfferPrice.get(quarter).getOfferPrice()) / 2.0;
        quartile3 = (itemPriceResponsesOrderByOfferPrice.get(splitCount + quarter - 1).getOfferPrice()
            + itemPriceResponsesOrderByOfferPrice.get(splitCount + quarter).getOfferPrice()) / 2.0;
      } else {
        quartile1 = itemPriceResponsesOrderByOfferPrice.get(quarter).getOfferPrice();
        quartile3 = itemPriceResponsesOrderByOfferPrice.get(splitCount + (quarter)).getOfferPrice();
      }
    } else {
      if (splitCount % 2 == 0) {
        quartile1 = (itemPriceResponsesOrderByOfferPrice.get(quarter - 1).getOfferPrice()
            + itemPriceResponsesOrderByOfferPrice.get(quarter).getOfferPrice()) / 2.0;
        quartile3 = (itemPriceResponsesOrderByOfferPrice.get(splitCount + quarter + 1).getOfferPrice()
            + itemPriceResponsesOrderByOfferPrice.get(splitCount + quarter).getOfferPrice()) / 2.0;
      } else {
        quartile1 = itemPriceResponsesOrderByOfferPrice.get(quarter).getOfferPrice();
        quartile3 = itemPriceResponsesOrderByOfferPrice.get(splitCount + (quarter) + 1).getOfferPrice();
      }
    }
    interQuartileRange = quartile3 - quartile1;
    lowestPrice = quartile1 - lowestPriceCoefficient * interQuartileRange;
    while (!itemPriceResponsesOrderByOfferPrice.isEmpty() && lowestPrice > itemPriceResponsesOrderByOfferPrice
        .get(index).getOfferPrice()){
      index++;
    }
    return  getItemInStockWithBestPrice(itemPriceResponsesOrderByOfferPrice, maxInventoryRequest, index, Boolean.FALSE);
  }

  /**
   * estimate normal price based on maximum frequency of normal price
   *
   * @param itemPriceResponses items by which normal price will be estimated
   * @param maxInventoryRequest no of items in inventory request
   * @return estimated normal price
   */
  private double getEstimatedNormalPrice(List<ItemPriceResponse> itemPriceResponses, int maxInventoryRequest) {
    Map<Double, List<ItemPriceResponse>> itemNormalPriceAggregateSort =
        itemPriceResponses.stream().collect(Collectors.groupingBy(ItemPriceResponse::getListPrice, Collectors.toList()))
            .entrySet().stream().sorted((itemPriceResponses1, itemPriceResponses2) -> {
          if (itemPriceResponses1.getValue().size() == itemPriceResponses2.getValue().size()) {
            return (int) (itemPriceResponses2.getKey() - itemPriceResponses1.getKey());
          } else {
            return itemPriceResponses2.getValue().size() - itemPriceResponses1.getValue().size();
          }
        }).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (key1, key2) -> key2, LinkedHashMap::new));

    List<ItemPriceResponse> itemNormalPriceEstimateList = new ArrayList<>();
    for (Map.Entry<Double, List<ItemPriceResponse>> entry : itemNormalPriceAggregateSort.entrySet()){
      itemNormalPriceEstimateList.addAll(entry.getValue());
    }

    return getItemInStockWithBestPrice(itemNormalPriceEstimateList, maxInventoryRequest, ZERO_INDEX, Boolean.TRUE);
  }

  /**
   * check product in inventory in group of items
   * @param itemPriceResponses list of items to check if any one from 0 index present in Inventory or not
   * @param maxInventoryRequest maximum no of items in one request of productLevel3InventoryService
   * @param estimateNormalPrice flag to check whether to get normal price or offer price
   * @return Item In Stock With Best Price
   */
  private double getItemInStockWithBestPrice(List<ItemPriceResponse> itemPriceResponses,
      int maxInventoryRequest, int startIndex, boolean estimateNormalPrice) {
    Map<String, String> inventorySkusMap = new LinkedHashMap<>();
    Map<String, Double> itemSkusMap = new HashMap<>();
    try{
      for (int idx = startIndex; idx < itemPriceResponses.size(); idx++) {
        ItemPriceResponse itemPriceResponse = itemPriceResponses.get(idx);
        inventorySkusMap.put(itemPriceResponse.getItemSku(), itemPriceResponse.getMerchantCode());
        itemSkusMap.put(itemPriceResponse.getItemSku(),
            estimateNormalPrice ? itemPriceResponse.getListPrice() : itemPriceResponse.getOfferPrice());
        if (inventorySkusMap.size() >= maxInventoryRequest || idx == itemPriceResponses.size() - 1) {
          List<ProductLevel3Inventory> inventoryByGdnSkuMap =
              productLevel3InventoryService.findInventoryByGdnSkuMap(inventorySkusMap);
          for (ProductLevel3Inventory productLevel3Inventory : inventoryByGdnSkuMap) {
            if (productLevel3Inventory.getWebAvailable() > 0) {
              return itemSkusMap.getOrDefault(productLevel3Inventory.getWebItemSku(),
                  EstimateItemPriceDTO.ESTIMATED_PRICE_NOT_AVAILABLE);
            }
          }
          inventorySkusMap.clear();
          itemSkusMap.clear();
        }
      }
    } catch (Exception ex) {
      LOGGER.error("Error while geting Item InStock With Best Price for ItemSkus {}", inventorySkusMap, ex);
    }
    return EstimateItemPriceDTO.ESTIMATED_PRICE_NOT_AVAILABLE;
  }

  @Override
  public ProductAndItemsResponse findDetailByProductSku(String productSku) throws Exception {
    ProductAndItemsResponse productAndItemsResponse = null;
    try {
      productAndItemsResponse =  productLevel3Repository.findDetailByProductSku(productSku);
    } catch (ApplicationException ex) {
      LOGGER.error("Error while fetching product data for productSku = {}", productSku);
    }
    return productAndItemsResponse;
  }

  @Override
  public void archiveProductStockAlert(String businessPartnerCode, String gdnSku, boolean archived, int retry) throws Exception {
    LOGGER.info("archiveProductStockAlert with param : {},{},{},{}",businessPartnerCode, gdnSku, archived, retry);
    List<PbpStockAlert> pbpStockAlertList = this.findPbpStockAlertByGdnSkuCode(gdnSku);
    for (PbpStockAlert pbpStockAlert : pbpStockAlertList) {
      if (archived) {
        pbpStockAlert.setMarkForDelete(true);
      } else {
        pbpStockAlert.setMarkForDelete(false);
      }
      LOGGER.info("archiveProductStockAlert with value : {}", pbpStockAlert);
        try {
          this.productStockAlertRepository.save(pbpStockAlert);
        } catch (ObjectOptimisticLockingFailureException ofe) {
          LOGGER.error(
              "ObjectOptimisticLockingFailureException occured in ProductStockAlertServiceBean.archiveGdnSku ! Retry again",
              ofe);
          LOGGER.info("ProductStockAlertServiceBean.archiveGdnSku retry again with value : {},{},{},{}",
              businessPartnerCode, gdnSku, archived, retry + 1);
        } catch (Exception e) {
          LOGGER.error("Exception occured in ProductStockAlertServiceBean.archiveGdnSku ! Retry again", e);
          LOGGER.info("ProductStockAlertServiceBean.archiveGdnSku retry again with value : {},{},{},{}",
              businessPartnerCode, gdnSku, archived, retry + 1);
        }
    }
  }

  private PbpStockAlert findOnePbpStockAlertByGdnSkuCode(String gdnSku) {
    List<PbpStockAlert> results = this.productStockAlertRepository
        .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(gdnSku);
    if (!CollectionUtils.isEmpty(results)) {
      return results.get(0);
    }
    return null;
  }

  private List<PbpStockAlert> findPbpStockAlertByGdnSkuCode(String gdnSku) {
    return of(productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(gdnSku))
        .orElse(new ArrayList<>());
  }

  private PbpStockAlert findOneDeletedPbpStockAlertByGdnSkuCode(String gdnSku) {
    List<PbpStockAlert> results = this.productStockAlertRepository
        .findByGdnSkuAndMarkForDeleteTrueOrderByUpdatedDateDesc(gdnSku);
    if (!CollectionUtils.isEmpty(results)) {
      return results.get(0);
    }
    return null;
  }

  public PbpStockAlert createPbpStockAlert(String gdnSku, String businessPartnerCode)
      throws Exception {
    try {
      String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
      ItemSummaryResponse productData = this.productLevel3Repository.findSummaryByGdnSku(gdnSku);
      ProductLevel3Summary productLevel3Summary =
          generateProductLevel3SummarySingle(businessPartnerCode, gdnSku, productData);

      PbpStockAlert pbpStockAlert = new PbpStockAlert();
      pbpStockAlert.setStoreId(storeId);
      pbpStockAlert.setBusinessPartnerCode(businessPartnerCode);
      pbpStockAlert.setGdnSku(gdnSku);
      pbpStockAlert.setOosAlertAttempt(0);
      pbpStockAlert.setProductName(productLevel3Summary.getItemName());

      if (productLevel3Summary.getSynchronizeStock()) {
        pbpStockAlert.setAvailableStock(productLevel3Summary.getAvailableStockLevel1());
        pbpStockAlert.setMinimumStock(productLevel3Summary.getMinimumStockLevel2());
        pbpStockAlert.setIsOos(!(productLevel3Summary.getAvailableStockLevel1() > 0));
        if(productLevel3Summary.getAvailableStockLevel1() > 0) {
          pbpStockAlert.setOosDate(null);
        } else {
          pbpStockAlert.setOosDate(new Date());
        }
        pbpStockAlert.setIsMinimumStock(
            !(productLevel3Summary.getAvailableStockLevel1() > productLevel3Summary
                .getMinimumStockLevel2() || pbpStockAlert.getIsOos()));
      } else {
        pbpStockAlert.setAvailableStock(productLevel3Summary.getAvailableStockLevel2());
        pbpStockAlert.setMinimumStock(productLevel3Summary.getMinimumStockLevel2());
        pbpStockAlert.setIsOos(!(productLevel3Summary.getAvailableStockLevel2() > 0));
        if(productLevel3Summary.getAvailableStockLevel2() > 0) {
          pbpStockAlert.setOosDate(null);
        } else {
          pbpStockAlert.setOosDate(new Date());
        }
        pbpStockAlert.setIsMinimumStock(
            !(productLevel3Summary.getAvailableStockLevel2() > productLevel3Summary
                .getMinimumStockLevel2() || pbpStockAlert.getIsOos()));
      }
      return pbpStockAlert;
    } catch (Exception e) {
      LOGGER.error("Error while createPbpStockAlert for gdnSku = {}", gdnSku);
      return null;
    }
  }

  @Override
  public List<String> bulkArchiveItems(List<String> itemSkus, String businessPartnerCode) {
    String businessPartnerPrefix = businessPartnerCode + "-";
    List<String> failedItemSkus = new ArrayList<>();
    for (String itemSku : itemSkus) {
      try {
        if (itemSku.startsWith(businessPartnerPrefix)) {
          toggleArchiveItem(itemSku, true);
        } else {
          failedItemSkus.add(itemSku);
        }
      } catch (Exception ex) {
        LOGGER.error("Error while archiving itemSku : {} ", itemSku);
        failedItemSkus.add(itemSku);
      }
    }
    LOGGER.info("Bulk Archive : Total Count : {} , Failed Skus : {}", itemSkus.size(), failedItemSkus);
    return failedItemSkus;
  }

  @Override
  public List<PredefinedAllowedAttributeValueResponse> getAllActiveBrandsByCNCategoryId(String requestId,
      String userName, String categoryId, boolean clearCache) throws Exception {
    GdnRestSingleResponse<SingleObjectResponse> finalParentCategoryCached =
        this.categoryRepository.getFinalParentCategoryCached(requestId, userName, categoryId);
    String parentCategoryCode = String.valueOf(finalParentCategoryCached.getValue().getValue());
    if (redisEnabled && redisTemplate.hasKey(getCacheKeyForCategoryCodes(parentCategoryCode)) && !clearCache) {
      return (List<PredefinedAllowedAttributeValueResponse>) redisTemplate
          .boundValueOps(getCacheKeyForCategoryCodes(parentCategoryCode)).get();
    } else {
      if (clearCache) {
        redisTemplate.delete(getCacheKeyForCategoryCodes(parentCategoryCode));
      }
      List<String> categoryCodes =
          this.categoryRepository.getAllChildCategoriesByC1CategoryCode(requestId, userName, parentCategoryCode);
      List<PredefinedAllowedAttributeValueResponse> activeBrands =
          this.solrActiveProductCollectionServiceBean.getAllActiveBrandsByCategoryCodes(categoryCodes);
      redisTemplate.boundValueOps(getCacheKeyForCategoryCodes(parentCategoryCode)).set(activeBrands);
      redisTemplate.expire(getCacheKeyForCategoryCodes(parentCategoryCode), Long.valueOf(redisCacheExpiration),
          TimeUnit.SECONDS);
      return activeBrands;
    }
  }

  private String getCacheKeyForCategoryCodes(String parentCategoryCode) {
    return CacheKeys.BRANDS_BY_CATEGORY_CODE + SolrConstants.COLON + ProductFieldNames.CATEGORY_CODE + "-"
        + parentCategoryCode;
  }

  @Override
  public Page<SuspensionProductResponse> getAllProducts(SummaryFilterRequest summaryFilterRequest, String requestId,
      String userName, String storeId, Pageable pageable) throws Exception {
    String catalogCode = EMPTY_STRING;
    List<String> categoryCodes = new ArrayList<>();
    boolean isCatalogCodeRequired = applicationProperties.isCatalogCodeRequired();
    if (!StringUtils.isEmpty(summaryFilterRequest.getCategoryCode())) {
      if (isCatalogCodeRequired) {
        if (redisEnabled && redisTemplate.hasKey(CacheKeys.CATALOG_CODE)) {
          catalogCode = (String) redisTemplate.boundValueOps(CacheKeys.CATALOG_CODE).get();
        } else {
          List<CatalogResponse> catalogResponses = productRepository.getCatalogByType(CatalogType.MASTER_CATALOG,
            pageable);
          if (CollectionUtils.isNotEmpty(catalogResponses)) {
            catalogCode = catalogResponses.get(0).getCatalogCode();
            redisTemplate.boundValueOps(CacheKeys.CATALOG_CODE).set(catalogCode);
            redisTemplate.expire(CacheKeys.CATALOG_CODE, Long.valueOf(redisCacheExpiration), TimeUnit.SECONDS);
          }
        }
      }
      categoryCodes = this.categoryRepository
          .getAllChildCategoriesByC1CategoryCode(requestId, userName, summaryFilterRequest.getCategoryCode());
      if(CollectionUtils.isEmpty(categoryCodes)) {
        categoryCodes = new ArrayList<>();
        categoryCodes.add(summaryFilterRequest.getCategoryCode());
      }
    }
    Page<ActiveProductResponse> result = productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable, categoryCodes, catalogCode, isCatalogCodeRequired);
    return new PageImpl<>(convertToSuspensionProductResponse(result.getContent()), pageable,
        result.getTotalElements());
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  @Override
  public void takeDownOrReactivateProduct(String storeId, String productSku, boolean isTakeDown, String productName, ProductL3Response productL3Response)
      throws Exception {
    ProductBusinessPartner productBusinessPartner = productBusinessPartnerRepository.findFirstByGdnProductSku(productSku);
    ProfileResponse profileResponse =
        businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    boolean businessPartnerActive =
        Objects.nonNull(profileResponse) && MerchantStatus.ACTIVE.name().equals(profileResponse.getMerchantStatus());
    String categoryCode = productBusinessPartner.getCategoryCode();
    List<ItemSummaryDetailResponse> itemSummaryDetailResponses = new ArrayList<>();
    List<ItemLevel5Response> itemLevel5ResponseList = new ArrayList<>();
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    boolean scheduleRemoval = false;
    boolean isArchived = true;
    int page = 0;
    int size = Integer.valueOf(
        productSystemParameterService.findByStoreIdAndVariable(storeId, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE)
            .getValue());
    if (StringUtils.isNotEmpty(productName)) {
      productBusinessPartner.setProductName(productName);
    }

    if (Objects.isNull(productL3Response)) {
      productL3Response = xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
    }
      CommonUtils.setMasterDataDetailsFromProductL3Response(productL3Response, productBusinessPartner);

    productBusinessPartner = productBusinessPartnerService.updateProductBusinessPartnerState(productBusinessPartner,
        isTakeDown, categoryCode, productL3Response);
    if (isTakeDown) {
        itemLevel5ResponseList = xProductOutbound.getL5ItemListing(Collections.singleton(productSku), new ArrayList<>(),
            new ArrayList<>(), false, Constants.ALL);
        itemSummaryDetailResponses = ConverterUtil.convertToItemSummaryDetailResponse(itemLevel5ResponseList,
            productL3Response.getProductType());
      ProductBusinessPartnerAndItemViewConfigDto productBusinessPartnerAndItemViewConfigDto = productBusinessPartnerService.
          updateProductItemBusinessPartnerStateTakeDownTrue(itemSummaryDetailResponses, itemViewConfigAndItemSkuRequests, productBusinessPartner);
      productBusinessPartner = productBusinessPartnerAndItemViewConfigDto.getProductBusinessPartner();
      itemViewConfigAndItemSkuRequests = productBusinessPartnerAndItemViewConfigDto.getItemViewConfigAndItemSkuRequests();
    } else {
      itemViewConfigAndItemSkuRequests = productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownFalse(
          itemViewConfigAndItemSkuRequests, productBusinessPartner, businessPartnerActive);
    }
    scheduleRemoval = isScheduleRemoval(productBusinessPartner);
    isArchived = checkArchived(productBusinessPartner);
            xProductOutbound.updateItemViewConfigAndForceReview(isTakeDown,
        itemViewConfigAndItemSkuRequests, isArchived, scheduleRemoval);
    productBusinessPartnerRepository.save(productBusinessPartner);
  }

  private boolean isScheduleRemoval(ProductBusinessPartner productBusinessPartner) {
    return schedulesAddEditEnabled && (productBusinessPartner.isFreeSample()
      || !productBusinessPartner.isOnline());
  }

  private static boolean checkArchived(ProductBusinessPartner productBusinessPartner) {
    return productBusinessPartner.getProductItemBusinessPartners().stream().allMatch(GdnBaseEntity::isMarkForDelete);
  }

  private List<ItemSummaryDetailResponse> getItemSummaryResponses(String productSku, int page, int size) throws Exception {
    Page<ItemSummaryDetailResponse> itemSummaryDetailResponses;
    do {
      ItemsSummaryDetailRequest itemsSummaryDetailRequest = new ItemsSummaryDetailRequest();
      itemsSummaryDetailRequest.setProductSku(productSku);
      itemSummaryDetailResponses = xProductOutbound
          .findSummaryDetailsByFilter(itemsSummaryDetailRequest, PageRequest.of(page, size));
      page++;
    } while (page * size < itemSummaryDetailResponses.getTotalElements());
    return itemSummaryDetailResponses.getContent();
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void activateProductOnNeedCorrection(String storeId, String productSku,
    ProfileResponse profileResponse, List<ProductCategoryResponse> categoryResponseList) throws Exception {
    ProductBusinessPartner productBusinessPartner = productBusinessPartnerRepository.findFirstByGdnProductSku(productSku);
    Map<String, Boolean> productItemWholesalePriceMap = getProductItemWholesalePriceMap(storeId, productBusinessPartner);
    GdnPreconditions.checkArgument(Objects.nonNull(profileResponse), ErrorMessages.BUSINESS_PARTNER_PROFILE_SHOULD_NOT_BE_NULL);
    settingFlagForInActiveMerchant(profileResponse, productBusinessPartner);
    ProductCollection productCollection = productCollectionRepository.findProductByGdnSku(productSku);
    productBusinessPartner.setState(ProductLevel1State.ACTIVE);
    productBusinessPartner.setActivated(true);
    productBusinessPartner.setMarkForDelete(true);
    PreOrderDTO preOrderDTO = PreOrderDTO.builder().isPreOrder(productBusinessPartner.isPreOrder())
        .preOrderDate(productBusinessPartner.getPreOrderDate()).preOrderType(productBusinessPartner.getPreOrderType())
        .preOrderValue(productBusinessPartner.getPreOrderValue()).build();
    Integer productType = productBusinessPartner.getProductItemBusinessPartners().get(0).getProductType();
    NeedCorrectionProductActivationRequest activationRequest =
        NeedCorrectionProductActivationRequest.builder()
            .preOrder(preOrderDTO)
            .productType(ConverterUtil.getProductTypeFromCode(productType))
            .productSku(productSku)
            .freeSample(productBusinessPartner.isFreeSample())
            .off2OnChannelActive(productBusinessPartner.isOff2OnChannelActive())
            .online(productBusinessPartner.isOnline())
            .cncActivated(productBusinessPartner.isCncActivated())
            .fbbActivated(productBusinessPartner.isFbbActivated())
            .b2bActivated(productBusinessPartner.isB2bActivated())
            .b2cActivated(productBusinessPartner.isB2cActivated())
            .bundleProduct(productBusinessPartner.isBundleProduct())
            .sizeChartCode(productBusinessPartner.getSizeChartCode())
            .itemRequest(productBusinessPartner.getProductItemBusinessPartners().stream()
                .filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete)).map(productItemBusinessPartner -> ConverterUtil
                    .getNeedRevisionActivationRequest(productItemBusinessPartner, productItemWholesalePriceMap, cncForWarehouseFeatureSwitch))
                .collect(Collectors.toList())).build();
    ActivateNeedRevisionResponse activateNeedRevisionResponse = xProductOutbound.activateOnNeedCorrection(activationRequest);
    if (activateNeedRevisionResponse.isCreateNew()) {
      ProductDetailResponse productData =
          this.productRepository.findProductDetailByProductCode(productCollection.getProductCode());
      this.create(productBusinessPartner.getBusinessPartnerId(), productData, productBusinessPartner, false,
          new ArrayList<>());
    } else {
      productLevel3V2Service.insertInventoryForNewlyAddedL5DuringNeedRevision(productBusinessPartner,
          activateNeedRevisionResponse.getNewlyAddedL5Responses(), profileResponse);
    }
    productBusinessPartnerRepository.save(productBusinessPartner);
    productCollection.setActivated(true);
    productCollection.setViewable(true);
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    productCollection.setState("ACTIVE");
    productCollectionRepository.save(productCollection);
    updateSolrProductCollectionDocument(productCollection);
  }


  private static void settingFlagForInActiveMerchant(ProfileResponse profileResponse, ProductBusinessPartner productBusinessPartner) {
    if (!MerchantStatus.ACTIVE.name().equals(profileResponse.getMerchantStatus())) {
      productBusinessPartner.setCncActivated(false);
      productBusinessPartner.setFbbActivated(false);
      productBusinessPartner.setOnline(false);
      productBusinessPartner.setOff2OnChannelActive(false);
      productBusinessPartner.getProductItemBusinessPartners().stream().forEach(productItemBusinessPartner -> {
        productItemBusinessPartner.setBuyable(false);
        productItemBusinessPartner.setDisplay(false);
        productItemBusinessPartner.setCncActive(false);
        productItemBusinessPartner.setFbbActive(false);
      });
    }
  }

  private Map<String, Boolean> getProductItemWholesalePriceMap(String storeId, ProductBusinessPartner productBusinessPartner) {
    Map<String, Boolean> productItemWholesalePriceMap = new HashMap<>();
    List<ProductItemWholesalePrice> productItemWholesalePrices = productItemWholesalePriceService
        .findByStoreIdAndItemSkus(storeId, new ArrayList<>(productBusinessPartner.getProductItemBusinessPartners().stream()
            .map(ProductItemBusinessPartner::getGdnProductItemSku).collect(Collectors.toSet())));
      productItemWholesalePrices.forEach(productItemWholesalePrice -> productItemWholesalePriceMap.put(
          productItemWholesalePrice.getItemSku() + Constants.HYPHEN + productItemWholesalePrice.getPickupPointCode(),
          productItemWholesalePrice.isWholesalePriceActivated()));
    return productItemWholesalePriceMap;
  }

  @Override
  public void retrySkipReviewProductActivation(String storeId, List<String> productSkuList) {
    for (String productCode : productSkuList) {
      try {
        productServiceWrapper.retrySkipReviewProductActivation(productCode);
      } catch (Exception e) {
        LOGGER.error("Product activation failed for product code : {}", productCode, e);
      }
    }
  }

  @Override
  public void updateProductItemViewConfig(ProductLevel3ViewConfigStockRequest productLevel3ViewConfigRequest,
      String productSku) throws Exception {
    List<ProductBusinessPartner> productBusinessPartnerList =
        this.productBusinessPartnerRepository.findByStoreIdAndGdnProductSkuIn(DEFAULT_STORE_ID,
            Arrays.asList(productSku));

    if (CollectionUtils.isEmpty(productBusinessPartnerList.get(0).getProductItemBusinessPartners())) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "product item not found");
    }
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartnerList.get(0)
        .getProductItemBusinessPartners()) {
      productItemBusinessPartner.setDisplay(productLevel3ViewConfigRequest.getDisplay());
      productItemBusinessPartner.setBuyable(productLevel3ViewConfigRequest.getBuyable());
      if (!cncForWarehouseFeatureSwitch) {
        productItemBusinessPartner.setCncActivated(productLevel3ViewConfigRequest.isCncActive());
      }
      productItemBusinessPartner.setCncBuyable(productLevel3ViewConfigRequest.isCncActive());
      productItemBusinessPartner.setCncDiscoverable(productLevel3ViewConfigRequest.isCncActive());
      if (Objects.nonNull(productLevel3ViewConfigRequest.getAvailableStock())) {
        productItemBusinessPartner.setStock(productLevel3ViewConfigRequest.getAvailableStock());
      }
      if (Objects.nonNull(productLevel3ViewConfigRequest.getB2bBuyable())) {
        productItemBusinessPartner.setB2bBuyable(productLevel3ViewConfigRequest.getB2bBuyable());
      }
      if (Objects.nonNull(productLevel3ViewConfigRequest.getB2bDisplay())) {
        productItemBusinessPartner.setB2bDiscoverable(productLevel3ViewConfigRequest.getB2bDisplay());
      }
      productItemBusinessPartner.setUpdatedBy(productLevel3ViewConfigRequest.getChannelId());
      productItemBusinessPartner.setUpdatedDate(new Date());
    }
    this.productBusinessPartnerRepository.saveAll(productBusinessPartnerList);
  }

  @Override
  public EditProductResponse updateProductLevel3Info(UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest,
      String productSku) throws Exception {
    EditProductResponse editProductResponse = new EditProductResponse();
    ApiErrorCode apiErrorCode;
    if (sanitizeProductNameAndSellerSku) {
      updateProductLevel3InfoRequest.setProductName(
          ValidationUtil.validateDataForProductName(updateProductLevel3InfoRequest.getProductName()));
    }
    if(validateUpdateProductLevel3Info){
      updateProductLevel3InfoRequest.getDimension().setProductType(updateProductLevel3InfoRequest.getProductType());
      ApiErrorCode dimensionErr = productLevel3V2Service.validateShippingAndDimensionForEdit(updateProductLevel3InfoRequest.getDimension(),true);
      if (Objects.nonNull(dimensionErr)) {
        editProductResponse.setApiErrorCode(dimensionErr);
        return editProductResponse;
      }
    }
    ProductL3Response savedProductData = xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
    String gdnSku = savedProductData.getDefaultItemSku();
    ProductAndItemsResponse productData = this.productLevel3Repository.findDetailByGdnSku(gdnSku);
    PickupPointResponse pickupPointData = new PickupPointResponse();
    pickupPointData.setCode(productData.getItems().get(0).getPickupPointCode());
    ProductLevel3 productLevel3 =
        generateProductLevel3(productData, null, pickupPointData, null, null, null, new HashMap<>(), new ArrayList<>(), false);
    productLevel3.setProductEditable(savedProductData.isProductEditable());
    CategoryDetailResponse categoryResponse= null;
    Map<String, ProductLevel3Attribute> categoryAttributesMap = new HashMap<>();
    if (MapUtils.isNotEmpty(updateProductLevel3InfoRequest.getAttributes())) {
      categoryResponse = productOutbound.getCategoryDetailByCategoryCode(productLevel3.getCategoryCode());
      categoryAttributesMap = categoryResponse.getCategoryAttributes().stream()
          .map(categoryAttributeResponse -> ConverterUtil.toProductLevel3Attribute(categoryAttributeResponse))
          .filter(categoryAttributeResponse -> !isVariantCreationOrBrandAttributeOrFamilyColour(categoryAttributeResponse))
          .collect(Collectors.toMap(ProductLevel3Attribute::getAttributeCode, Function.identity()));
    }
    ProfileResponse profileResponse =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(savedProductData.getMerchantCode());
    apiErrorCode =
        validateUpdateProductLevel3InfoRequest(updateProductLevel3InfoRequest, productLevel3, categoryAttributesMap,
            profileResponse);
    if (Objects.nonNull(apiErrorCode)) {
      editProductResponse.setApiErrorCode(apiErrorCode);
      return editProductResponse;
    }

    //Update dimension, productType and logistics
    List<ProductLevel3Logistics> productLevel3Logistics = new ArrayList<>();
    ApiErrorCode bpBopisApiErrorCode =
      productService.checkBpBopisEligibility(updateProductLevel3InfoRequest.getProductType(),
        profileResponse);
    if (Objects.nonNull(bpBopisApiErrorCode)) {
      editProductResponse.setApiErrorCode(bpBopisApiErrorCode);
      return editProductResponse;
    }
    if(validateUpdateProductLevel3Info && Objects.nonNull(categoryResponse)) {
        ApiErrorCode categoryErr =
                CommonUtils.checkCategoryBopisEligibility(updateProductLevel3InfoRequest.getProductType(), categoryResponse, savedProductData.getProductCode());
        if (Objects.nonNull(categoryErr)) {
          editProductResponse.setApiErrorCode(categoryErr);
          return editProductResponse;
        }
    }
    LOGGER.info("findProfileByBusinessPartnerCode : {}", profileResponse);
    if (StringUtils.isNotBlank(profileResponse.getCompany().getMerchantDeliveryType())) {
      productLevel3Logistics =
          productLevel3LogisticsService.findLogisticsByItemSku(gdnSku, savedProductData.getMerchantCode(),
              profileResponse.getCompany().getMerchantDeliveryType());
      LOGGER.info("Logistics info for itemsku : {}, merchant code : {}, deliveryType : {}, productLevel3Logistics : {}",
          gdnSku, savedProductData.getMerchantCode(), profileResponse.getCompany().getMerchantDeliveryType(),
          productLevel3Logistics);
    }
    apiErrorCode = isValidLogistics(productLevel3Logistics, updateProductLevel3InfoRequest.getLogistics());
    if (Objects.nonNull(apiErrorCode)) {
      editProductResponse.setApiErrorCode(apiErrorCode);
      return editProductResponse;
    }
    ProductLevel3DetailResponse productLevel3DetailResponse =
        generateProductLevel3Detail(savedProductData, new ArrayList<>(), productLevel3Logistics, profileResponse);
    ProductLevel3UpdateRequest productLevel3UpdateRequest =
        regenerateProductLevel3UpdateRequest(updateProductLevel3InfoRequest, productLevel3, productLevel3Logistics);
    apiErrorCode = updateLogistics(productLevel3UpdateRequest, true, productLevel3DetailResponse, false, false, migrateProductInOtherLogisticUpdateFlow);
    if (Objects.nonNull(apiErrorCode)) {
      editProductResponse.setApiErrorCode(apiErrorCode);
      return editProductResponse;
    }

    //update instore
    if (Objects.nonNull(updateProductLevel3InfoRequest.getInStore())
        && savedProductData.isOff2OnChannelActive() != updateProductLevel3InfoRequest.getInStore()) {
      updateOff2OnActiveFlag(productLevel3.getBusinessPartnerCode(), productLevel3.getProductSku(),
          productLevel3.getProductName(), updateProductLevel3InfoRequest.getInStore());
      productLevel3.setOff2OnChannelActive(updateProductLevel3InfoRequest.getInStore());
    }

    //Product L3 info change
    boolean productInfoChanged =
        isProductInfoChangedAndSetupdatedRequestValue(productLevel3, updateProductLevel3InfoRequest, categoryAttributesMap);
    if (productInfoChanged) {
      productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(productLevel3, editProductResponse, profileResponse, false,
          null, new ArrayList<>(), new ProductL3UpdateRequest(), false, false);
      editProductResponse = updateEditInfo(productLevel3, true, true, false, profileResponse,
        false, savedProductData, false, editProductResponse.getProductCollection(), null);
    }
    return editProductResponse;
  }

  private ApiErrorCode isValidLogistics(List<ProductLevel3Logistics> productLevel3Logistics,
      List<ProductItemLogisticsRequest> logistics) {
    if (CollectionUtils.isNotEmpty(logistics)) {
      Map<String, Boolean> existingLogisticsMap = productLevel3Logistics.stream().collect(
          Collectors.toMap(ProductLevel3Logistics::getLogisticProductCode, ProductLevel3Logistics::isSelected));
      for (ProductItemLogisticsRequest productItemLogisticsRequest : logistics) {
        if (!existingLogisticsMap.containsKey(productItemLogisticsRequest.getLogisticProductCode())) {
          return ApiErrorCode.INVALID_LOGISTICS_OPTION_SELECTED;
        }
      }
    }
    return null;
  }

  private boolean isVariantCreationOrBrandAttributeOrFamilyColour(ProductLevel3Attribute productLevel3Attribute) {
    return productLevel3Attribute.isVariantCreation() || "DEFINING_ATTRIBUTE".equals(
        productLevel3Attribute.getAttributeType()) || BRAND.equalsIgnoreCase(productLevel3Attribute.getAttributeName())
        || Constants.FAMILY_COLOUR.equalsIgnoreCase(productLevel3Attribute.getAttributeName());
  }

  private ProductLevel3UpdateRequest regenerateProductLevel3UpdateRequest(
      UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest, ProductLevel3 product,
      List<ProductLevel3Logistics> existingProductLevel3Logistics) throws Exception {
    ProductLevel3UpdateRequest productLevel3UpdateRequest = new ProductLevel3UpdateRequest();
    BeanUtils.copyProperties(product, productLevel3UpdateRequest);
    productLevel3UpdateRequest.setSynchronize(product.getSynchronize());
    productLevel3UpdateRequest.setShippingWeight(product.getItems().get(0).getShippingWeight());
    productLevel3UpdateRequest.setLength(product.getItems().get(0).getLength());
    productLevel3UpdateRequest.setWeight(product.getItems().get(0).getWeight());
    productLevel3UpdateRequest.setWidth(product.getItems().get(0).getWidth());
    productLevel3UpdateRequest.setHeight(product.getItems().get(0).getHeight());
    productLevel3UpdateRequest.setLateFulfillment(product.getItems().get(0).getLateFulfillment());
    productLevel3UpdateRequest.setDangerousGoodsLevel(product.getItems().get(0).getDangerousGoodsLevel());
    productLevel3UpdateRequest.setProductLevel3LogisticsRequest(product.getItems().get(0).getLogistics());
    if (Objects.nonNull(updateProductLevel3InfoRequest.getProductType())) {
      productLevel3UpdateRequest.setProductType(updateProductLevel3InfoRequest.getProductType());
    }
    if (Objects.nonNull(updateProductLevel3InfoRequest.getDimension())) {
      productLevel3UpdateRequest.setLength(updateProductLevel3InfoRequest.getDimension().getLength());
      productLevel3UpdateRequest.setWidth(updateProductLevel3InfoRequest.getDimension().getWidth());
      productLevel3UpdateRequest.setHeight(updateProductLevel3InfoRequest.getDimension().getHeight());
      productLevel3UpdateRequest.setWeight(updateProductLevel3InfoRequest.getDimension().getWeight() / 1000);
      productLevel3UpdateRequest.setShippingWeight(
          generatorService.generateShippingWeight(updateProductLevel3InfoRequest.getDimension().getLength(),
              updateProductLevel3InfoRequest.getDimension().getWidth(),
              updateProductLevel3InfoRequest.getDimension().getHeight(),
              updateProductLevel3InfoRequest.getDimension().getWeight() / 1000, product.getCategoryCode()));
    }
    if (Objects.nonNull(updateProductLevel3InfoRequest.getPreOrder())) {
      productLevel3UpdateRequest.setPreOrder(updateProductLevel3InfoRequest.getPreOrder());
    }
    if (Objects.nonNull(updateProductLevel3InfoRequest.getLogistics())) {
      List<ProductLevel3Logistics> productLevel3LogisticsRequest = new ArrayList<>();
      Map<String, Boolean> updateProductLevel3InfoRequestMap = updateProductLevel3InfoRequest.getLogistics().stream()
          .collect(Collectors.toMap(ProductItemLogisticsRequest::getLogisticProductCode,
              ProductItemLogisticsRequest::isSelected));
      for (ProductLevel3Logistics productLevel3Logistics : existingProductLevel3Logistics) {
        if (updateProductLevel3InfoRequestMap.containsKey(productLevel3Logistics.getLogisticProductCode())) {
          productLevel3Logistics.setSelected(
              updateProductLevel3InfoRequestMap.get(productLevel3Logistics.getLogisticProductCode()));
        }
        productLevel3LogisticsRequest.add(productLevel3Logistics);
      }
      productLevel3UpdateRequest.setProductLevel3LogisticsRequest(productLevel3LogisticsRequest);
    } else {
      productLevel3UpdateRequest.setProductLevel3LogisticsRequest(existingProductLevel3Logistics);
    }
    return productLevel3UpdateRequest;
  }

  private boolean isProductInfoChangedAndSetupdatedRequestValue(ProductLevel3 productLevel3,
      UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest,
      Map<String, ProductLevel3Attribute> categoryAttributesMap) {
    boolean isContentChange = false;
    if (!StringUtils.equals(updateProductLevel3InfoRequest.getProductName(),
        productLevel3.getProductName())) {
      productLevel3.setProductName(updateProductLevel3InfoRequest.getProductName());
      isContentChange = true;
    }
    if (StringUtils.isNotEmpty(updateProductLevel3InfoRequest.getDescription())) {
      String updatedDescription =
          new String(Base64.getDecoder().decode(updateProductLevel3InfoRequest.getDescription()));
      if (!StringUtils.equals(updatedDescription, productLevel3.getDescription())) {
        productLevel3.setDescription(updatedDescription);
        isContentChange = true;
      }
    }
    if (StringUtils.isNotEmpty(updateProductLevel3InfoRequest.getUniqueSellingPoint())) {
      String updatedUsp =
          new String(Base64.getDecoder().decode(updateProductLevel3InfoRequest.getUniqueSellingPoint()));
      if (!StringUtils.equals(updatedUsp, productLevel3.getUniqueSellingPoint())) {
        productLevel3.setUniqueSellingPoint(updatedUsp);
        isContentChange = true;
      }
    }
    if (StringUtils.isNotEmpty(updateProductLevel3InfoRequest.getVideoUrl()) && !StringUtils.equals(
        updateProductLevel3InfoRequest.getVideoUrl(), productLevel3.getUrl())) {
      productLevel3.setUrl(updateProductLevel3InfoRequest.getVideoUrl());
      isContentChange = true;
    }
    Map<String, String> requestedProductAttribute = updateProductLevel3InfoRequest.getAttributes();
    List<ProductLevel3Attribute> updatedProductLevel3Attribute = productLevel3.getAttributes();
    if (MapUtils.isNotEmpty(requestedProductAttribute)) {
      Map<String, ProductLevel3Attribute> savedProductAttributes = productLevel3.getAttributes().stream()
          .filter(productAttribute -> !isVariantCreationOrBrandAttributeOrFamilyColour(productAttribute))
          .collect(Collectors.toMap(ProductLevel3Attribute::getAttributeCode, Function.identity()));
      for (String attributeCode : requestedProductAttribute.keySet()) {
        if (savedProductAttributes.containsKey(attributeCode)) {
          if (!StringUtils.equals(savedProductAttributes.get(attributeCode).getValues().get(0),
              requestedProductAttribute.get(attributeCode))) {
            savedProductAttributes.get(attributeCode)
                .setValues(Arrays.asList(requestedProductAttribute.get(attributeCode)));
            isContentChange = true;
          }
        } else {
          LOGGER.debug("Adding new attribute for productSku : {} AttributeCode : {}", productLevel3.getProductSku(),
              attributeCode);
          ProductLevel3Attribute newAttribute = categoryAttributesMap.get(attributeCode);
          newAttribute.setValues(Arrays.asList(requestedProductAttribute.get(attributeCode)));
          updatedProductLevel3Attribute.add(newAttribute);
          isContentChange = true;
        }
      }
      productLevel3.setAttributes(updatedProductLevel3Attribute);
    }

    if (brandCategoryEditEnabledForExternal && CommonUtils.isBrandNameUpdated(
        updateProductLevel3InfoRequest.getBrandName(), productLevel3.getBrand())) {
      productLevel3.setBrandCode(updateProductLevel3InfoRequest.getBrandCode());
      productLevel3.setBrand(updateProductLevel3InfoRequest.getBrandName());
      isContentChange = true;
    }

    if (brandCategoryEditEnabledForExternal && CommonUtils.isCategoryCodeUpdated(
        updateProductLevel3InfoRequest.getCategoryCode(), productLevel3.getCategoryCode())) {
      productLevel3.setCategoryCode(updateProductLevel3InfoRequest.getCategoryCode());
      productLevel3.setCategoryId(updateProductLevel3InfoRequest.getCategoryId());
      productLevel3.setCategoryName(updateProductLevel3InfoRequest.getCategoryName());
      isContentChange = true;
    }

    return isContentChange;
  }

  private ApiErrorCode validateUpdateProductLevel3InfoRequest(
      UpdateProductLevel3InfoRequest updateProductLevel3InfoRequest, ProductLevel3 savedProductData,
      Map<String, ProductLevel3Attribute> categoryAttributesMap, ProfileResponse profileResponse) throws Exception {
    ApiErrorCode apiErrorCode = null;
    if (StringUtils.isEmpty(updateProductLevel3InfoRequest.getProductName().trim())) {
      LOGGER.error("Product Name is empty for productSku : {}", savedProductData.getProductSku());
      return ApiErrorCode.PRODUCT_NAME_IS_EMPTY;
    }
    boolean isOfficialSeller = Optional.of(profileResponse.isOfficial()).orElse(false);
    if (!StringUtils.equalsIgnoreCase(updateProductLevel3InfoRequest.getProductName(),
        savedProductData.getProductName()) && !isOfficialSeller) {
      AgpSimpleQueryResponse agpResponse =
          agpQueryFeign.findNumberOfOrder(savedProductData.getProductSku(), String.valueOf(0), String.valueOf(0),
              Constants.AGP_ITEM_STATUS);
      if (Objects.isNull(agpResponse) || Objects.isNull(agpResponse.getHits())) {
        throw new Exception("Error while fetching a response from agp for order status for product sku "
            + savedProductData.getProductSku());
      }
      if (agpResponse.getHits().getTotal() > 0) {
        LOGGER.error("Product has order for productSku : {}", savedProductData.getProductSku());
        return ApiErrorCode.PRODUCT_HAS_ORDER;
      }
    }
    Map<String, String> requestedProductAttribute = updateProductLevel3InfoRequest.getAttributes();
    if (MapUtils.isNotEmpty(requestedProductAttribute)) {
      for (String attributeCode : requestedProductAttribute.keySet()) {
        ProductLevel3Attribute productLevel3Attribute = categoryAttributesMap.get(attributeCode);
        if (Objects.isNull(productLevel3Attribute)) {
          LOGGER.error("Invalid product attribute update productSku : {} attributeCode : {} ",
              savedProductData.getProductSku(), attributeCode);
          apiErrorCode = ApiErrorCode.PRODUCT_ATTRIBUTE_INVALID;
          break;
        } else {
          if(productLevel3Attribute.getAttributeType().equals(AttributeType.PREDEFINED_ATTRIBUTE.name())) {
            PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = null;
            try {
              predefinedAllowedAttributeValueResponse =
                  this.predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(
                      GdnMandatoryRequestParameterUtil.getRequestId(), attributeCode,
                      requestedProductAttribute.get(attributeCode));
            } catch (Exception e) {
              LOGGER.error("Invalid predefined attribute value for attribute code : {}", attributeCode, e);
              apiErrorCode = ApiErrorCode.PRODUCT_ATTRIBUTE_INVALID;
              break;
            }
            if (Objects.isNull(predefinedAllowedAttributeValueResponse)) {
              LOGGER.error("Invalid predefined attribute value for attribute code : {}", attributeCode);
              apiErrorCode = ApiErrorCode.PRODUCT_ATTRIBUTE_INVALID;
              break;
            }
          }
        }
      }
    }
    return  apiErrorCode;
  }

  private List<SuspensionProductResponse> convertToSuspensionProductResponse(List<ActiveProductResponse> result) {
    List<SuspensionProductResponse> suspensionProductResponseList = new ArrayList<>();
    for (ActiveProductResponse activeProductResponse : result) {
      SuspensionProductResponse suspensionProductResponse = new SuspensionProductResponse();
      BeanUtils.copyProperties(activeProductResponse, suspensionProductResponse);
      List<String> itemSkus = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(activeProductResponse.getItemDetailResponses())) {
        for (ItemDetailResponse item : activeProductResponse.getItemDetailResponses()) {
          itemSkus.add(item.getItemSku());
        }
      }
      suspensionProductResponse.setItemSku(itemSkus);
      suspensionProductResponse
          .setCategoryCode(getCategoryCodeFromMasterCatalog(activeProductResponse.getMasterCatalog()));
      suspensionProductResponse.setBusinessPartnerCode(activeProductResponse.getMerchantCode());
      suspensionProductResponse.setState(activeProductResponse.getStatus());
      suspensionProductResponse.setProductSku(activeProductResponse.getProductSku());
      suspensionProductResponseList.add(suspensionProductResponse);
    }
    return suspensionProductResponseList;
  }

  private String getCategoryCodeFromMasterCatalog(String masterCatalog){
    if(StringUtils.isNotBlank(masterCatalog) && masterCatalog.contains(DELIMITER)){
      String codes[] =  masterCatalog.split(DELIMITER);
      return codes[1];
    }
    return masterCatalog;
  }

  @Override
  public void saveSuspensionHistory(String storeId, String productSku, String businessPartnerCode, String reason, String description,
      SuspensionStatus status, String createdBy, Date createdDate) {
    ProductSuspensionHistory productSuspensionHistory = productSuspensionHistoryRepository
        .findTop1ByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId, productSku);
    ProductSuspensionHistory history = null;

    if (Objects.nonNull(productSuspensionHistory) && SuspensionStatus.SUSPENDED.equals(status)) {
      history = new ProductSuspensionHistory(productSku, productSuspensionHistory.getBusinessPartnerCode(), reason,
          description, status, productSuspensionHistory.getCount() + 1, createdBy, createdDate, storeId);
    } else if (Objects.isNull(productSuspensionHistory) && SuspensionStatus.SUSPENDED.equals(status)) {
      history = new ProductSuspensionHistory(productSku, businessPartnerCode, reason, description, status, 1, createdBy,
          createdDate, storeId);
    } else if (Objects.isNull(productSuspensionHistory) && SuspensionStatus.ACTIVE.equals(status)) {
      history = new ProductSuspensionHistory(productSku, businessPartnerCode, reason, description, status, 0, createdBy,
          createdDate, storeId);
    } else if (Objects.nonNull(productSuspensionHistory) && SuspensionStatus.ACTIVE.equals(status)) {
      if (reason.contains(INVALID_REASON)) {
        history = new ProductSuspensionHistory(productSku, businessPartnerCode, reason, description, status,
            productSuspensionHistory.getCount() - 1, createdBy, createdDate, storeId);
      } else {
        history = new ProductSuspensionHistory(productSku, businessPartnerCode, reason, description, status,
            productSuspensionHistory.getCount(), createdBy, createdDate, storeId);
      }
    }
    LOGGER.info("Saving the suspension history {} for productSku {} with status {}", history, productSku, status);
    productSuspensionHistoryRepository.saveAndFlush(history);
  }

  @Override
  public Page<SuspensionItemResponse> getSuspendedItems(SummaryFilterRequest summaryFilterRequest, String requestId,
      String userName, String storeId, Pageable pageable) throws Exception {
    String catalogCode = EMPTY_STRING;
    boolean isCatalogCodeRequired = applicationProperties.isCatalogCodeRequired();
    if (!CollectionUtils.isEmpty(summaryFilterRequest.getCategoryCodes())) {
      if (isCatalogCodeRequired) {
        if (redisEnabled && redisTemplate.hasKey(CacheKeys.CATALOG_CODE)) {
          catalogCode = (String) redisTemplate.boundValueOps(CacheKeys.CATALOG_CODE).get();
        } else {
          List<CatalogResponse> catalogResponses = productRepository.getCatalogByType(CatalogType.MASTER_CATALOG,
            pageable);
          if (CollectionUtils.isNotEmpty(catalogResponses)) {
            catalogCode = catalogResponses.get(0).getCatalogCode();
            redisTemplate.boundValueOps(CacheKeys.CATALOG_CODE).set(catalogCode);
            redisTemplate.expire(CacheKeys.CATALOG_CODE, Long.valueOf(redisCacheExpiration), TimeUnit.SECONDS);
          }
        }
      }
    }
    Page<ItemSummaryResponse> result = productLevel3Repository
        .getSuspendedItemList(summaryFilterRequest, pageable, summaryFilterRequest.getCategoryCodes(), catalogCode, isCatalogCodeRequired);
    return new PageImpl<SuspensionItemResponse>(convertToSuspensionItemResponse(result.getContent(), storeId), pageable,
        result.getTotalElements());
  }

  @Override
  public Page<ProductSuspensionHistory> getSuspensionHistory(String storeId, String productSku, PageRequest pageRequest)
      throws Exception {
    Page<ProductSuspensionHistory> productSuspensionHistories = productSuspensionHistoryRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId, productSku, pageRequest);
    return productSuspensionHistories;
  }

  @Override
  public List<ProductResponse> getProductsByProductCodeAndMerchantCode(String productCode, String merchantCode)
      throws Exception {
    return productLevel3Repository.getProductsByProductCodeAndMerchantCode(productCode, merchantCode);
  }

  @Override
  public CountProductLevel3InactiveResponse countSummaryForInactiveProduct(String storeId, String requestId,
      String userName, String businessPartnerCode) throws Exception {
    CountProductLevel3InactiveResponse countProductLevel3InactiveResponse = new CountProductLevel3InactiveResponse();
    Map<ProductLevel3InactiveSummaryCriteria, Long> totalItemsByCriterias = new HashMap<>();
    LOGGER.info("Fetching the rejected product counts for merchant code : {}", businessPartnerCode );
    long rejectedCount =
        productBusinessPartnerService.countRejectedProductsByBusinessPartnerId(storeId, businessPartnerCode);
    totalItemsByCriterias.put(ProductLevel3InactiveSummaryCriteria.REJECTED, rejectedCount);
    LOGGER.info("Fetching the suspended product counts for merchant code : {}", businessPartnerCode );
    long suspendedCount = productBusinessPartnerService
        .countSuspendedProductsByBusinessPartnerCode(storeId, requestId, userName, businessPartnerCode);
    totalItemsByCriterias.put(ProductLevel3InactiveSummaryCriteria.SUSPENDED, suspendedCount);
    LOGGER.info("Fetching the archived product counts for merchant code : {}", businessPartnerCode );
    Page<ItemSummaryResponse> itemSummaryResponsePage = productLevel3Repository
        .findSummaryByFilter(businessPartnerCode, null, null, null, null, null, null, null, PageRequest.of(0, 1), true,
            null);
    totalItemsByCriterias
        .put(ProductLevel3InactiveSummaryCriteria.ARCHIVED, itemSummaryResponsePage.getTotalElements());
    countProductLevel3InactiveResponse.setTotalItemsByCriterias(totalItemsByCriterias);
    return countProductLevel3InactiveResponse;
  }

  private List<SuspensionItemResponse> convertToSuspensionItemResponse(List<ItemSummaryResponse> result,
      String storeId) {
    List<String> productCodes = new ArrayList<>();
    for (ItemSummaryResponse itemSummaryResponse : result) {
      if (StringUtils.isNotBlank(itemSummaryResponse.getProductCode())) {
        productCodes.add(itemSummaryResponse.getProductCode());
      }
    }

    Map<String, ProductCollection> productCollectionMap = new HashMap<>();
    if (!CollectionUtils.isEmpty(productCodes)) {
      for (ProductCollection productCollection : productCollectionRepository
          .findByProductCodeInAndStoreIdAndMarkForDeleteFalse(productCodes, storeId)) {
        if (!StringUtils.isBlank(productCollection.getProductCode())) {
          productCollectionMap.put(productCollection.getProductCode(), productCollection);
        }
      }
    }

    List<SuspensionItemResponse> suspensionItemResponses = new ArrayList<>();
    for (ItemSummaryResponse itemSummaryResponse : result) {
      SuspensionItemResponse suspensionItemResponse = new SuspensionItemResponse();
      ProductCollection product = productCollectionMap.get(itemSummaryResponse.getProductCode());
      if (Objects.isNull(product)) {
        LOGGER.warn("could not find the product in product_collection with productSku : {}",
            itemSummaryResponse.getProductSku());
        suspensionItemResponse.setCategoryName(DELIMITER_DASH);
      } else {
        suspensionItemResponse.setCategoryName(product.getCategoryName());
      }

      BeanUtils.copyProperties(itemSummaryResponse, suspensionItemResponse);
      suspensionItemResponse.setItemName(itemSummaryResponse.getGeneratedItemName());
      suspensionItemResponse.setBusinessPartnerCode(itemSummaryResponse.getMerchantCode());

      ProductSuspensionHistory productSuspensionHistory = productSuspensionHistoryRepository
          .findTop1ByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId,
              itemSummaryResponse.getProductSku());
      if (Objects.isNull(productSuspensionHistory)) {
        LOGGER
            .warn("could not find history for the product with productSku : {}", itemSummaryResponse.getProductSku());
        suspensionItemResponse.setReason(DELIMITER_DASH);
      } else {
        suspensionItemResponse.setReason(productSuspensionHistory.getDescription());
        suspensionItemResponse.setBannedDate(productSuspensionHistory.getCreatedDate());
      }
      if (!CollectionUtils.isEmpty(itemSummaryResponse.getMasterDataItemImages())) {
        suspensionItemResponse.setImageUrl(itemSummaryResponse.getMasterDataItemImages().get(0).getLocationPath());
      }
      suspensionItemResponse.setProductDetailPageLink(toProductDetailPage(itemSummaryResponse.getItemSku()));
      suspensionItemResponses.add(suspensionItemResponse);
    }
    return suspensionItemResponses;
  }

  private String toProductDetailPage(String itemSKU) {
    return String.join(SKU_SEPARATOR, applicationProperties.getProductDetailPageUrlPrefix(), toProductDetailsSKU(itemSKU));
  }

  private String toProductDetailsSKU(String itemSKU) {
    return ofNullable(itemSKU).map(sku -> sku.substring(0, itemSKU.lastIndexOf(SKU_SEPARATOR)))
        .map(sku -> sku.replace(SKU_SEPARATOR, DOT_SEPARATOR)).map(sku -> String.join(DOT_SEPARATOR, sku, HTML))
        .orElse(StringUtils.EMPTY);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public void doSuspensionProductsActions(String storeId, String username, SuspensionProductRequest suspensionProductRequest)
      throws Exception {
    if (SUSPEND.equals(suspensionProductRequest.getAction())) {
      suspendProducts(storeId, username, suspensionProductRequest);
    } else if (REACTIVATE.equals(suspensionProductRequest.getAction())) {
      reactivateProducts(storeId, username, suspensionProductRequest);
    }
  }

  private void suspendProducts(String storeId, String username, SuspensionProductRequest suspensionProductRequest) throws Exception {
    for (ProductLevel3Request request : suspensionProductRequest.getProducts()) {
      ProductL3Response productL3Response =
          xProductOutbound.getProductDetailsByProductSku(request.getProductSku()).getValue();
      LOGGER.info("productL3Response {} recieved for the request {}" , productL3Response, request);
      if (Objects.nonNull(productL3Response) ) {
        if (!productL3Response.isSuspended()) {
          productLevel3Repository.toggleSuspensionProduct(productL3Response.getProductSku(), true);
          for (String itemSku : productL3Response.getItemSkus()) {
            if (!skipProductLevel3AggregatorCreation) {
              productLevel3AggregatorService
                  .updateState(itemSku, productL3Response.getMerchantCode(), ProductLevel3AggregatorState.ARCHIVED);
            }
              this.archiveProductStockAlert(productL3Response.getMerchantCode(), itemSku, true, 0);
          }
          this.updatedProductHistoryService.createProductL3AuditLog(request.getBusinessPartnerCode(), Constants.DEFAULT,
              productL3Response.getProductSku(),
             productL3Response.getMasterDataProduct().getProductName(),
              UpdateProductActivity.SUSPENSION.getDesc(), String.valueOf(false), String.valueOf(true), false,
              StringUtils.EMPTY);
          saveSuspensionHistory(storeId, productL3Response.getProductSku(),
             productL3Response.getMerchantCode(), suspensionProductRequest.getReason(),
              suspensionProductRequest.getNotes(), SuspensionStatus.SUSPENDED, username,
              Calendar.getInstance().getTime());
          LOGGER.info("Successfully saving the suspended emailEvent for productSku {} ", productL3Response.getProductSku());
          this.productMailEventService.createAndSaveMailEventForSuspensionOrReActivation(ProductMailEventsEnum.SUSPENDED,
             productL3Response.getMerchantCode(), suspensionProductRequest.getNotes(),
              productL3Response.getProductCode(),
              productL3Response.getProductSku());
        }
      }
    }
  }

  private void reactivateProducts(String storeId, String username, SuspensionProductRequest suspensionProductRequest) throws Exception {
    for (ProductLevel3Request request : suspensionProductRequest.getProducts()) {
      ProductL3Response productL3Response =
          xProductOutbound.getProductDetailsByProductSku(request.getProductSku()).getValue();
      if (Objects.nonNull(productL3Response) ) {
        if (productL3Response.isSuspended()) {
          productLevel3Repository.toggleSuspensionProduct(productL3Response.getProductSku(), false);
          for (String itemSku : productL3Response.getItemSkus()) {
            if (!skipProductLevel3AggregatorCreation) {
              productLevel3AggregatorService
                  .updateState(itemSku, productL3Response.getMerchantCode(), ProductLevel3AggregatorState.ACTIVE);
            }
              this.archiveProductStockAlert(productL3Response.getMerchantCode(), itemSku, false, 0);
          }
          this.updatedProductHistoryService.createProductL3AuditLog(request.getBusinessPartnerCode(), Constants.DEFAULT,
              productL3Response.getProductSku(),
              productL3Response.getMasterDataProduct().getProductName(),
              UpdateProductActivity.SUSPENSION.getDesc(), String.valueOf(true), String.valueOf(false), false,
              StringUtils.EMPTY);
          saveSuspensionHistory(storeId, productL3Response.getProductSku(),
              productL3Response.getMerchantCode(), suspensionProductRequest.getReason(),
              suspensionProductRequest.getNotes(), SuspensionStatus.ACTIVE, username,
              Calendar.getInstance().getTime());
          LOGGER.info("Successfully saving the re-activation emailEvent for productSku {} ", productL3Response.getProductSku());
          this.productMailEventService.createAndSaveMailEventForSuspensionOrReActivation(ProductMailEventsEnum.RE_ACTIVATED,
             productL3Response.getMerchantCode(), suspensionProductRequest.getNotes(),
              productL3Response.getProductCode(),
              productL3Response.getProductSku());


        }
      }
    }
  }

  @Override
  public Integer getMinimumPrice(String storeId) {
    ProductSystemParameter minimumPrice =
        productSystemParameterService.findByStoreIdAndVariable(storeId, Constants.MINIMUM_PRICE);
    if (Objects.nonNull(minimumPrice)) {
      return Integer.parseInt(minimumPrice.getValue());
    }
    return null;
  }

  @Override
  public boolean checkCategoryProductWholesaleRules(String storeId, String productCode, String categoryCode)
      throws Exception {
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    List<ProductBusinessPartner> productBusinessPartnerList =
        productBusinessPartnerRepository.findByStoreIdAndProductId(storeId, productCollection.getProductId());
    List<String> itemSkuList = new ArrayList<>();
    Map<String, String> itemSkuAndPickupPointMap = new HashMap<>();
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartnerList) {
      itemSkuList.addAll(
          ofNullable(productBusinessPartner.getProductItemBusinessPartners()).orElse(new ArrayList<>())
              .stream().map(ProductItemBusinessPartner::getGdnProductItemSku).collect(Collectors.toList()));
      for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
          .getProductItemBusinessPartners()) {
        itemSkuAndPickupPointMap.putIfAbsent(productItemBusinessPartner.getGdnProductItemSku(),
            productItemBusinessPartner.getPickupPointId());
      }
    }
    List<ProductItemCreationRequest> productItemCreationRequestList= new ArrayList<>();
    List<WholesalePriceSkuResponse> wholesalePriceSkuResponseList = new ArrayList<>();
    List<List<String>> itemSkuPartition = Lists.partition(itemSkuList, 50);
    for(List<String> itemSkuBatch : itemSkuPartition) {
      wholesalePriceSkuResponseList
          .addAll(this.productPricingOutbound.getWholesalePriceList(new HashSet<>(itemSkuBatch), itemSkuAndPickupPointMap));
    }
    if(productCollection.isPostLive()) {
      for(WholesalePriceSkuResponse wholesalePriceSkuResponse : wholesalePriceSkuResponseList) {
        ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
        productItemCreationRequest.setWholesalePriceActivated(ACTIVE_STATUS.equalsIgnoreCase(wholesalePriceSkuResponse.getSkuStatus()));
        productItemCreationRequest.setProductItemWholesalePriceRequests(
            getProductItemWholesalePriceRequestFromMap(wholesalePriceSkuResponse.getWholesaleRules()));
        productItemCreationRequestList.add(productItemCreationRequest);
      }
    } else {
      List<ProductItemWholesalePrice> productItemWholesalePriceList =
          this.productItemWholesalePriceService.findByStoreIdAndItemSkus(storeId, itemSkuList);
      for (ProductItemWholesalePrice productItemWholesalePrice : productItemWholesalePriceList) {
        ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
        productItemCreationRequest.setWholesalePriceActivated(productItemWholesalePrice.isWholesalePriceActivated());
        for (ProductItemWholesalePriceResponse productItemWholesalePriceResponse : mapperUtil
            .mapStringToResponse(productItemWholesalePrice.getWholesaleRules())) {
          productItemCreationRequest.setProductItemWholesalePriceRequests(new ArrayList<>());
          productItemCreationRequest.getProductItemWholesalePriceRequests().add(
              new ProductItemWholesalePriceRequest(productItemWholesalePriceResponse.getQuantity(),
                  productItemWholesalePriceResponse.getWholesaleDiscount()));
        }
        productItemCreationRequestList.add(productItemCreationRequest);
      }
    }
    ApiErrorCode apiErrorCode =
        wholesaleValidationUtil.validateWholesaleConfigOnFlow1(categoryCode, productItemCreationRequestList, true);
    if( Objects.nonNull(apiErrorCode)) {
      return false;
    } else {
      return true;
    }
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse updateItemsPriceStockImages(String storeId,
      List<ProductPriceStockAndImagesRequest> request, String businessPartnerCode,
      List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages, boolean productEditable,
      ProductL3Response productL3Response) throws Exception {
    LOGGER.info(
        "updateItemsPriceStockImages with request : {} businessPartnerCode : {} copyToAllVariantImages : {} productEditable : {}",
        request, businessPartnerCode, copyToAllVariantImages, productEditable);
    List<ProductPriceStockAndImagesRequest> successValidationVariantList;
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    Map<String, ItemSummaryResponse> savedProductDataMap = new HashMap<>();
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap = new HashMap<>();
    Map<String, Double> offerPriceMap = new HashMap<>();
    List<ItemSummaryResponse> itemSummaryResponseList = getItemSummaryResponseByItemSkus(
        request.stream().map(ProductPriceStockAndImagesRequest::getItemSku).collect(Collectors.toList()));
    Map<String, ItemSummaryResponse> itemSummaryResponseMap = itemSummaryResponseList.stream()
        .collect(Collectors.toMap(ItemSummaryResponse::getItemSku, Function.identity()));
    if (productEditable) {
      successValidationVariantList = variantEditValidationService
          .validateListOfVariantsWithSuccess(request, failedRequests, itemSummaryResponseMap);
    } else {
      successValidationVariantList = variantEditValidationService
          .validateListOfVariantsForMultiUsedProduct(request, failedRequests, itemSummaryResponseMap);
    }
    List<ProductLevel3Inventory> savedInventoryList = this.productLevel3InventoryService
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(businessPartnerCode,
            successValidationVariantList.stream().map(ProductPriceStockAndImagesRequest::getItemSku)
                .collect(Collectors.toList()));
    Map<String, ProductLevel3Inventory> savedInventoryListMap = savedInventoryList.stream()
        .collect(Collectors.toMap(ProductLevel3Inventory::getWebItemSku, Function.identity()));
    List<String> campaignValidationFailedSku = new ArrayList<>();
    for (ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      boolean priceValidated = false;
      ItemSummaryResponse savedProductData =
          itemSummaryResponseMap.get(productPriceStockAndImagesRequest.getItemSku());
      productPriceStockAndImagesRequest.setPickupPointCode(savedProductData.getPickupPointCode());
      if (isSalesPriceChangedOrPriceEditDisabled(savedProductData.getPrice(),
          productPriceStockAndImagesRequest.getPrices().get(0).getSalePrice(),
          savedProductData.isPriceEditDisabled())) {
        try {
          variantEditValidationService.validatePriceLockCampaignRequest(productPriceStockAndImagesRequest, failedRequests);
          priceValidated = true;
        } catch (ApplicationRuntimeException e) {
          LOGGER.error(
              "Error on validate of price lock feature for campaign, request : {}, error : {}, error - ",
              productPriceStockAndImagesRequest, e.getErrorMessage(), e);
          campaignValidationFailedSku.add(productPriceStockAndImagesRequest.getItemSku());
          continue;
        }
      }
      ProductItemBusinessPartner productItemBusinessPartner = this.productItemBusinessPartnerRepository
          .findByStoreIdAndGdnProductItemSkuAndMarkForDeleteFalse(storeId, productPriceStockAndImagesRequest.getItemSku());
      if (Objects.isNull(savedProductData) || Objects.isNull(productItemBusinessPartner)) {
        LOGGER.warn("Product details not found for itemSku : {}", productPriceStockAndImagesRequest.getItemSku());
        continue;
      }
      savedProductDataMap.put(productPriceStockAndImagesRequest.getItemSku(), savedProductData);
      UpdateItemSummaryRequest productLevel3UpdateSummaryRequest =
          generateProductLevel3UpdateSummaryRequestFromProductPriceStockAndImagesRequest(
              productPriceStockAndImagesRequest, savedProductData);
      offerPriceMap.put(productPriceStockAndImagesRequest.getItemSku(),
          productPriceStockAndImagesRequest.getPrices().get(0).getSalePrice());
      ItemSummaryResponse updatedProductData;

      //wholesale config from pricing validation
      WholesalePriceSkuResponse wholesalePriceSkuResponse = productPricingOutbound
          .getWholesalePrice(productPriceStockAndImagesRequest.getItemSku(), savedProductData.getPickupPointCode());
      wholesalePriceSkuResponseMap.put(productPriceStockAndImagesRequest.getItemSku(), wholesalePriceSkuResponse);
      if (Objects.nonNull(productPriceStockAndImagesRequest.getWholesalePriceActivated())
          && productPriceStockAndImagesRequest.getWholesalePriceActivated()) {
        Boolean isInSameThreshold = variantEditValidationService.isSameThreshold(productPriceStockAndImagesRequest,
            savedProductData.getMasterCatalog().getCategory().getCategoryCode(), savedProductData.getItemSku(),
            wholesalePriceSkuResponse, null);
        productLevel3UpdateSummaryRequest.setWholesalePriceActivated(isInSameThreshold);
      } else {
        productLevel3UpdateSummaryRequest
            .setWholesalePriceActivated(productPriceStockAndImagesRequest.getWholesalePriceActivated());
      }

      //stock update and save in history
      ProductLevel3Inventory savedInventory = savedInventoryListMap.get(productPriceStockAndImagesRequest.getItemSku());
      if (Objects.nonNull(productPriceStockAndImagesRequest.getDeltaStock()) || Objects
          .nonNull(productPriceStockAndImagesRequest.getMinimumStockLevel2())) {
        updateItemStock(businessPartnerCode, productPriceStockAndImagesRequest.getItemSku(),
            productPriceStockAndImagesRequest.getDeltaStock(),
            productPriceStockAndImagesRequest.getMinimumStockLevel2(),
            productPriceStockAndImagesRequest.getProductSku(), productPriceStockAndImagesRequest.getItemName(), savedInventory, failedRequests);
        productItemBusinessPartner.setStock(productPriceStockAndImagesRequest.getAvailableStockLevel2());
        productItemBusinessPartner.setMinimumStock(productPriceStockAndImagesRequest.getMinimumStockLevel2());
      }

      //sync stock update in-case of Warehouse merchant
      if (Objects.nonNull(productPriceStockAndImagesRequest.getSynchronizeStock()) && validateSynchronizeStock(
          businessPartnerCode) && !productPriceStockAndImagesRequest.getSynchronizeStock()
          .equals(savedInventory.isWebSyncStock())) {
        this.productLevel3InventoryService.updateSyncStockByBusinessPartnerCodeAndGdnSku(businessPartnerCode,
            productPriceStockAndImagesRequest.getItemSku(), productPriceStockAndImagesRequest.getSynchronizeStock(),
            Collections.singletonList(new ItemSkuPickupPointSyncStockDto(productPriceStockAndImagesRequest.getItemSku(),
                productPriceStockAndImagesRequest.getPickupPointCode(),
                productPriceStockAndImagesRequest.getSynchronizeStock())));
        this.updatedProductHistoryService
            .createProductL3AuditLog(businessPartnerCode, productPriceStockAndImagesRequest.getItemSku(),
                productPriceStockAndImagesRequest.getProductSku(), productPriceStockAndImagesRequest.getItemName(),
                UpdateProductActivity.SYNC_STOCK.getDesc(),
                String.valueOf(!productPriceStockAndImagesRequest.getSynchronizeStock()),
                String.valueOf(productPriceStockAndImagesRequest.getSynchronizeStock()), false, StringUtils.EMPTY);
      }

      //x-product save and save in history
      try {
        if (checkIfItemChanged(productLevel3UpdateSummaryRequest, savedProductData)) {
          updatedProductData =
              this.productLevel3Repository.updateSummary(businessPartnerCode, savedProductData.getItemSku(),
                  productLevel3UpdateSummaryRequest);
          updatedProductData.setWholesalePriceActivated(productLevel3UpdateSummaryRequest.getWholesalePriceActivated());
          generateProductBusinessPartnerFromProductPriceStockAndImagesRequest(productPriceStockAndImagesRequest,
              productItemBusinessPartner);
          createAuditLogs(businessPartnerCode, productPriceStockAndImagesRequest.getItemSku(), updatedProductData,
              savedProductData, productPriceStockAndImagesRequest.getItemName(), savedInventoryListMap.get(productPriceStockAndImagesRequest.getItemSku()));
        }
        if (priceValidated) {
          updateDiscountPriceInCampaign(productPriceStockAndImagesRequest.getItemSku(),
            productPriceStockAndImagesRequest.getPrices().get(0).getSalePrice(),
            productPriceStockAndImagesRequest.getCategoryCode(),
            productPriceStockAndImagesRequest.getPickupPointCode());
        }
      } catch (Exception e) {
        LOGGER.error("Exception caught on saving price info in x-product : itemSku : {} ",
            savedProductData.getItemSku(), e);
        throw e;
      }
      if (CollectionUtils.isNotEmpty(productLevel3UpdateSummaryRequest.getItemViewConfigs()) && isViewConfigChanged(
          productLevel3UpdateSummaryRequest, savedProductData)) {
        List<ItemViewConfigDTO> viewConfigs = new ArrayList<>(productLevel3UpdateSummaryRequest.getItemViewConfigs());
        String status =
            ConverterUtil.getProductStatus(viewConfigs.get(0).isBuyable(), viewConfigs.get(0).isDiscoverable());
        if (ProductLevel3Status.B2B.name().equals(status) || ProductLevel3Status.TEASER.name().equals(status)) {
          LOGGER.debug("Send notification if product status is changed");
          productNotificationService
              .sendNotificationForProductStatus(businessPartnerCode, savedProductData.getGeneratedItemName(), status);
        }
      }
      if(productEditable) {
        //L2 save
        this.productItemBusinessPartnerRepository.save(productItemBusinessPartner);
      }
    }
    successValidationVariantList = successValidationVariantList.stream().filter(
        productPriceStockAndImagesRequest -> !campaignValidationFailedSku
            .contains(productPriceStockAndImagesRequest.getItemSku())).collect(Collectors.toList());
    //update upc code in PCB and save history
    String productCode = StringUtils.EMPTY;
    boolean isUpcCodeUpdate = false;
    if (CollectionUtils.isNotEmpty(successValidationVariantList)) {
      productCode = successValidationVariantList.stream().findFirst().get().getProductCode();
      isUpcCodeUpdate = updateUpcCode(successValidationVariantList, businessPartnerCode, false, new ArrayList<>(),
          new SimpleStringResponse(), false, null, false, null).getKey();
    }
    Set<String> wholesaleFlagOffUpdate = new HashSet<>();
    //WS price update and save in history
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse =
        updateBulkWholesalePriceInPricing(savedProductDataMap, successValidationVariantList,
            wholesalePriceSkuResponseMap, businessPartnerCode, storeId, offerPriceMap);
    failedRequests =
        validateWholesaleResponseForFailedItemSku(wholesalePriceBulkUpdateResponse.getFailedItemSkuToFailedReasonMap(),
            failedRequests, savedProductDataMap, ApiErrorCode.WHOLESALE_PRICE_UPDATE_FAILED.getCode(),
            ApiErrorCode.WHOLESALE_PRICE_UPDATE_FAILED.getDesc());

    //WS flag update
    BulkActivateDeactivateResponse bulkActivateDeactivateResponse =
        updateBulkWholesaleFlagInPricing(savedProductDataMap, successValidationVariantList,
            wholesalePriceSkuResponseMap, wholesalePriceBulkUpdateResponse);
    if (MapUtils.isNotEmpty(bulkActivateDeactivateResponse.getFailedItemSkuToFailedReasonMap())) {
      LOGGER.debug("bulkActivateDeactivateResponse.getFailedItemSkuToFailedReasonMap() : {}",
          bulkActivateDeactivateResponse.getFailedItemSkuToFailedReasonMap());
      wholesaleFlagOffUpdate.addAll(bulkActivateDeactivateResponse.getFailedItemSkuToFailedReasonMap().keySet().stream()
          .collect(Collectors.toList()));
    }

    if (MapUtils.isNotEmpty(wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatusMap())) {
      LOGGER.debug("wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatusMap() : {}",
          wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatusMap());
      wholesaleFlagOffUpdate.addAll(wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatusMap().entrySet().stream()
          .filter(wsFlag -> !ACTIVE.equals(wsFlag.getValue()) && MapUtils.isNotEmpty(
              bulkActivateDeactivateResponse.getFailedItemSkuToFailedReasonMap())
              && bulkActivateDeactivateResponse.getFailedItemSkuToFailedReasonMap().containsKey(wsFlag.getKey()))
          .map(Map.Entry::getKey).collect(Collectors.toList()));
    }

    if (CollectionUtils.isNotEmpty(wholesaleFlagOffUpdate)) {
      LOGGER.debug("wholesaleFlagOffUpdate : {}", wholesaleFlagOffUpdate);
      xProductOutbound.updateWholeSaleActivationFlag(wholesaleFlagOffUpdate.stream().collect(Collectors.toList()), false);
    }

    failedRequests =
        validateWholesaleResponseForFailedItemSku(bulkActivateDeactivateResponse.getFailedItemSkuToFailedReasonMap(), failedRequests,
            savedProductDataMap, ApiErrorCode.WHOLESALE_FLAG_UPDATE_FAILED.getCode(),
            ApiErrorCode.WHOLESALE_FLAG_UPDATE_FAILED.getDesc());

    //image update in PCB and review check
    EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse = new EditedResizeAndImagesUpdateStatusResponse();
    Map<String, Boolean> itemImagesUpdateStatus = new HashMap<>();
    if (productEditable) {
      ProductCollection productCollection =
          productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
      editedResizeAndImagesUpdateStatusResponse =
          updateProductItemsImages(successValidationVariantList, copyToAllVariantImages, productCode,
              businessPartnerCode, false, CommonUtils.isProductActivatedBefore(productCollection),
            null);
    itemImagesUpdateStatus = editedResizeAndImagesUpdateStatusResponse.getItemImagesUpdateStatus();
      //Merchant config and category check for review type
      boolean postLive = productGoingForReviewIsPostLive(storeId, request.stream().findFirst().get().getProductSku(),
          businessPartnerCode, successValidationVariantList, itemImagesUpdateStatus, productL3Response, true)
          .isPostLiveConfig();
      if (!editedResizeAndImagesUpdateStatusResponse.getEditedImages().isEmpty()) {
        productPublisherService.publishEditImageResizeEvent(new EditedImageResizeEvent(productCode, DEFAULT_STORE_ID,
            editedResizeAndImagesUpdateStatusResponse.getEditedImages()));
      }
      updateProductScore(productCode, itemImagesUpdateStatus, isUpcCodeUpdate);
      return createItemsPriceStockImagesUpdateResponse(failedRequests, itemImagesUpdateStatus.get(NEW), postLive, false);
    }
    updateProductScore(productCode, itemImagesUpdateStatus, isUpcCodeUpdate);
    return createItemsPriceStockImagesUpdateResponse(failedRequests, false, false, false);
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse updateVariantsPriceStockImages(String storeId, String businessPartnerCode,
    List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages, boolean productEditable,
    ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse, ProductLevel3 productLevel3, boolean productDetailEdit)
    throws Exception {
    LOGGER.info(
        "updateItemsPriceStockImages with request : {} businessPartnerCode : {} copyToAllVariantImages : {} productEditable : {}",
        productVariantUpdateRequest, businessPartnerCode, copyToAllVariantImages, productEditable);
    List<ProductPriceStockAndImagesRequest> successValidationVariantList = new ArrayList<>();
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap = new HashMap<>();
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap = new HashMap<>();
    Map<String, Double> offerPriceMap = new HashMap<>();
    List<ItemPickupPointQuickEditRequest> modifiedItemPickupPoints = new ArrayList<>();
    List<ProductItemBusinessPartner> productItemBusinessPartnerList = new ArrayList<>();
    List<ItemPickupPointListingRequest> itemPickupPointListingRequestList = new ArrayList<>();
    List<UpdateDiscountDTO> campaignUpdateDiscountRequestList = new ArrayList<>();
    WholesaleMappingResponse wholesaleMappingResponse = null;
    Map<String, String> itemSkuAndPickupCodeMapping = new HashMap<>();
    Set<String> wholeSaleFlagUpdatedL5s = new HashSet<>();
    Map<String, Set<String>> l5XScheduleUpdateChangeTypeMap = new HashMap<>();
    Map<String, Boolean> addingWholeSale1stTimeL5s = new HashMap<>();
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    Map<String, String> itemSkuToItemCodeMap = new HashMap<>();
    Pair<Boolean, List<ProductItemUpcCodeUpdateRequest>> updateUpcCodeRequest = null;
    SimpleStringResponse validationErrorCode = new SimpleStringResponse(ErrorCategory.VALIDATION.getCode());
    EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse = null;
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest>
      successRequestForImageEdit = new ArrayList<>();
    List<LocationPathAndCommonImage> locationPathAndCommonImages = new ArrayList<>();
    List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests = new ArrayList<>();
    ProductCollection productCollection = new ProductCollection();
    EditProductItemAndImageResponse editProductItemAndImageResponse = new EditProductItemAndImageResponse();
    ItemPickupPointListingResponse parentProductL5Response = new ItemPickupPointListingResponse();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    boolean isOnlineFlagChanged = false;
    boolean isCNCFlagChangedAtL3Level = false;
    boolean isFbbFlagChangedAtL3Level = false;
    CombinedEditItemResponse combinedEditItemResponse = null;
    EditFlagChangesDTO editFlagChangesDTO = new EditFlagChangesDTO();

    EditItemResponse editItemResponse = null;

    try {
      ProfileResponse profileResponse = Optional.ofNullable(editResponse.getProfileResponse())
        .orElse(businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode));
      processSellerPenaltyChecks(productVariantUpdateRequest, profileResponse,
        sellerPenaltyEnabledPhase2);
      CommonUtils.setCncViewConfigFromProductVariantUpdateRequest(productVariantUpdateRequest, cncForWarehouseFeatureSwitch);
      ProductDetailEditDTO productDetailEditDTO =
        getProductDetailEditDTO(productEditable, productVariantUpdateRequest, editResponse,
          productLevel3, productDetailEdit && combinedEditFlowEnabled);
      CommonUtils.setB2cActivatedFromProfileResponse(productVariantUpdateRequest, profileResponse,
        overrideFlagsFromSellerSalesChannel);
      ProductCollection savedProductCollection =
        getProductCollection(storeId, productVariantUpdateRequest, editResponse, productDetailEditDTO);
      // Get L5 response map
      savedItemPickupPointDataMap =
          getL5CodeAndResponseMap(storeId, businessPartnerCode, productVariantUpdateRequest,
              itemPickupPointListingRequestList, profileResponse);
      Integer productType =
        getProductType(productLevel3, productDetailEditDTO, savedItemPickupPointDataMap);
      CommonUtils.populateL5WithActiveSchedules(l5sWithActiveSchedules,
        savedItemPickupPointDataMap);
      // since xProduct save is not yet done, populate the flags from L3 edit processing
      if(productEditable) {
        populateL3FlagsFromL3UpdateRequest(productDetailEditDTO, savedItemPickupPointDataMap);
      }
      itemsPriceStockImagesUpdateResponse =
          checkProductStatus(productVariantUpdateRequest, savedItemPickupPointDataMap);
      if (Objects.nonNull(itemsPriceStockImagesUpdateResponse)) {
        return itemsPriceStockImagesUpdateResponse;
      }
      // Add delete variants request modification
      updateRequestsForAddDeleteVariants(productVariantUpdateRequest, newlyAddedProductItemRequests, editResponse.getExtraDeletedItems());
      parentProductL5Response = savedItemPickupPointDataMap.entrySet().iterator().next().getValue();
//      if (!parentProductL5Response.isProductSyncStatus() && productVariantUpdateRequest.isProductEditable()) {
//        migrateAndSyncProduct(businessPartnerCode, productVariantUpdateRequest.getProductSku(),
//            parentProductL5Response.getProductCode());
//      }
      if (Objects.nonNull(productVariantUpdateRequest.getOnline())
          && parentProductL5Response.isOnline() != productVariantUpdateRequest.getOnline()) {
        isOnlineFlagChanged = true;
      }
        if (Objects.nonNull(productVariantUpdateRequest.getCnc())
            && parentProductL5Response.isCncActiveAtL3Level() != productVariantUpdateRequest.getCnc()) {
          isCNCFlagChangedAtL3Level = true;
        }
      if (Objects.nonNull(productVariantUpdateRequest.getFbbActiveAtL3Level())
          && parentProductL5Response.isFbbActiveAtL3Level() != productVariantUpdateRequest.getFbbActiveAtL3Level()) {
        isFbbFlagChangedAtL3Level = true;
      }
      CommonUtils.setB2bAndB2cFLags(productVariantUpdateRequest, parentProductL5Response, editFlagChangesDTO);
      productCollection = this.combinedEditFlowEnabled ? savedProductCollection :
        productCollectionRepository.findByStoreIdAndProductCode(storeId,
          parentProductL5Response.getProductCode());

      if (instoreNewFlowEnabled) {
        validateStatusAndCncStatusForInstoreProduct(productType, productVariantUpdateRequest,
            savedItemPickupPointDataMap.values().stream().findFirst()
                .map(ItemPickupPointListingResponse::getMissingFields).map(CollectionUtils::isNotEmpty).orElse(false));
      }

      for (Map.Entry<String, ItemPickupPointListingResponse> entrySet : savedItemPickupPointDataMap.entrySet()) {
        itemSkuToItemCodeMap.putIfAbsent(entrySet.getValue().getItemSku(), entrySet.getValue().getSkuCode());
      }

      // Add new item L5s also in offerPriceMap
      for (ItemPickupPointRequest pickupPointRequest : productVariantUpdateRequest.getAddPickupPoints()) {
        offerPriceMap.put(pickupPointRequest.getItemSku() + Constants.HYPHEN + pickupPointRequest.getPickupPointId(),
            pickupPointRequest.getSalePrice());
      }
      List<ItemInfoDto> itemInfoDtos = new ArrayList<>();
      // Validate request
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest
          .getProductItems()) {
        for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest
            .getModifiedItemPickupPoints()) {
          itemPickupPointRequest.setItemSku(productVariantPriceStockAndImagesRequest.getItemSku());
          if (Objects.nonNull(itemPickupPointRequest.getWholesalePriceActivated())) {
            ItemInfoDto itemInfoDto =
              ItemInfoDto.builder().itemSku(productVariantPriceStockAndImagesRequest.getItemSku())
                .pickupPointCode(itemPickupPointRequest.getPickupPointId()).itemPickupPointId(
                  productVariantPriceStockAndImagesRequest.getItemSku() + Constants.HYPHEN
                    + itemPickupPointRequest.getPickupPointId()).build();
            itemSkuAndPickupCodeMapping.putIfAbsent(productVariantPriceStockAndImagesRequest.getItemSku(),
                itemPickupPointRequest.getPickupPointId());
            itemInfoDtos.add(itemInfoDto);
          }
          // Set values same for all L5s wherever applicable
          itemPickupPointRequest
              .setSellerSku(productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints().get(0).getSellerSku());
          if (!inventoryL5SyncStockEnabled) {
            itemPickupPointRequest.setSynchronizeStock(
                productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints().get(0).getSynchronizeStock());
          }
          itemPickupPointRequest.setMinimumStock(
              productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints().get(0).getMinimumStock());
            ItemPickupPointListingResponse itemPickupPointListingResponse =
              getPickupPointListingResponse(savedItemPickupPointDataMap, itemPickupPointRequest,
                isMultiPickupPointEnabled(profileResponse));
            if(throwErrorOnEmptyL5Response) {
              validateL5Response(itemPickupPointRequest, itemPickupPointListingResponse);
            }
            // update l5XScheduleUpdateChangeTypeMap with L5 code and schedule change type if valid
            CommonUtils.getSchedulesUpdatedL5(l5XScheduleUpdateChangeTypeMap, itemPickupPointListingResponse,
              itemPickupPointRequest, schedulesAddEditEnabled, cncForWarehouseFeatureSwitch);
            itemPickupPointRequest.setFbbActive(itemPickupPointListingResponse.isFbbActive());
        }
      }
      String categoryCode = StringUtils.EMPTY;
      if(!savedItemPickupPointDataMap.isEmpty()) {
        categoryCode = savedItemPickupPointDataMap.entrySet().iterator().next().getValue().getCategoryCode();
      }
      getWholesaleResponseMap(wholesalePriceSkuResponseMap, itemInfoDtos, itemSkuAndPickupCodeMapping);
      LOGGER.info("Product-update : Response for wholesale obtained from pricing for productSku : {} ",
          productVariantUpdateRequest.getProductSku());
      wholesaleMappingResponse = getWholesaleMappingResponse(itemInfoDtos, categoryCode);
      validateRequest(productEditable, productVariantUpdateRequest, savedItemPickupPointDataMap,
          wholesalePriceSkuResponseMap, failedRequests, validationErrorCode,
        isMultiPickupPointEnabled(profileResponse),wholesaleMappingResponse, profileResponse, newlyAddedProductItemRequests);
      LOGGER.info("Product-update : Validation passed for productSku : {} ",
        productVariantUpdateRequest.getProductSku());
      boolean isUpcCodeUpdate = processUpcCodeUpdate(productVariantUpdateRequest, productCollection,
        parentProductL5Response, successValidationVariantList, validationErrorCode,
        businessPartnerCode, productDetailEditDTO, newlyAddedProductItemRequests);
      if(this.combinedEditFlowEnabled && productEditable) {
         editedResizeAndImagesUpdateStatusResponse =
          updateProductItemsImages(successValidationVariantList, copyToAllVariantImages,
            productCollection.getProductCode(), businessPartnerCode,
            productVariantUpdateRequest.isNeedCorrection(),
            CommonUtils.isProductActivatedBefore(productCollection), productDetailEditDTO);
        editProductItemAndImageResponse =
          productLevel3V2Service.performPCBUpdateForPDPEditRequest(productDetailEditDTO,
            productCollection);
        log.info("Product and Item Image response from PCB for product : {} was {} ",
          productCollection.getProductCode(), editProductItemAndImageResponse);
        handleImageResponseFromPCB(productDetailEditDTO, editProductItemAndImageResponse,
          successRequestForImageEdit, locationPathAndCommonImages);
        editResponse.setNewlySavedItemResponseList(editProductItemAndImageResponse.getNewlySavedItemResponses());
        historyUpdateForItemImages(businessPartnerCode, productVariantUpdateRequest,
          editProductItemAndImageResponse, editedResizeAndImagesUpdateStatusResponse);
      }
      AddDeleteVariantRequest addDeleteVariantRequest =
          prepareAddAndDeleteVariantRequest(productVariantUpdateRequest, editResponse, newlyAddedProductItemRequests,
              itemSkuToItemCodeMap, offerPriceMap, l5sWithActiveSchedules);
      log.info("Add delete variant request to x-product : {}  ", addDeleteVariantRequest);
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest
          .getProductItems()) {
        ItemPickupPointRequest parentItemPickupPointRequest = new ItemPickupPointRequest();
        if (CollectionUtils.isNotEmpty(productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints())) {
          parentItemPickupPointRequest = productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints().get(0);
        }
        for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest
            .getModifiedItemPickupPoints()) {
          // Override seller sku and min stock from 1st item
          itemPickupPointRequest.setSellerSku(parentItemPickupPointRequest.getSellerSku());
          itemPickupPointRequest.setItemSku(productVariantPriceStockAndImagesRequest.getItemSku());
          itemPickupPointRequest.setMinimumStock(parentItemPickupPointRequest.getMinimumStock());
          itemPickupPointRequest.setPpCodeChangedForNonMppSeller(
            itemPickupPointRequest.isPpCodeChangedForNonMppSeller());
          boolean multiPickupPointEnabledSeller = isMultiPickupPointEnabled(profileResponse);
          ItemPickupPointListingResponse itemPickupPointListingResponse =
            getPickupPointListingResponse(savedItemPickupPointDataMap, itemPickupPointRequest, multiPickupPointEnabledSeller);
          validateL5Response(itemPickupPointRequest, itemPickupPointListingResponse);
          try {
            validateCampaignLockPrice(campaignUpdateDiscountRequestList,
              productVariantPriceStockAndImagesRequest, itemPickupPointRequest,
              itemPickupPointListingResponse);
          } catch (Exception e) {
            log.error(
              "Campaign validation failed during L5 update for itemPickupPointCode : {} with "
                + "Error: ", itemPickupPointRequest.getItemSku() + Constants.HYPHEN
                + itemPickupPointRequest.getPickupPointId(), e);
            throw e;
          }
          log.info(
            "Product-update : Campaign lock price validated for itemPickupPointRequest : {} ",
            itemPickupPointRequest);
          ProductItemBusinessPartner productItemBusinessPartner = this.productItemBusinessPartnerRepository
              .findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(storeId,
                  productVariantPriceStockAndImagesRequest.getItemSku(), itemPickupPointRequest.getPickupPointId());
          if (Objects.isNull(productItemBusinessPartner)) {
            LOGGER.warn("Product details not found for itemSku : {} pickupPointCode : {}",
                productVariantPriceStockAndImagesRequest.getItemSku(), itemPickupPointRequest.getPickupPointId());
          }

          ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();

          offerPriceMap.put(
              productVariantPriceStockAndImagesRequest.getItemSku() + Constants.HYPHEN + itemPickupPointRequest
                  .getPickupPointId(), itemPickupPointRequest.getSalePrice());

          //wholesale config from pricing validation
          validateWholeSaleAndSetWholeSaleActivatedFlag(wholesalePriceSkuResponseMap,
              productVariantPriceStockAndImagesRequest, itemPickupPointRequest, itemPickupPointListingResponse,
              itemPickupPointQuickEditRequest, wholesaleMappingResponse);
          LOGGER.info("Product-update : wholesale price validation for productSku : {} ",
              productVariantUpdateRequest.getProductSku());
          String l5Code = getL5Code(itemPickupPointRequest);
          Set<String> scheduleUpdateChangeType =
            l5XScheduleUpdateChangeTypeMap.getOrDefault(l5Code, Collections.emptySet());
          //x-product save and save in history
          if (checkIfItemChangedL5(itemPickupPointRequest, itemPickupPointListingResponse)
            || CollectionUtils.isNotEmpty(scheduleUpdateChangeType)) {
            prepareRequestForUpdateL5(modifiedItemPickupPoints, itemPickupPointRequest,
              itemPickupPointQuickEditRequest, l5sWithActiveSchedules, editResponse);
            if (Objects.nonNull(productItemBusinessPartner)) {
              generateProductBusinessPartnerFromItemPickupPointRequest(itemPickupPointRequest,
                productItemBusinessPartner);
            }
          }

          if (productEditable && Objects.nonNull(productItemBusinessPartner)) {
            productItemBusinessPartnerList.add(productItemBusinessPartner);
          }
        }
      }
      updateAddDeleteVariantStatusInProductCollection(editResponse, productVariantUpdateRequest.getProductCode(),
        AddDeleteVariantStatus.PENDING);
      //update upc code in PCB and save history
      String productCode = StringUtils.EMPTY;
      failedRequests = updateWholeSalePriceAndFlag(businessPartnerCode, productVariantUpdateRequest, failedRequests,
          savedItemPickupPointDataMap, wholesalePriceSkuResponseMap, offerPriceMap, itemSkuAndPickupCodeMapping,
          wholeSaleFlagUpdatedL5s, modifiedItemPickupPoints, itemSkuToItemCodeMap,
        addingWholeSale1stTimeL5s, profileResponse, newlyAddedProductItemRequests);
      LOGGER.info("Product-update : wholesale price and flag updated for productSku : {}",
          productVariantUpdateRequest.getProductSku());

      List<ItemPickupPointQuickEditRequest> addItemPickupPoints = new ArrayList<>();
      // Get add pickup point request
      getAddPickupPointsRequest(productVariantUpdateRequest, addItemPickupPoints);

      List<ItemPickupPointDeleteRequest> itemPickupPointDeleteRequests = new ArrayList<>();
      // Get delete pickup point delete request
      itemPickupPointDeleteRequests =
        ConverterUtil.getDeletePickupPointDeleteRequest(productVariantUpdateRequest);
      List<ProductItemBusinessPartner> productItemBusinessPartners = productBusinessPartnerService
          .generateNewProductItemBusinessPartnerData(newlyAddedProductItemRequests, productType);
      productItemBusinessPartnerList.addAll(productItemBusinessPartners);
      if (CollectionUtils.isNotEmpty(productItemBusinessPartnerList)) {
        log.info("Saving product item business partner list : {} for productCode : {} ", productItemBusinessPartnerList,
            productCollection.getProductCode());
        CommonUtils.updateProductItemBusinessPartnerForAutoCategoryChange(productItemBusinessPartnerList,
          productDetailEditDTO.isAutoCategoryChange(), productDetailEditDTO.isBopisEligible(),
          bopisCategoryActionOnCategoryChangeSwitch, productType, editFlagChangesDTO);
        productItemBusinessPartnerService.saveAll(productItemBusinessPartnerList);
      }
      log.info("Sending update request to x-product : {} for merchant : {} productSku : {}  ",
        modifiedItemPickupPoints, businessPartnerCode, productVariantUpdateRequest.getProductSku());
      // Call to X-product
      if (this.combinedEditFlowEnabled && productEditable) {
        formItemPickupPointRequestForCombinedEdit(storeId, productVariantUpdateRequest,
          productDetailEditDTO, parentProductL5Response, modifiedItemPickupPoints,
          addItemPickupPoints, itemPickupPointDeleteRequests, isOnlineFlagChanged,
          isCNCFlagChangedAtL3Level, isFbbFlagChangedAtL3Level, addDeleteVariantRequest,
          editFlagChangesDTO, productType);
        combinedEditItemResponse =
          productLevel3V2Service.updateAllCollectionsDownstreamAndProcessHistoryForPDPEdit(
          productDetailEditDTO, editResponse).getKey();

        if(CollectionUtils.isNotEmpty(productDetailEditDTO.getReviewTypeList())) {
          productCollection.setReviewType(
            String.join(",", productDetailEditDTO.getReviewTypeList()));
        }
        Optional.ofNullable(productCollection).ifPresent(pbp -> {
          if (StringUtils.isNotEmpty(productVariantUpdateRequest.getProductName())) {
            pbp.setProductName(productVariantUpdateRequest.getProductName());
          }
        });
      }else {
        editItemResponse =
          this.xProductOutbound.updateItemPickupPoints(storeId, parentProductL5Response.getProductSku(),
            parentProductL5Response.getProductType(), modifiedItemPickupPoints, addItemPickupPoints,
            itemPickupPointDeleteRequests, productVariantUpdateRequest.getOnline(), isOnlineFlagChanged,
            productVariantUpdateRequest.getCnc(), isCNCFlagChangedAtL3Level,
            productVariantUpdateRequest.getFbbActiveAtL3Level(), isFbbFlagChangedAtL3Level,
            addDeleteVariantRequest, productVariantUpdateRequest, editFlagChangesDTO);
      }
      LOGGER.info("Product-update : data updated in x-product for productSku : {} ",
          productVariantUpdateRequest.getProductSku());
      try {
        updateDiscountPriceInCampaign(campaignUpdateDiscountRequestList);
      } catch (Exception e) {
        log.error("Update Failed For productSku : {} with Error : ",
          productVariantUpdateRequest.getProductSku(), e);
        throw e;
      }
      boolean isMultiPickupPointEnabledSeller = isMultiPickupPointEnabled(profileResponse);
      for (ProductVariantPriceStockAndImagesRequest productItems : productVariantUpdateRequest.getProductItems()) {
        updatePickupPointInInventoryForNonMppSellers(productVariantUpdateRequest, productItems,
          savedItemPickupPointDataMap, isMultiPickupPointEnabledSeller);
      }
      editResponse.setScheduleUpdatedL5s(l5XScheduleUpdateChangeTypeMap);
      // History
      createAuditLogs(businessPartnerCode, productVariantUpdateRequest, savedItemPickupPointDataMap, editItemResponse,
          wholeSaleFlagUpdatedL5s, isOnlineFlagChanged, addingWholeSale1stTimeL5s, profileResponse,
          parentProductL5Response, editResponse, newlyAddedProductItemRequests,
          this.combinedEditFlowEnabled && productEditable, combinedEditItemResponse);
      LOGGER.info("Product-update : logs created for productSku : {} , {} ",
          productVariantUpdateRequest.getProductSku(), isUpcCodeUpdate);

      sendNotificationForStatusChange(businessPartnerCode, productVariantUpdateRequest,
        savedItemPickupPointDataMap, isMultiPickupPointEnabledSeller);

      // x-inventory data update
      inventoryDataUpdate(businessPartnerCode, productVariantUpdateRequest, savedItemPickupPointDataMap,
          profileResponse, itemSkuToItemCodeMap, newlyAddedProductItemRequests);
      LOGGER.info("Product-update : data updated in x-inv for productSku : {} ",
          productVariantUpdateRequest.getProductSku());

      //image update in PCB and review check
      Map<String, Boolean> itemImagesUpdateStatus = new HashMap<>();
      boolean newImagesAdded = false;
      // Adding images for newly added L4 also to the request
      if (CollectionUtils.isNotEmpty(newlyAddedProductItemRequests)) {
        for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : newlyAddedProductItemRequests) {
          getSuccessValidationList(parentProductL5Response, successValidationVariantList,
              productVariantPriceStockAndImagesRequest);
        }
      }
      productCode = successValidationVariantList.stream().findFirst().get().getProductCode();
      updateAddDeleteVariantStatusInProductCollection(editResponse, productCode,
        AddDeleteVariantStatus.SUCCESS);
      if (productEditable) {
        if(Objects.isNull(editedResizeAndImagesUpdateStatusResponse)) {
          editedResizeAndImagesUpdateStatusResponse =
            updateProductItemsImages(successValidationVariantList, copyToAllVariantImages,
              productCode, businessPartnerCode, productVariantUpdateRequest.isNeedCorrection(),
              CommonUtils.isProductActivatedBefore(productCollection), productDetailEditDTO);
        }
        itemImagesUpdateStatus = editedResizeAndImagesUpdateStatusResponse.getItemImagesUpdateStatus();
        if (this.combinedEditFlowEnabled) {
          // create audit logs For common and Item image update
          createAuditLogs(businessPartnerCode, successRequestForImageEdit,
            productVariantUpdateRequest.getProductSku(), false,
            MapUtils.isNotEmpty(editedResizeAndImagesUpdateStatusResponse.getItemImagesUpdateStatus()));
          if (editedResizeAndImagesUpdateStatusResponse.getItemImagesUpdateStatus()
            .getOrDefault(NEW, false) || editedResizeAndImagesUpdateStatusResponse.getItemImagesUpdateStatus()
            .getOrDefault(UPDATE, false)) {
            EditedResizeImagesResponse editedResizeImagesResponse =  publishEditImagesForResizing(editedResizeAndImagesUpdateStatusResponse.getProductLevel3SummaryDetailsImageRequests(),
              locationPathAndCommonImages, productCode);
            editedResizeAndImagesUpdateStatusResponse.setEditedImages(editedResizeImagesResponse.getEditedImages());
          }
        }
        //Merchant config and category check for review type
        PostLiveFlagsDTO postLiveFlagsDTO = getProductLevel3Service()
            .productAfterUpdateGoingForReviewIsPostLive(storeId, parentProductL5Response.getProductSku(), businessPartnerCode,
                successValidationVariantList, itemImagesUpdateStatus, new ProductL3Response(), false);
        // Publish edited image resize event
        EditedResizeAndImagesUpdateStatusResponse imageEditResponse =
            publishEditedResizeEventIfApplicable(productVariantUpdateRequest, editResponse,
                editedResizeAndImagesUpdateStatusResponse, productCode);
        LOGGER.info("Product-update : image data updated for productSku : {} ",
            productVariantUpdateRequest.getProductSku());
        boolean generateScoreONUpcCodeOrUrlUpdate =
          CommonUtils.isProductScoreGenerationNeeded(isUpcCodeUpdate,
            editResponse.isYouTubeUrlUpdated(), productDetailEditDTO.getVideoUpdated());
        L3VersionResponse l3VersionResponse =
          getL3VersionResponse(productCode, generateScoreONUpcCodeOrUrlUpdate,
            itemImagesUpdateStatus, combinedEditItemResponse);
        itemsPriceStockImagesUpdateResponse =
            createItemsPriceStockImagesUpdateResponse(failedRequests,
              itemImagesUpdateStatus.getOrDefault(NEW, false ), postLiveFlagsDTO.isPostLiveConfig(),
                getTakeDownStatus(postLiveFlagsDTO.isPostLiveConfig(), itemImagesUpdateStatus, editedResizeAndImagesUpdateStatusResponse,
                    productCollection));
        itemsPriceStockImagesUpdateResponse.setNewImagesAdded(newImagesAdded);
        itemsPriceStockImagesUpdateResponse.setEditedResizeAndImagesUpdateStatusResponse(imageEditResponse);
        itemsPriceStockImagesUpdateResponse.setL3Version(this.combinedEditFlowEnabled ?
          Optional.ofNullable(combinedEditItemResponse).map(CombinedEditItemResponse::getL3Version).orElse(null) : editItemResponse.getL3Version());
        if (Objects.isNull(postLiveFlagsDTO.getPostLiveConfigOfProduct())) {
          postLiveFlagsDTO.setPostLiveConfigOfProduct(productCollection.isPostLive());
        }
        itemsPriceStockImagesUpdateResponse.setScheduleRemovedForStatusUpdate(editResponse.getScheduleRemovedForStatusUpdate());
        itemsPriceStockImagesUpdateResponse.setReviewType(postLiveFlagsDTO.getPostLiveConfigOfProduct() ?
            Constants.POST_LIVE_REVIEW_TYPE : Constants.PRE_LIVE_REVIEW_TYPE);
        if (Optional.ofNullable(l3VersionResponse).map(L3VersionResponse::getL3Version).isPresent()) {
          itemsPriceStockImagesUpdateResponse.setL3Version(l3VersionResponse.getL3Version());
        }
        return itemsPriceStockImagesUpdateResponse;
      }
      updateLogisticsForNewlyAddedItems(editResponse, productCollection, addDeleteVariantRequest);
      L3VersionResponse l3VersionResponse = updateProductScore(productCode, itemImagesUpdateStatus, isUpcCodeUpdate);
      itemsPriceStockImagesUpdateResponse =
          createItemsPriceStockImagesUpdateResponse(failedRequests, false, false, false);
      itemsPriceStockImagesUpdateResponse.setL3Version(l3VersionResponse.getL3Version());
      itemsPriceStockImagesUpdateResponse.setScheduleRemovedForStatusUpdate(editResponse.getScheduleRemovedForStatusUpdate());
      return createItemsPriceStockImagesUpdateResponse(failedRequests, false, false, false);
    } catch (ApplicationRuntimeException e) {
      return getResponseInCaseApplicationRuntimeException(productVariantUpdateRequest, e, validationErrorCode);
    } catch (Exception e) {
      LOGGER.error("Exception when updating itemPickupPoint with request : {} , error - ", productVariantUpdateRequest,
          e);
      return getItemsPriceStockImagesUpdateResponse(productVariantUpdateRequest, ErrorCategory.UNSPECIFIED.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode());
    }
  }

  private void validateStatusAndCncStatusForInstoreProduct(Integer productType,
      ProductVariantUpdateRequest productVariantUpdateRequest, boolean hasMissingFields) {
    boolean isL3NotOffline = Boolean.TRUE.equals(productVariantUpdateRequest.getOnline()) || Boolean.TRUE.equals(
        productVariantUpdateRequest.getCnc());
    boolean isL5NotOffline = productVariantUpdateRequest.getProductItems().stream().anyMatch(
        item -> item.getModifiedItemPickupPoints().stream().anyMatch(
            l5Item -> l5Item.isBuyable() || l5Item.isDisplay() || l5Item.isCncBuyable() || l5Item.isCncDisplay()));
    if (hasMissingFields && (isL3NotOffline || isL5NotOffline)) {
      log.error("Error updating status instore product : {} ",
          productVariantUpdateRequest.getProductItems().stream().findFirst().get().getItemSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.INSTORE_STATUS_CHANGE_ERROR.getDesc());
    }
  }

  private static void validateL5Response(ItemPickupPointRequest itemPickupPointRequest,
      ItemPickupPointListingResponse itemPickupPointListingResponse) {
    if (Objects.isNull(itemPickupPointListingResponse)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.ITEM_PICKUP_POINT_NOT_FOUND.getDesc() + itemPickupPointRequest.getItemSku() + Constants.HYPHEN
              + itemPickupPointRequest.getPickupPointId() + ErrorMessages.PRODUCT_SKU);
    }
  }

  private void updateAddDeleteVariantStatusInProductCollection(EditProductResponse editResponse,
    String productCode, AddDeleteVariantStatus addDeleteVariantStatus) {
    if (CollectionUtils.isNotEmpty(editResponse.getNewlySavedItemResponseList().stream()
      .map(NewlySavedItemResponse::getItemCode).collect(Collectors.toList()))) {
      productService.updateAddDeleteVariantStatus(productCode,
        addDeleteVariantStatus);
    }
  }

  private static String getL5Code(ItemPickupPointRequest itemPickupPointRequest) {
    String l5Code = itemPickupPointRequest.getItemSku() + Constants.HYPHEN
      + itemPickupPointRequest.getPickupPointId();
    return l5Code;
  }

  private ProductDetailEditDTO getProductDetailEditDTO(boolean productEditable,
    ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse,
    ProductLevel3 productLevel3, boolean productDetailEdit) throws Exception {
    ProductDetailEditDTO productDetailEditDTO =
      Optional.ofNullable(editResponse.getProductDetailEditDTO()).orElse(new ProductDetailEditDTO());
    productDetailEditDTO.setProductSku(productVariantUpdateRequest.getProductSku());
    productLevel3.setProductSku(productVariantUpdateRequest.getProductSku());
    productLevel3.setProductEditable(productEditable);
    productDetailEditDTO.setProductLevel3(productLevel3);
    productDetailEditDTO.setProductEditable(productEditable);
    if(Objects.isNull(productDetailEditDTO.getProductLevel3DetailResponse())) {
      productDetailEditDTO.setProductLevel3DetailResponse(
          this.getL3DetailByProductSku(productVariantUpdateRequest.getProductSku(), false, true, new ArrayList<>(), false));
    }
    return productDetailEditDTO;
  }

  private static Integer getProductType(ProductLevel3 productLevel3,
    ProductDetailEditDTO productDetailEditDTO,
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap) {
    Integer productType =
      Optional.ofNullable(productDetailEditDTO).map(ProductDetailEditDTO::getProductLevel3)
        .map(ProductLevel3::getProductType).orElse(productLevel3.getProductType());
    if (Objects.isNull(productType)) {
      productType = savedItemPickupPointDataMap.values().stream().findFirst()
        .map(ItemPickupPointListingResponse::getProductType).map(ProductType::getCode).orElse(null);
    }
    return productType;
  }

  public ProductCollection getProductCollection(String storeId,
    ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse,
    ProductDetailEditDTO productDetailEditDTO) {
    ProductCollection savedProductCollection =
      Optional.ofNullable(editResponse.getProductDetailEditDTO()).map(ProductDetailEditDTO::getProductCollectionDTO)
        .map(ConverterUtil::convertDtoToProductCollection).orElseGet(() -> {
          String productCode =
            productVariantUpdateRequest.getProductItems().stream().filter(Objects::nonNull)
              .map(ProductVariantPriceStockAndImagesRequest::getProductCode).filter(Objects::nonNull).findFirst().orElse(productVariantUpdateRequest.getProductCode());
          return productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
        });
    if(Objects.isNull(productDetailEditDTO.getProductCollectionDTO())){
      productDetailEditDTO.setProductCollectionDTO(ConverterUtil.convertProductCollectionToDto(savedProductCollection));
    }
    return savedProductCollection;
  }


  public void historyUpdateForItemImages(String businessPartnerCode,
    ProductVariantUpdateRequest productVariantUpdateRequest,
    EditProductItemAndImageResponse editProductItemAndImageResponse,
    EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse)
    throws Exception {
    if (MapUtils.isNotEmpty(editProductItemAndImageResponse.getImagePathErrorMap())
      && Optional.ofNullable(editedResizeAndImagesUpdateStatusResponse)
      .map(EditedResizeAndImagesUpdateStatusResponse::getProductItemImageHistoryDTO).isPresent()) {
      createAuditLogs(businessPartnerCode,
        editedResizeAndImagesUpdateStatusResponse.getProductItemImageHistoryDTO()
          .getProductItemImageUpdateRequest(), productVariantUpdateRequest.getProductSku(),
        editedResizeAndImagesUpdateStatusResponse.getProductItemImageHistoryDTO()
          .getItemSkuItemNameMap(),
        editedResizeAndImagesUpdateStatusResponse.getProductItemImageHistoryDTO()
          .getSkuCodeItemSkuMap(), false);
    }
  }

  private CombinedEditItemResponse getCombinedEditItemResponse(EditProductResponse editResponse,
    ProductDetailEditDTO productDetailEditDTO,
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> successRequestForImageEdit,
    List<LocationPathAndCommonImage> locationPathAndCommonImages) throws Exception {
    Pair<CombinedEditItemResponse, EditProductItemAndImageResponse>
      editProductItemAndImageResponsePair;
    CombinedEditItemResponse combinedEditItemResponse;
    editProductItemAndImageResponsePair =
    productLevel3V2Service.updateAllCollectionsDownstreamAndProcessHistoryForPDPEdit(
      productDetailEditDTO, editResponse);
    return editProductItemAndImageResponsePair.getKey();
  }

  private void formItemPickupPointRequestForCombinedEdit(String storeId, ProductVariantUpdateRequest productVariantUpdateRequest,
    ProductDetailEditDTO productDetailEditDTO, ItemPickupPointListingResponse parentProductL5Response,
    List<ItemPickupPointQuickEditRequest> modifiedItemPickupPoints, List<ItemPickupPointQuickEditRequest> addItemPickupPoints,
    List<ItemPickupPointDeleteRequest> itemPickupPointDeleteRequests, boolean isOnlineFlagChanged, boolean isCNCFlagChangedAtL3Level,
    boolean isFbbFlagChangedAtL3Level, AddDeleteVariantRequest addDeleteVariantRequest, EditFlagChangesDTO editFlagChangesDTO,
    Integer productType) throws Exception {
    this.xProductOutbound.updateItemPickupPointsForCombinedEdit(storeId,
      parentProductL5Response.getProductSku(), getProductTypeFromCode(productType),
      modifiedItemPickupPoints, addItemPickupPoints, itemPickupPointDeleteRequests,
      productVariantUpdateRequest.getOnline(), isOnlineFlagChanged,
      productVariantUpdateRequest.getCnc(), isCNCFlagChangedAtL3Level,
      productVariantUpdateRequest.getFbbActiveAtL3Level(), isFbbFlagChangedAtL3Level,
      addDeleteVariantRequest, productVariantUpdateRequest, editFlagChangesDTO,
      productDetailEditDTO);
  }

  private ProductType getProductTypeFromCode(int code) {
    if (ProductType.REGULAR.getCode() == code) {
      return ProductType.REGULAR;
    } else if (ProductType.BIG_PRODUCT.getCode() == code) {
      return ProductType.BIG_PRODUCT;
    } else {
      return ProductType.BOPIS;
    }
  }

  public boolean processUpcCodeUpdate(ProductVariantUpdateRequest productVariantUpdateRequest,
    ProductCollection productCollection, ItemPickupPointListingResponse parentProductL5Response,
    List<ProductPriceStockAndImagesRequest> successValidationVariantList,
    SimpleStringResponse validationErrorCode, String businessPartnerCode,
    ProductDetailEditDTO productDetailEditDTO,
    List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests) throws Exception {
    boolean isUpcCodeUpdate = false;
    for (ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest : productVariantUpdateRequest.getProductItems()) {
      getSuccessValidationList(parentProductL5Response, successValidationVariantList,
        productPriceStockAndImagesRequest);
    }
    // Add Newly Added variants for Processing its images and upc code updates
    if(combinedEditFlowEnabled) {
      newlyAddedProductItemRequests.stream().forEach(
        productVariantPriceStockAndImagesRequest -> getSuccessValidationList(parentProductL5Response,
          successValidationVariantList, productVariantPriceStockAndImagesRequest));
    }

    boolean isUpcCodeNotNullForAllItems = productVariantUpdateRequest.getProductItems().stream()
      .anyMatch(request -> Objects.nonNull(request.getUpcCode()));

    if (isUpcCodeNotNullForAllItems) {
      List<String> skuCodeList = productVariantUpdateRequest.getProductItems().stream()
        .filter(request -> Objects.nonNull(request.getUpcCode()))
        .map(ProductVariantPriceStockAndImagesRequest::getSkuCode).collect(Collectors.toList());

      Pair<Boolean, List<ProductItemUpcCodeUpdateRequest>> updateUpcCodeRequest =
        updateUpcCode(successValidationVariantList, businessPartnerCode, false, skuCodeList,
          validationErrorCode, newUpcCodeValidateFlow, productVariantUpdateRequest,
          combinedEditFlowEnabled, productDetailEditDTO);

      isUpcCodeUpdate = Optional.of(updateUpcCodeRequest).map(Pair::getKey).orElse(false);

      LOGGER.info("Product-update : upcCode updated for productSku : {} updated : {}",
        productVariantUpdateRequest.getProductSku(), isUpcCodeUpdate);

      if (isUpcCodeUpdate && productCollection.isReviewPending()) {
        if (!combinedEditFlowEnabled) {
          publishEditedProductEventToPDT(DEFAULT_STORE_ID,
            CONTENT_REFRESH, productCollection, null);
          productCollection.setEdited(true);
          productCollection = productService.saveProductCollectionForUPC(productCollection);
        } else {
          productCollection.setEdited(true);
        }
      }
      if(combinedEditFlowEnabled){
        productDetailEditDTO.setProductItemUpcCodeUpdateRequests(
          Optional.of(updateUpcCodeRequest).map(Pair::getValue).orElse(Collections.emptyList()));
      }
    }
    return isUpcCodeUpdate;
  }

  private static void handleImageResponseFromPCB(ProductDetailEditDTO productDetailEditDTO,
    EditProductItemAndImageResponse editProductItemAndImageResponse,
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> successRequestForImageEdit,
    List<LocationPathAndCommonImage> locationPathAndCommonImages) {
    Map<String, Map<String, String>> imagePathErrorMap =
      Optional.ofNullable(editProductItemAndImageResponse)
        .map(EditProductItemAndImageResponse::getImagePathErrorMap)
        .orElse(new HashMap<>());
    if (CollectionUtils.isNotEmpty(productDetailEditDTO.getProductImageEditRequests())) {
      LOGGER.info("Updated common images for request : {} response : {} ",
        productDetailEditDTO.getProductImageEditRequests(),
        imagePathErrorMap);
      for (com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest pcbRequest : productDetailEditDTO.getProductImageEditRequests()) {
        String imageResponse =
          imagePathErrorMap.get(pcbRequest.getImagePath()).get(Constants.COPY_ALL_STATUS);
        if (Constants.SUCCESS.equals(imageResponse) || Constants.PARTIAL_SUCCESS.equals(imageResponse)) {
          if (Objects.nonNull(pcbRequest.getCopyToAllVariantImages())) {
            successRequestForImageEdit.add(pcbRequest);
          }
          if (Optional.ofNullable(pcbRequest.getCopyToAllVariantImages())
              .map(com.gdn.x.productcategorybase.dto.request.CopyImageEditRequest::isAdd).isPresent()) {
            LocationPathAndCommonImage locationPathAndCommonImage =
                new LocationPathAndCommonImage(pcbRequest.getImagePath(), true);
            locationPathAndCommonImages.add(locationPathAndCommonImage);
          }
        } else {
          log.error("Error while updating common image : {} ", pcbRequest);
        }
      }
    }
  }

  public L3VersionResponse getL3VersionResponse(String productCode, boolean isUpcCodeUpdate,
      Map<String, Boolean> itemImagesUpdateStatus, CombinedEditItemResponse combinedEditItemResponse) {
    L3VersionResponse l3VersionResponse = new L3VersionResponse();
      l3VersionResponse = updateProductScore(productCode, itemImagesUpdateStatus, isUpcCodeUpdate);
    return l3VersionResponse;
  }

  private void updatePickupPointInInventoryForNonMppSellers(ProductVariantUpdateRequest productVariantUpdateRequest,
    ProductVariantPriceStockAndImagesRequest productItems,
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap,
    boolean isMultiPickupPointEnabledSeller) throws Exception {
    ItemPickupPointRequest itemPickupPointRequestForNonMpp =
      productItems.getModifiedItemPickupPoints().stream().filter(
        itemPickupPointRequest -> Boolean.TRUE.equals(
          itemPickupPointRequest.isPpCodeChangedForNonMppSeller())).findAny().orElse(null);
    if (Objects.nonNull(itemPickupPointRequestForNonMpp)) {
      ItemPickupPointListingResponse itemPickupPointListingResponse =
        getPickupPointListingResponse(savedItemPickupPointDataMap,
          itemPickupPointRequestForNonMpp, isMultiPickupPointEnabledSeller);
      WebInventoryUpdatePickupPointResponseDTO updatePickupPointResponse =
        inventoryOutbound.updatePickupPoint(generateMandatoryRequestParam(),
          new ListRequestDTO<>(getWebInventoryUpdatePickupPointRequestForNonMppSeller(
            itemPickupPointRequestForNonMpp.getPickupPointId(),
            itemPickupPointListingResponse)));
      LOGGER.info("Product-update : data updated in x-inv for Item : {} with PickupPoint : {} ",
        productVariantUpdateRequest.getProductSku(),
        itemPickupPointRequestForNonMpp.getPickupPointId());
    }
  }

  private boolean isFreeSampleFromRequest(ProductDetailEditDTO productDetailEditDTO) {
    return Optional.ofNullable(productDetailEditDTO)
      .map(ProductDetailEditDTO::getProductDetailEditRequestForXProduct)
      .map(ProductDetailPageEditRequest::getProductEditRequest)
      .map(ProductEditRequest::getProductRequest)
      .map(ProductRequest::isFreeSample)
      .orElse(false);
  }

  private Boolean getLateFulfillment(ProductDetailEditDTO productDetailEditDTO) {
    return Optional.ofNullable(productDetailEditDTO)
      .map(ProductDetailEditDTO::getProductDetailEditRequestForXProduct)
      .map(ProductDetailPageEditRequest::getProductEditRequest)
      .map(ProductEditRequest::getLatefulfillment)
      .orElse(null);
  }

  private Boolean isOff2OnActiveFlag(ProductDetailEditDTO productDetailEditDTO) {
    return Optional.ofNullable(productDetailEditDTO)
      .map(ProductDetailEditDTO::getProductDetailEditRequestForXProduct)
      .map(ProductDetailPageEditRequest::getProductEditRequest)
      .map(ProductEditRequest::getProductRequest)
      .map(ProductRequest::isOff2OnChannelActive)
      .orElse(false);
  }

  private Boolean isForceReview(ProductDetailEditDTO productDetailEditDTO) {
    return Optional.ofNullable(productDetailEditDTO)
      .map(ProductDetailEditDTO::getProductDetailEditRequestForXProduct)
      .map(ProductDetailPageEditRequest::getProductEditRequest)
      .map(ProductEditRequest::isForceReview)
      .orElse(false);
  }

  private ProductType getProductType(ProductDetailEditDTO productDetailEditDTO) {
    return Optional.ofNullable(productDetailEditDTO)
      .map(ProductDetailEditDTO::getProductDetailEditRequestForXProduct)
      .map(ProductDetailPageEditRequest::getProductEditRequest)
      .map(ProductEditRequest::getProductRequest)
      .map(ProductRequest::getProductType)
      .orElse(null);
  }

  private void handleForceReview(ItemPickupPointListingResponse response) {
    response.setArchiveFlagAtL3Level(true);
    response.setArchived(true);
    response.getViewConfigs().stream().findFirst().ifPresent(viewConfigResponse -> {
      viewConfigResponse.setDisplay(false);
      viewConfigResponse.setBuyable(false);
    });
  }

  private void handleFreeSampleChanges(Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap,
    boolean freeSampleFromRequest, boolean combinedEditFlowEnabled) {
    boolean savedFreeSample = savedItemPickupPointDataMap.values().stream()
      .filter(Objects::nonNull)
      .anyMatch(savedResponse -> Boolean.TRUE.equals(savedResponse.isFreeSample()));

    if (!Objects.equals(freeSampleFromRequest, savedFreeSample) && Boolean.TRUE.equals(
      freeSampleFromRequest && combinedEditFlowEnabled)) {
      savedItemPickupPointDataMap.values().stream().filter(Objects::nonNull)
        .forEach(itemPickupPointListingResponse -> {
          itemPickupPointListingResponse.setFreeSample(true);
          itemPickupPointListingResponse.setOnline(false);
          itemPickupPointListingResponse.getViewConfigs().stream().findFirst()
            .ifPresent(viewConfigResponse -> {
              viewConfigResponse.setDisplay(false);
              viewConfigResponse.setBuyable(false);
            });
        });
    }
  }

  private void populateResponseFlags(ItemPickupPointListingResponse response, boolean lateFulfillment,
    Boolean off2OnActiveFlag, boolean forceReview, ProductType productType) {
    response.setLateFulfillment(Boolean.TRUE.equals(lateFulfillment));
    response.setProductType(productType);
    response.setOff2OnActiveFlag(off2OnActiveFlag);
    if (forceReview) {
      handleForceReview(response);
    }
  }

  private void processItemPickupPointList(ProductDetailEditDTO productDetailEditDTO,
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap) {
    if (MapUtils.isNotEmpty(savedItemPickupPointDataMap) && combinedEditFlowEnabled) {
      savedItemPickupPointDataMap.values().stream()
        .filter(Objects::nonNull)
        .forEach(response -> {
          boolean lateFulfillment = Boolean.TRUE.equals(getLateFulfillment(productDetailEditDTO));
          ProductType type = getProductType(productDetailEditDTO);
          Boolean off2OnActiveFlag = isOff2OnActiveFlag(productDetailEditDTO);
          boolean forceReview = isForceReview(productDetailEditDTO);

          populateResponseFlags(response, lateFulfillment, off2OnActiveFlag, forceReview, type);
        });

      handleFreeSampleChanges(savedItemPickupPointDataMap, isFreeSampleFromRequest(productDetailEditDTO),
        combinedEditFlowEnabled);
    }
  }

  private void populateL3FlagsFromL3UpdateRequest(ProductDetailEditDTO productDetailEditDTO,
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap) {
    boolean freeSampleFromRequest = isFreeSampleFromRequest(productDetailEditDTO);
    processItemPickupPointList(productDetailEditDTO, savedItemPickupPointDataMap);
  }


  private ItemsPriceStockImagesUpdateResponse getResponseInCaseApplicationRuntimeException(
      ProductVariantUpdateRequest productVariantUpdateRequest, ApplicationRuntimeException e,
      SimpleStringResponse validationErrorCode) {
    LOGGER.error("Exception when updating itemPickupPoint with request : {} , error - ", productVariantUpdateRequest, e);
    String errorMessage = ErrorCategory.UNSPECIFIED.getMessage();
    if(ErrorCategory.VALIDATION.equals(e.getErrorCodes())) {
      errorMessage = e.getErrorMessage();
    }
    String errorCode = validationErrorCode.getResult();
    return getItemsPriceStockImagesUpdateResponse(productVariantUpdateRequest, errorMessage, errorCode);
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse validateL5UpdateRequest(
      ProductVariantUpdateRequest productVariantUpdateRequest, ProfileResponse profileResponse) {
    SimpleStringResponse errorCode = new SimpleStringResponse();
    List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests = new ArrayList<>();
    updateRequestsForAddDeleteVariants(productVariantUpdateRequest, newlyAddedProductItemRequests, new HashMap<>());
    try {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, errorCode,
          newlyAddedProductItemRequests, profileResponse);
    } catch (ApplicationRuntimeException e) {
      return getResponseInCaseApplicationRuntimeException(productVariantUpdateRequest, e, errorCode);
    } catch (Exception e) {
      LOGGER.error("Exception when updating itemPickupPoint with request : {} , error - ", productVariantUpdateRequest,
          e);
      return getItemsPriceStockImagesUpdateResponse(productVariantUpdateRequest, ErrorCategory.UNSPECIFIED.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode());
    }
    return null;
  }

  @Override
  public EditedResizeAndImagesUpdateStatusResponse publishEditedResizeEventIfApplicable(
      ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse,
      EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse, String productCode) {
    if (!editedResizeAndImagesUpdateStatusResponse.getEditedImages().isEmpty()) {
      if (RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType() == editResponse.getAction()) {
        log.info("Skipping edited image resize for productCode : {} productSku : {} ", productCode,
            productVariantUpdateRequest.getProductSku());
        return editedResizeAndImagesUpdateStatusResponse;
      } else {
        productPublisherService.publishEditImageResizeEvent(new EditedImageResizeEvent(productCode, DEFAULT_STORE_ID,
            editedResizeAndImagesUpdateStatusResponse.getEditedImages()));
      }
    }
    return null;
  }

  private void updateLogisticsForNewlyAddedItems(EditProductResponse editResponse, ProductCollection productCollection,
      AddDeleteVariantRequest addDeleteVariantRequest) {
    if (updateLogisticsForNewlyAddedItem) {
      if (CommonUtils.isEligibleForLogisticsUpdate(addDeleteVariantRequest)) {
        if (CollectionUtils.isNotEmpty(editResponse.getProductLevel3LogisticsRequest())) {
          List<ProductLevel3Logistics> logistics = new ArrayList<>();
          for (ProductLevel3Logistics productLevel3Logistics : editResponse.getProductLevel3LogisticsRequest()) {
            ProductLevel3Logistics productLevel3LogisticsRequest = new ProductLevel3Logistics();
            productLevel3LogisticsRequest.setSelected(productLevel3Logistics.isSelected());
            productLevel3LogisticsRequest.setLogisticProductCode(productLevel3Logistics.getLogisticProductCode());
            logistics.add(productLevel3LogisticsRequest);
          }
          List<String> itemSkuList =
              addDeleteVariantRequest.getAddVariantsList().stream().map(AddVariantRequest::getItemSku)
                  .collect(Collectors.toList());
          int size = Integer.valueOf(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
              SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE).getValue());
          List<List<String>> itemSkuPartition = Lists.partition(itemSkuList, size);
          for (List<String> itemSkus : itemSkuPartition) {
            log.info("Saving logistics for itemSkus : {} ", itemSkus);
            this.productLevel3LogisticsService.saveLogisticsByItemSku(itemSkus,
                productCollection.getBusinessPartnerCode(), logistics, true);
          }
        }
      }
    }
  }

  private void updateRequestsForAddDeleteVariants(ProductVariantUpdateRequest productVariantUpdateRequest,
      List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests, Map<String, String> extraDeletedItems) {
    List<ProductVariantPriceStockAndImagesRequest> existingItems = new ArrayList<>();
    for (ProductVariantPriceStockAndImagesRequest request : productVariantUpdateRequest.getProductItems()) {
      if (addDeleteVariantSwitch && request.isNewlyAddedItem()) {
        newlyAddedProductItemRequests.add(request);
      }
      if (!request.isNewlyAddedItem()) {
        existingItems.add(request);
      }
    }
    if (!addDeleteVariantSwitch) {
      productVariantUpdateRequest.setDeletedProductItems(new ArrayList<>());
    }
    productVariantUpdateRequest.setProductItems(existingItems);
    productItemBusinessPartnerService.getDeletedVariantItemSkus(productVariantUpdateRequest, extraDeletedItems);
    LOGGER.info("Product-update : add/ delete variants request add : {} , delete : {}",
        newlyAddedProductItemRequests, productVariantUpdateRequest.getDeletedProductItems());
  }

  private AddDeleteVariantRequest prepareAddAndDeleteVariantRequest(
    ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse,
    List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequest,
    Map<String, String> itemSkuToItemCodeMap, Map<String, Double> offerPriceMap,
    Set<String> l5sWithActiveSchedules) {
    Map<String, String> itemCodeAndProductItemId = new HashMap<>();
    AddDeleteVariantRequest addDeleteVariantRequest = new AddDeleteVariantRequest();
    if (addDeleteVariantSwitch) {
      Map<String, ProductVariantPriceStockAndImagesRequest> addNewItemsRequestMap =
          newlyAddedProductItemRequest.stream()
              .filter(ProductVariantPriceStockAndImagesRequest::isNewlyAddedItem).collect(
                  Collectors.toMap(ProductVariantPriceStockAndImagesRequest::getItemName,
                      productVariantPriceStockAndImagesRequest -> productVariantPriceStockAndImagesRequest, (a, b) -> b));
      Set<String> itemsToDelete =
          productVariantUpdateRequest.getDeletedProductItems().stream().map(DeletedProductItems::getItemSku)
              .collect(Collectors.toSet());
      List<AddVariantRequest> addVariantsList = new ArrayList<>();
      for (NewlySavedItemResponse newlySavedItemResponseFromPCB : Optional.ofNullable(editResponse.getNewlySavedItemResponseList()).orElse(Collections.emptyList())) {
        itemCodeAndProductItemId.putIfAbsent(newlySavedItemResponseFromPCB.getItemCode(),
            newlySavedItemResponseFromPCB.getProductItemId());
        ProductVariantPriceStockAndImagesRequest addNewItemRequest =
          addNewItemsRequestMap.getOrDefault(newlySavedItemResponseFromPCB.getGeneratedItemName(),
            new ProductVariantPriceStockAndImagesRequest());
        AddVariantRequest addVariantRequest =
            getAddVariantRequest(productVariantUpdateRequest, editResponse, newlySavedItemResponseFromPCB,
                addNewItemRequest);
        List<ItemPickupPointQuickEditRequest> itemPickupPointQuickEditRequests = new ArrayList<>();
        for (ItemPickupPointRequest itemPickupPointRequest : addNewItemRequest.getModifiedItemPickupPoints()) {
          processL5DetailsForAddDeleteVariantRequest(productVariantUpdateRequest, editResponse,
            offerPriceMap, l5sWithActiveSchedules, itemPickupPointRequest, addVariantRequest,
            itemPickupPointQuickEditRequests);
        }
        addVariantRequest.setItemPickupPoints(itemPickupPointQuickEditRequests);
        addVariantsList.add(addVariantRequest);
        addNewItemsRequestMap.put(newlySavedItemResponseFromPCB.getGeneratedItemName(), addNewItemRequest);
      }
      Map<String, AddVariantRequest> addVariantItemNameRequestMap = addVariantsList.stream().collect(
          Collectors.toMap(AddVariantRequest::getGeneratedItemName, addVariantRequest -> addVariantRequest,
              (a, b) -> b));
      // populating add variant request with itemSku and SkuCode
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : newlyAddedProductItemRequest) {
        setItemSkuAndSkuCodeInAddDeleteVariantRequest(itemSkuToItemCodeMap, productVariantPriceStockAndImagesRequest,
          addVariantItemNameRequestMap, itemCodeAndProductItemId);
      }
      addDeleteVariantRequest.setAddVariantsList(addVariantsList);
      addDeleteVariantRequest.setDeleteVariantsList(new ArrayList<>(itemsToDelete));
      log.info("Add/delete variant request for productSku : {} , request : {} ",
        productVariantUpdateRequest.getProductSku(), addDeleteVariantRequest);
    }
    if (CollectionUtils.isEmpty(addDeleteVariantRequest.getAddVariantsList()) && CollectionUtils.isEmpty(
        addDeleteVariantRequest.getDeleteVariantsList())) {
      return null;
    }
    return addDeleteVariantRequest;
  }

  private void processL5DetailsForAddDeleteVariantRequest(ProductVariantUpdateRequest productVariantUpdateRequest,
    EditProductResponse editResponse, Map<String, Double> offerPriceMap,
    Set<String> l5sWithActiveSchedules, ItemPickupPointRequest itemPickupPointRequest,
    AddVariantRequest addVariantRequest,
    List<ItemPickupPointQuickEditRequest> itemPickupPointQuickEditRequests) {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
        getItemPickupPointQuickEditRequest(productVariantUpdateRequest, itemPickupPointRequest, cncForWarehouseFeatureSwitch);
    Boolean scheduleRemovedForStatusUpdate =
      CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest, itemPickupPointQuickEditRequest,
        l5sWithActiveSchedules, editResponse.getScheduleRemovedForStatusUpdate(), cncForWarehouseFeatureSwitch);
    editResponse.setScheduleRemovedForStatusUpdate(scheduleRemovedForStatusUpdate);
    itemPickupPointQuickEditRequest.setItemSku(addVariantRequest.getItemSku());
    itemPickupPointRequest.setItemSku(addVariantRequest.getItemSku());
    itemPickupPointQuickEditRequests.add(itemPickupPointQuickEditRequest);
    offerPriceMap.put(itemPickupPointQuickEditRequest.getItemSku() + Constants.HYPHEN
        + itemPickupPointQuickEditRequest.getPickupPointCode(), itemPickupPointRequest.getSalePrice());
  }

  private static void setItemSkuAndSkuCodeInAddDeleteVariantRequest(
    Map<String, String> itemSkuToItemCodeMap,
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
    Map<String, AddVariantRequest> addVariantItemNameRequestMap,
    Map<String, String> itemCodeAndProductItemId) {
    if (MapUtils.isNotEmpty(addVariantItemNameRequestMap)
      && addVariantItemNameRequestMap.containsKey(
      productVariantPriceStockAndImagesRequest.getItemName())) {
      AddVariantRequest addVariantRequest =
        addVariantItemNameRequestMap.get(productVariantPriceStockAndImagesRequest.getItemName());
      productVariantPriceStockAndImagesRequest.setItemSku(addVariantRequest.getItemSku());
      productVariantPriceStockAndImagesRequest.setSkuCode(addVariantRequest.getItemCode());
      productVariantPriceStockAndImagesRequest.setProductItemId(
        itemCodeAndProductItemId.get(addVariantRequest.getItemCode()));
      itemSkuToItemCodeMap.put(addVariantRequest.getItemSku(), addVariantRequest.getItemCode());
      for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
        itemPickupPointRequest.setItemSku(addVariantRequest.getItemSku());
      }
    }
  }

  private static AddVariantRequest getAddVariantRequest(ProductVariantUpdateRequest productVariantUpdateRequest,
      EditProductResponse editResponse, NewlySavedItemResponse newlySavedItemResponse,
      ProductVariantPriceStockAndImagesRequest newlyAddedItemRequest) {
    AddVariantRequest addVariantRequest = new AddVariantRequest();
    addVariantRequest.setGeneratedItemName(newlyAddedItemRequest.getItemName());
    addVariantRequest.setMerchantSku(newlyAddedItemRequest.getMerchantSku());
    addVariantRequest.setItemCode(newlySavedItemResponse.getItemCode());
    addVariantRequest.setHeight(newlySavedItemResponse.getHeight());
    addVariantRequest.setWeight(newlySavedItemResponse.getWeight());
    addVariantRequest.setLength(newlySavedItemResponse.getLength());
    addVariantRequest.setWidth(newlySavedItemResponse.getWidth());
    addVariantRequest.setShippingWeight(newlySavedItemResponse.getShippingWeight());
    addVariantRequest.setMainImageUrl(newlySavedItemResponse.getMainImageUrl());
    addVariantRequest.setDangerousLevel(newlySavedItemResponse.getDangerousGoodsLevel());
    addVariantRequest.setForceReview(false);
    String[] sequence = newlySavedItemResponse.getItemCode().split(Constants.HYPHEN);
    addVariantRequest.setItemSku(productVariantUpdateRequest.getProductSku() + Constants.HYPHEN + sequence[2]);
    Map<String, String> attributeCodeAndName = editResponse.getAttributeCodeAndName();
    List<ProductAttributeDetailDTO> productAttributeDetailDTOS = new ArrayList<>();
    for (Map.Entry<String, String> attributeMap : newlyAddedItemRequest.getAttributesMap().entrySet()) {
      ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
      productAttributeDetailDTO.setAttributeCode(attributeMap.getKey());
      productAttributeDetailDTO.setAttributeValue(attributeMap.getValue());
      productAttributeDetailDTO.setAttributeName(attributeCodeAndName.get(attributeMap.getKey()));
      productAttributeDetailDTOS.add(productAttributeDetailDTO);
    }
    addVariantRequest.setDefiningAttributes(productAttributeDetailDTOS);
    return addVariantRequest;
  }

  @Override
  public boolean getTakeDownStatus(boolean postLiveIfImageUpdate, Map<String, Boolean> itemImagesUpdateStatus,
      EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse,
      ProductCollection productCollection) {
    boolean postLive = productCollection.isPostLive();
    if (itemImagesUpdateStatus.getOrDefault(NEW, false) || itemImagesUpdateStatus.getOrDefault(UPDATE, false)) {
      postLive = postLiveIfImageUpdate;
    }
    return !postLive && (itemImagesUpdateStatus.getOrDefault(NEW, false) || CollectionUtils
        .isNotEmpty(editedResizeAndImagesUpdateStatusResponse.getEditedImages()));
  }

  private ItemPickupPointListingResponse getPickupPointListingResponse(
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap,
    ItemPickupPointRequest itemPickupPointRequest, boolean multiPickupPointEnabledSeller) {
    String l5Code = getL5Code(itemPickupPointRequest);
    return multiPickupPointEnabledSeller ?
      savedItemPickupPointDataMap.get(l5Code) :
      savedItemPickupPointDataMap.get(itemPickupPointRequest.getItemSku());
  }

  private List<WebInventoryUpdatePickupPointRequestDTO> getWebInventoryUpdatePickupPointRequestForNonMppSeller(
    String newPickupPointCode, ItemPickupPointListingResponse itemPickupPointListingResponse) {
    List<WebInventoryUpdatePickupPointRequestDTO> webInventoryUpdatePickupPointRequestDTOList = new ArrayList<>();
    WebInventoryUpdatePickupPointRequestDTO webInventoryUpdatePickupPointRequestDTO =
      new WebInventoryUpdatePickupPointRequestDTO();
    webInventoryUpdatePickupPointRequestDTO.setWebItemSku(itemPickupPointListingResponse.getItemSku());
    webInventoryUpdatePickupPointRequestDTO.setWebMerchantCode(itemPickupPointListingResponse.getMerchantCode());
    webInventoryUpdatePickupPointRequestDTO.setPickupPointCode(itemPickupPointListingResponse.getPickUpPointCode());
    webInventoryUpdatePickupPointRequestDTO.setNewPickupPointCode(newPickupPointCode);
    webInventoryUpdatePickupPointRequestDTOList.add(webInventoryUpdatePickupPointRequestDTO);
    return webInventoryUpdatePickupPointRequestDTOList;
  }

  private void updateDiscountPriceInCampaign(
    List<UpdateDiscountDTO> campaignUpdateDiscountRequestList) throws Exception {
    if (CollectionUtils.isNotEmpty(campaignUpdateDiscountRequestList)) {
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest =
        new CampaignUpdateDiscountRequest();
      campaignUpdateDiscountRequest.setDiscountDTOList(campaignUpdateDiscountRequestList);
      CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        campaignOutbound.validateAndUpdateDiscountPrice(true, campaignUpdateDiscountRequest);
      if (MapUtils.isNotEmpty(
        campaignUpdateDiscountResponse.getItemSkuStatusMap())) {
        campaignUpdateDiscountRequestList.stream().map(UpdateDiscountDTO::getItemSku)
          .filter(campaignUpdateDiscountResponse.getItemSkuStatusMap()::containsKey).findAny()
          .ifPresent(itemSku -> {
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              campaignUpdateDiscountResponse.getItemSkuStatusMap().get(itemSku));
          });
      }
    }
  }

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public PostLiveFlagsDTO productAfterUpdateGoingForReviewIsPostLive(String storeId, String productSku,
      String businessPartnerCode, List<ProductPriceStockAndImagesRequest> successValidationVariantList,
      Map<String, Boolean> itemImagesUpdateStatus, ProductL3Response productL3Response, boolean takeDownProduct) throws Exception {
    return productGoingForReviewIsPostLive(storeId, productSku, businessPartnerCode, successValidationVariantList,
        itemImagesUpdateStatus, productL3Response, takeDownProduct);
  }

  private void createAuditLogs(String businessPartnerCode, ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap, EditItemResponse editItemResponse,
      Set<String> wholeSaleFlagUpdatedL5s, boolean isOnlineFlagChanged, Map<String, Boolean> addingWholeSale1stTimeL5s,
      ProfileResponse profileResponse, ItemPickupPointListingResponse parentProductL5Response, EditProductResponse editResponse,
      List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests, boolean combinedEditFlowEnabled,
      CombinedEditItemResponse combinedEditItemResponse) throws Exception {
    boolean multiPickupPointEnabledSeller = isMultiPickupPointEnabled(profileResponse);
    boolean isL5updated = RequestHelper.isL5Updated(editItemResponse, combinedEditItemResponse);
    if (isL5updated) {
      List<ItemPickupPoint> updatedItemPickupPoints =
          getUpdatedItemPickupPoints(editItemResponse, combinedEditFlowEnabled, combinedEditItemResponse);
      Map<String, ItemPickupPoint> updatedItemPickupPointMap = new HashMap<>();
      if (CollectionUtils.isNotEmpty(updatedItemPickupPoints)) {
        updatedItemPickupPointMap = updatedItemPickupPoints.stream().collect(Collectors.toMap(
            itemPickupPoint -> itemPickupPoint.getItemSku() + Constants.HYPHEN + itemPickupPoint.getPickupPointCode(),
            itemPickupPoint -> itemPickupPoint, (a, b) -> a));
      }

      for (ProductVariantPriceStockAndImagesRequest priceStockAndImagesRequest : productVariantUpdateRequest
          .getProductItems()) {
        for (ItemPickupPointRequest itemPickupPointRequest : priceStockAndImagesRequest.getModifiedItemPickupPoints()) {
          String L5Code = getL5Code(itemPickupPointRequest);
          if (updatedItemPickupPointMap.containsKey(L5Code)) {
            ItemPickupPointListingResponse itemPickupPointListingResponse =
              getPickupPointListingResponse(savedItemPickupPointDataMap,itemPickupPointRequest,
                multiPickupPointEnabledSeller);
            UpdateProductItemLevel3Model savedData = updateProductItemLevel3ModelConverter
                .convertFromItemPickupPointListingResponse(itemPickupPointListingResponse, addingWholeSale1stTimeL5s);
            UpdateProductItemLevel3Model updatedData = updateProductItemLevel3ModelConverter
                .convertFromItemPickupPoint(updatedItemPickupPointMap.get(L5Code), wholeSaleFlagUpdatedL5s, savedData,
                    addingWholeSale1stTimeL5s);
            this.updatedProductHistoryService
                .saveUpdateProductLevel3Audit(businessPartnerCode, itemPickupPointListingResponse.getItemSku(),
                    savedData, updatedData, StringUtils.EMPTY, itemPickupPointListingResponse.getProductSku(),
                    itemPickupPointListingResponse.getItemName(), false, StringUtils.EMPTY);
          }
        }
        if (CollectionUtils.isNotEmpty(priceStockAndImagesRequest.getModifiedItemPickupPoints())) {
          ItemPickupPointRequest itemPickupPointRequest =
              priceStockAndImagesRequest.getModifiedItemPickupPoints().get(0);
          String L5Code = getL5Code(itemPickupPointRequest);
          if (updatedItemPickupPointMap.containsKey(L5Code)) {
            ItemPickupPoint updatedData = updatedItemPickupPointMap.get(L5Code);
            ItemPickupPointListingResponse savedData =
              getPickupPointListingResponse(savedItemPickupPointDataMap,itemPickupPointRequest,
                multiPickupPointEnabledSeller);
            if (!StringUtils.equals(updatedData.getMerchantSku(), savedData.getMerchantSku())) {
              this.updatedProductHistoryService
                  .createProductL3AuditLog(businessPartnerCode, savedData.getItemSku(), savedData.getProductSku(),
                      savedData.getItemName(), UpdateProductActivity.MERCHANT_SKU.getDesc(), savedData.getMerchantSku(),
                      updatedData.getMerchantSku(), false, Constants.HYPHEN);
            }
            Set<String> scheduleUpdateChangeType =
              editResponse.getScheduleUpdatedL5s().getOrDefault(L5Code, Collections.emptySet());
            createAuditLogsForSchedulesEdit(scheduleUpdateChangeType, businessPartnerCode,
              savedData, updatedData);
          }
        }
      }
    }
    if (isOnlineFlagChanged) {
      this.updatedProductHistoryService
          .createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT, parentProductL5Response.getProductSku(),
              productVariantUpdateRequest.getProductName(), UpdateProductActivity.ONLINE_FLAG.getDesc(),
              String.valueOf(parentProductL5Response.isOnline()), String.valueOf(!parentProductL5Response.isOnline()), false,
              Constants.HYPHEN);
    }
    if (Objects.nonNull(editItemResponse) && editItemResponse.isFbbFlagChangedAtL3Level()) {
      this.updatedProductHistoryService
          .createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT, parentProductL5Response.getProductSku(),
              productVariantUpdateRequest.getProductName(), UpdateProductActivity.FBB_FLAG.getDesc(),
              String.valueOf(parentProductL5Response.isFbbActiveAtL3Level()),
              String.valueOf(!parentProductL5Response.isFbbActiveAtL3Level()), false, Constants.HYPHEN);
    }
    // Add history for add and delete pick up point
    additionAndDeletionHistory(businessPartnerCode, productVariantUpdateRequest, false);
    itemAdditionAndDeletionHistory(businessPartnerCode, productVariantUpdateRequest, editResponse, newlyAddedProductItemRequests);
  }

  private void createAuditLogsForSchedulesEdit(Set<String> scheduleUpdateChangeType,
    String businessPartnerCode, ItemPickupPointListingResponse savedData,
    ItemPickupPoint updatedData) throws Exception {
    if (CollectionUtils.isNotEmpty(scheduleUpdateChangeType)) {
      if (scheduleUpdateChangeType.contains(Constants.DISCOVERABLE_SCHEDULE_UPDATE)) {
        String savedDiscoverableSchedule =
          CommonUtils.getDiscoverableSchedule(savedData.getViewConfigs());
        String updatedDiscoverableSchedules =
          CommonUtils.getDiscoverableSchedule(updatedData.getItemViewConfig());
        this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode,
          savedData.getItemSku(), savedData.getProductSku(), savedData.getItemName(),
          UpdateProductActivity.DISCOVERABLE_SCHEDULE.getDesc(),
          String.valueOf(savedDiscoverableSchedule), String.valueOf(updatedDiscoverableSchedules),
          false, savedData.getPickUpPointCode());
      }
      if (scheduleUpdateChangeType.contains(Constants.BUYABLE_SCHEDULE_UPDATE)) {
        String savedBuyableSchedule = CommonUtils.getBuyableSchedule(savedData.getViewConfigs());
        String updatedBuyableSchedules =
          CommonUtils.getBuyableSchedule(updatedData.getItemViewConfig());
        this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode,
          savedData.getItemSku(), savedData.getProductSku(), savedData.getItemName(),
          UpdateProductActivity.BUYABLE_SCHEDULE.getDesc(), String.valueOf(savedBuyableSchedule),
          String.valueOf(updatedBuyableSchedules), false, savedData.getPickUpPointCode());

      }
    }
  }


  public List<ItemPickupPoint> getUpdatedItemPickupPoints(EditItemResponse editItemResponse,
      boolean combinedEditFlowEnabled, CombinedEditItemResponse combinedEditItemResponse) {
    List<ItemPickupPoint> updatedItemPickupPoints = new ArrayList<>();
    if (combinedEditFlowEnabled) {
      updatedItemPickupPoints =
        toItemPickupPoints(Optional.ofNullable(combinedEditItemResponse.getUpdatedItemPickupPointsVo()).orElse(Collections.emptyList()));
    } else {
      updatedItemPickupPoints = editItemResponse.getUpdatedItemPickupPoints();
    }
    return updatedItemPickupPoints;
  }

  private List<ItemPickupPoint> toItemPickupPoints(List<ItemPickupPointVo> updatedItemPickupPointsVo) {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    for (ItemPickupPointVo itemPickupPointVo : updatedItemPickupPointsVo) {
      ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
      BeanUtils.copyProperties(itemPickupPointVo, itemPickupPoint);
      itemPickupPoint.setItemViewConfig(itemPickupPointVo.getItemViewConfig());
      itemPickupPoint.setActivePromoBundlings(itemPickupPointVo.getActivePromoBundlings());
      itemPickupPoint.setPrice(itemPickupPointVo.getPrice());
      itemPickupPointList.add(itemPickupPoint);
    }
    return itemPickupPointList;
  }

  private void itemAdditionAndDeletionHistory(String businessPartnerCode,
      ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse,
      List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests) throws Exception {
    if (addDeleteVariantSwitch) {
      if (CollectionUtils.isNotEmpty(newlyAddedProductItemRequests)) {
        for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : newlyAddedProductItemRequests) {
            this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode,
                productVariantPriceStockAndImagesRequest.getItemSku(), productVariantUpdateRequest.getProductSku(),
                productVariantPriceStockAndImagesRequest.getItemName(), UpdateProductActivity.ITEM_ADDED.getDesc(),
                StringUtils.EMPTY, productVariantPriceStockAndImagesRequest.getItemSku(), false, Constants.HYPHEN);
          }
        }
      }
      if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getDeletedProductItems())) {
        for (DeletedProductItems deletedProductItems : productVariantUpdateRequest.getDeletedProductItems()) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode,
              deletedProductItems.getItemSku(), productVariantUpdateRequest.getProductSku(),
              editResponse.getItemCodeToItemNameMapping().get(deletedProductItems.getItemCode()),
              UpdateProductActivity.ITEM_DELETED.getDesc(), deletedProductItems.getItemSku(), StringUtils.EMPTY, false,
              Constants.HYPHEN);
        }
      }
  }

  private void additionAndDeletionHistory(String businessPartnerCode,
      ProductVariantUpdateRequest productVariantUpdateRequest, boolean needRevision)
      throws Exception {
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getAddPickupPoints()) || CollectionUtils
        .isNotEmpty(productVariantUpdateRequest.getDeletePickupPoints())) {
      if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getAddPickupPoints())) {
        Set<String> addedPickupPoints =
            productVariantUpdateRequest.getAddPickupPoints().stream().map(ItemPickupPointRequest::getPickupPointId)
                .collect(Collectors.toSet());
        for (String addedPickupPoint : addedPickupPoints) {
          this.updatedProductHistoryService
              .createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT, productVariantUpdateRequest.getProductSku(),
                  productVariantUpdateRequest.getProductName(), UpdateProductActivity.PICKUP_POINT_ADDED.getDesc(),
                  StringUtils.EMPTY, addedPickupPoint, needRevision, addedPickupPoint);
        }
        captureHistoryForOnlineFlagChanges(businessPartnerCode, productVariantUpdateRequest);
      }
      if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getDeletePickupPoints())) {
        Set<String> deletedPickupPoints =
            productVariantUpdateRequest.getDeletePickupPoints().stream().map(PickupPointDeleteRequest::getPickupPointId)
                .collect(Collectors.toSet());
        for (String pickupPointCode : deletedPickupPoints) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT,
              productVariantUpdateRequest.getProductSku(), productVariantUpdateRequest.getProductName(),
              UpdateProductActivity.PICKUP_POINT_DELETED.getDesc(), pickupPointCode, StringUtils.EMPTY, needRevision,
              pickupPointCode);
        }
      }
    }
  }

  private WholesaleMappingResponse getWholesaleMappingResponse(List<ItemInfoDto> itemInfoDtos, String categoryCode) {
    if (CollectionUtils.isNotEmpty(itemInfoDtos)) {
      return pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, categoryCode)
          .getValue();
    }
    return null;
  }

  private void getWholesaleResponseMap(Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
      List<ItemInfoDto> itemInfoDtos, Map<String, String> itemSkuAndPickupCodeMapping) throws Exception {
    if (CollectionUtils.isNotEmpty(itemInfoDtos)) {
      List<WholesalePriceSkuResponse> wholesalePriceByItemSkuAndPickupPointCode =
          productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(itemInfoDtos);
      for (WholesalePriceSkuResponse wholesalePriceSkuResponse : wholesalePriceByItemSkuAndPickupPointCode) {
          String pickupPointCode = StringUtils.isBlank(wholesalePriceSkuResponse.getPickUpPointCode()) ?
            itemSkuAndPickupCodeMapping.get(wholesalePriceSkuResponse.getItemSku()) :
            wholesalePriceSkuResponse.getPickUpPointCode();
        wholesalePriceSkuResponseMap
            .putIfAbsent(wholesalePriceSkuResponse.getItemSku() + Constants.HYPHEN + pickupPointCode,
                wholesalePriceSkuResponse);
      }
    }
  }

  private void captureHistoryForOnlineFlagChanges(String businessPartnerCode, ProductVariantUpdateRequest productVariantUpdateRequest) {
    productVariantUpdateRequest.getAddPickupPoints().forEach(request -> {
      try {
        if (request.isBuyable()) {
          this.updatedProductHistoryService.createProductL3AuditLog(
            businessPartnerCode, request.getItemSku(), productVariantUpdateRequest.getProductSku(),
            productVariantUpdateRequest.getProductName(), UpdateProductActivity.BUYABLE.getDesc(),
            Constants.HYPHEN, String.valueOf(true), false, request.getPickupPointId()
          );
        }
        if (request.isDisplay()) {
          this.updatedProductHistoryService.createProductL3AuditLog(
            businessPartnerCode, request.getItemSku(), productVariantUpdateRequest.getProductSku(),
            productVariantUpdateRequest.getProductName(), UpdateProductActivity.DISPLAYABLE.getDesc(),
            Constants.HYPHEN, String.valueOf(true), false, request.getPickupPointId()
          );
        }
      } catch (Exception e) {
        log.error("History Capture of new Added PP Failed for item SKU: {}", request.getItemSku(), e);
      }
    });
  }
  private void revertCampaignSellingPrice(Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap,
      List<UpdateDiscountDTO> campaignUpdateDiscountRequestList) throws Exception {
    if (CollectionUtils.isNotEmpty(campaignUpdateDiscountRequestList)) {
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
      List<UpdateDiscountDTO> updateDiscountDTOList = new ArrayList<>();
      for (UpdateDiscountDTO updateDiscountDTO : campaignUpdateDiscountRequestList) {
        ItemPickupPointListingResponse itemPickupPointListingResponse = savedItemPickupPointDataMap
            .get(updateDiscountDTO.getItemSku() + Constants.HYPHEN + updateDiscountDTO.getPickUpPointCode());
        updateDiscountDTO.setItemSku(itemPickupPointListingResponse.getItemSku());
        updateDiscountDTO.setPickUpPointCode(itemPickupPointListingResponse.getPickUpPointCode());
        updateDiscountDTO.setCategoryCode(updateDiscountDTO.getCategoryCode());
        updateDiscountDTO.setSellingPrice(itemPickupPointListingResponse.getPrices().get(0).getSalePrice());
        updateDiscountDTOList.add(updateDiscountDTO);
      }
      campaignUpdateDiscountRequest.setDiscountDTOList(updateDiscountDTOList);
      campaignOutbound.validateAndUpdateDiscountPrice(true, campaignUpdateDiscountRequest);
    }
  }

  private void inventoryDataUpdate(String businessPartnerCode, ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap, ProfileResponse profileResponse,
      Map<String, String> itemSkuToItemCodeMap,
      List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests)
      throws Exception {
    List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList = new ArrayList<>();
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest
        .getProductItems()) {
      for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest
          .getModifiedItemPickupPoints()) {
        InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO = new InventoryDetailInfoRequestDTO();
        inventoryDetailInfoRequestDTO.setPickupPointCode(itemPickupPointRequest.getPickupPointId());
        inventoryDetailInfoRequestDTO.setWebItemSku(productVariantPriceStockAndImagesRequest.getItemSku());
        inventoryDetailInfoRequestDTO.setWebMerchantCode(businessPartnerCode);
        inventoryDetailInfoRequestDTOList.add(inventoryDetailInfoRequestDTO);
      }
    }
    Map<String, ProductLevel3Inventory> savedInventoryListMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(inventoryDetailInfoRequestDTOList)) {
      List<ProductLevel3Inventory> savedInventoryList = productLevel3InventoryService
          .findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(inventoryDetailInfoRequestDTOList);
      savedInventoryListMap = savedInventoryList.stream().collect(Collectors.toMap(
          productLevel3Inventory -> productLevel3Inventory.getWebItemSku() + Constants.HYPHEN + productLevel3Inventory
              .getWebPickupPointCode(), Function.identity()));
    }
    //stock update and save in history
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getProductItems())) {
      updateStockAndPopulateHistory(businessPartnerCode, productVariantUpdateRequest,
        savedItemPickupPointDataMap, profileResponse, savedInventoryListMap);
    }
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getAddPickupPoints()) || CollectionUtils.isNotEmpty(
        newlyAddedProductItemRequests)) {
      insertInventoryForNewlyAddedL5s(businessPartnerCode, productVariantUpdateRequest,
        savedItemPickupPointDataMap, itemSkuToItemCodeMap, profileResponse, newlyAddedProductItemRequests);
    }
  }

  private void updateStockAndPopulateHistory(String businessPartnerCode,
    ProductVariantUpdateRequest productVariantUpdateRequest,
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap,
    ProfileResponse profileResponse, Map<String, ProductLevel3Inventory> savedInventoryListMap) throws Exception {
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest
        .getProductItems()) {
      if (CollectionUtils.isNotEmpty(productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints())) {
        ItemPickupPointRequest defaultItemPickupPointRequest =
            productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints().get(0);
        Integer minimumStock = defaultItemPickupPointRequest.getMinimumStock();
        String defaultL5Code =
            productVariantPriceStockAndImagesRequest.getItemSku() + Constants.HYPHEN + defaultItemPickupPointRequest
                .getPickupPointId();
        ProductLevel3Inventory savedInventory = savedInventoryListMap.get(defaultL5Code);
        if (Objects.nonNull(minimumStock) && !minimumStock.equals(
          Optional.ofNullable(savedInventory).map(ProductLevel3Inventory::getWebMinAlert).orElse(null))) {
          this.productLevel3InventoryService
              .updateMinimumStockAlert(businessPartnerCode, productVariantPriceStockAndImagesRequest.getItemSku(),
                  minimumStock);
          this.productBusinessPartnerService
              .updateMinimumStockByGdnProductItemSku(productVariantPriceStockAndImagesRequest.getItemSku(),
                  minimumStock);
          defaultItemPickupPointRequest.setItemSku(productVariantPriceStockAndImagesRequest.getItemSku());
          ItemPickupPointListingResponse itemPickupPointListingResponse =
            getPickupPointListingResponse(savedItemPickupPointDataMap,
              defaultItemPickupPointRequest, isMultiPickupPointEnabled(profileResponse));
          updatedProductHistoryService
              .createProductL3AuditLog(businessPartnerCode, productVariantPriceStockAndImagesRequest.getItemSku(),
                  productVariantPriceStockAndImagesRequest.getProductSku(),
                  itemPickupPointListingResponse.getItemName(),
                  UpdateProductActivity.STOCK_MINIMUM_VALUE.getDesc(),
                  String.valueOf(savedInventory.getWebMinAlert()), String.valueOf(minimumStock), false,
                  Constants.HYPHEN);
        }
        //sync stock update in-case of Warehouse merchant
      mergeStockValueAndSyncStockUpdate(businessPartnerCode, savedItemPickupPointDataMap,
            profileResponse, productVariantPriceStockAndImagesRequest, savedInventoryListMap,
            savedInventory, defaultItemPickupPointRequest, productVariantUpdateRequest.getPreOrder());
      }
    }
  }

  private void updateOrInsertStock(String businessPartnerCode,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap, ProfileResponse profileResponse,
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest, ItemPickupPointRequest itemPickupPointRequest,
      ProductLevel3Inventory savedInventory, PreOrderRequest preOrder,
      Map<String, ProductLevel3Inventory> savedInventoryListMap) throws Exception {
    if (ProductChangeUtil.stockChanged(itemPickupPointRequest.getStock()) || ProductChangeUtil.stockChanged(
        itemPickupPointRequest.getInitialPreOrderQuota())) {
      ApiErrorCode apiErrorCode = productLevel3InventoryService.updateOrInsertStock(
          RequestHelper.getUpdateOrInsertStockVo(mppForWhEnabled, businessPartnerCode,
              profileResponse, productVariantPriceStockAndImagesRequest, itemPickupPointRequest,
              preOrderConfig.isPoQuotaFeatureSwitch(), preOrder));
      if (Objects.isNull(apiErrorCode) && Objects.nonNull(savedInventory)) {
        Integer updatedPreOrderQuota = null;
        Integer updatedStock = null;
        ItemPickupPointListingResponse itemPickupPointListingResponse =
            getPickupPointListingResponse(savedItemPickupPointDataMap, itemPickupPointRequest,
                isMultiPickupPointEnabled(profileResponse));
        savedInventory =
            getProductLevel3InventoryByL5(savedInventory, savedInventoryListMap, itemPickupPointListingResponse);
        if (Objects.nonNull(itemPickupPointRequest.getInitialPreOrderQuota())) {
          updatedPreOrderQuota =
              savedInventory.getNullSafeInitialPreOrderQuota() + itemPickupPointRequest.getInitialPreOrderQuota();
        }
        if (Objects.nonNull(itemPickupPointRequest.getStock())) {
          updatedStock = savedInventory.getWebAvailable() + itemPickupPointRequest.getStock();
        }
        if (Optional.ofNullable(itemPickupPointRequest.getStock()).orElse(0) != 0) {
          updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, itemPickupPointRequest.getItemSku(),
              productVariantPriceStockAndImagesRequest.getProductSku(), itemPickupPointListingResponse.getItemName(),
              UpdateProductActivity.STOCK_VALUE.getDesc(), String.valueOf(savedInventory.getWebAvailable()),
              String.valueOf(updatedStock), false, itemPickupPointRequest.getPickupPointId());
        }
        if (Optional.ofNullable(itemPickupPointRequest.getInitialPreOrderQuota()).orElse(0) != 0) {
          updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, itemPickupPointRequest.getItemSku(),
              productVariantPriceStockAndImagesRequest.getProductSku(), itemPickupPointListingResponse.getItemName(),
              UpdateProductActivity.PRE_ORDER_QUOTA.getDesc(), String.valueOf(savedInventory.getNullSafeInitialPreOrderQuota()),
              String.valueOf(updatedPreOrderQuota), false, itemPickupPointRequest.getPickupPointId());
        }
      }
    }
  }

  public static ProductLevel3Inventory getProductLevel3InventoryByL5(ProductLevel3Inventory savedInventory,
      Map<String, ProductLevel3Inventory> savedInventoryListMap,
      ItemPickupPointListingResponse itemPickupPointListingResponse) {
    if (savedInventoryListMap.containsKey(itemPickupPointListingResponse.getItemSku() + Constants.HYPHEN
        + itemPickupPointListingResponse.getPickUpPointCode())) {
      savedInventory = savedInventoryListMap.get(itemPickupPointListingResponse.getItemSku() + Constants.HYPHEN
          + itemPickupPointListingResponse.getPickUpPointCode());
    }
    return savedInventory;
  }

  private void mergeStockValueAndSyncStockUpdate(String businessPartnerCode,
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap,
    ProfileResponse profileResponse,
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
    Map<String, ProductLevel3Inventory> savedInventoryListMap,
    ProductLevel3Inventory savedInventory, ItemPickupPointRequest defaultItemPickupPointRequest,
      PreOrderRequest preOrder) throws Exception {
    for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
      ProductLevel3Inventory savedL5Inventory = savedInventoryListMap.get(
        productVariantPriceStockAndImagesRequest.getItemSku() + Constants.HYPHEN
          + itemPickupPointRequest.getPickupPointId());
      if (shouldMergeStockValueAndSyncStockUpdate(itemPickupPointRequest, savedL5Inventory, profileResponse)) {
        ItemPickupPointListingResponse itemPickupPointListingResponse =
          getPickupPointListingResponse(savedItemPickupPointDataMap, itemPickupPointRequest,
            isMultiPickupPointEnabled(profileResponse));
        try {
          this.productLevel3InventoryService.updateSyncStockOrInsertStock(
            RequestHelper.getSyncStockUpdateVo(mppForWhEnabled, businessPartnerCode, profileResponse,
              productVariantPriceStockAndImagesRequest, itemPickupPointRequest,
              itemPickupPointRequest.getStock()));
          Integer updatedStock = savedInventory.getWebAvailable() + itemPickupPointRequest.getStock();
          logSyncStockAndValueUpdate(businessPartnerCode, productVariantPriceStockAndImagesRequest, savedInventory,
            defaultItemPickupPointRequest, itemPickupPointRequest, itemPickupPointListingResponse,
            updatedStock);
        }
        catch (Exception e) {
          log.error("Failed to update Inventory for sync stock and value update for {} with Error: ", itemPickupPointRequest, e);
        }
      } else {
        // Directly calling the L5 stock update as L4 sync stock is deprecated
        updateSyncStockAtL5ForSingleL5(businessPartnerCode, profileResponse, savedInventoryListMap,
            productVariantPriceStockAndImagesRequest, defaultItemPickupPointRequest, itemPickupPointRequest);
        updateOrInsertStock(businessPartnerCode, savedItemPickupPointDataMap, profileResponse,
            productVariantPriceStockAndImagesRequest, itemPickupPointRequest, savedInventory, preOrder, savedInventoryListMap);
      }
    }
  }

  private void logSyncStockAndValueUpdate(String businessPartnerCode,
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
    ProductLevel3Inventory savedInventory, ItemPickupPointRequest defaultItemPickupPointRequest,
    ItemPickupPointRequest itemPickupPointRequest,
    ItemPickupPointListingResponse itemPickupPointListingResponse, Integer updatedStock)
    throws Exception {
    updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode,
      itemPickupPointRequest.getItemSku(),
      productVariantPriceStockAndImagesRequest.getProductSku(),
      itemPickupPointListingResponse.getItemName(), UpdateProductActivity.STOCK_VALUE.getDesc(),
      String.valueOf(savedInventory.getWebAvailable()), String.valueOf(updatedStock), false,
      itemPickupPointRequest.getPickupPointId());
    this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode,
      productVariantPriceStockAndImagesRequest.getItemSku(),
      productVariantPriceStockAndImagesRequest.getProductSku(),
      productVariantPriceStockAndImagesRequest.getItemName(),
      UpdateProductActivity.SYNC_STOCK.getDesc(),
      String.valueOf(!defaultItemPickupPointRequest.getSynchronizeStock()),
      String.valueOf(defaultItemPickupPointRequest.getSynchronizeStock()), false,
      itemPickupPointRequest.getPickupPointId());
  }

  private boolean shouldMergeStockValueAndSyncStockUpdate(
    ItemPickupPointRequest itemPickupPointRequest, ProductLevel3Inventory productLevel3Inventory,
    ProfileResponse profileResponse) {
    boolean syncStockFlagUpdate =
      validateSyncStockValueUpdated(profileResponse, itemPickupPointRequest,
        productLevel3Inventory);
    boolean stockValueUpdate =
      Objects.nonNull(itemPickupPointRequest.getStock()) && !itemPickupPointRequest.getStock()
        .equals(Constants.ZERO);
    return syncStockFlagUpdate && stockValueUpdate && mergeStockValueAndSyncStockUpdate;
  }

  private void updateSyncStockAtL5ForSingleL5(String businessPartnerCode, ProfileResponse profileResponse,
      Map<String, ProductLevel3Inventory> savedInventoryListMap,
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
      ItemPickupPointRequest defaultItemPickupPointRequest, ItemPickupPointRequest itemPickupPointRequest)
      throws Exception {
    ProductLevel3Inventory savedL5Inventory = savedInventoryListMap.get(
        productVariantPriceStockAndImagesRequest.getItemSku() + Constants.HYPHEN + itemPickupPointRequest
            .getPickupPointId());
    if (validateSyncStockValueUpdated(profileResponse, itemPickupPointRequest, savedL5Inventory)) {
      this.productLevel3InventoryService.updateSyncStockOrInsertStock(
          RequestHelper.getSyncStockUpdateVo(mppForWhEnabled, businessPartnerCode, profileResponse,
              productVariantPriceStockAndImagesRequest, itemPickupPointRequest, null));
      this.updatedProductHistoryService
          .createProductL3AuditLog(businessPartnerCode, productVariantPriceStockAndImagesRequest.getItemSku(),
              productVariantPriceStockAndImagesRequest.getProductSku(),
              productVariantPriceStockAndImagesRequest.getItemName(), UpdateProductActivity.SYNC_STOCK.getDesc(),
              String.valueOf(!defaultItemPickupPointRequest.getSynchronizeStock()),
              String.valueOf(defaultItemPickupPointRequest.getSynchronizeStock()), false,
              itemPickupPointRequest.getPickupPointId());
    }
  }

  private boolean validateSyncStockValueUpdated(ProfileResponse profileResponse,
      ItemPickupPointRequest defaultItemPickupPointRequest, ProductLevel3Inventory savedL5Inventory) {
    return Objects.nonNull(savedL5Inventory) && Objects.nonNull(defaultItemPickupPointRequest.getSynchronizeStock())
        && !defaultItemPickupPointRequest.getSynchronizeStock().equals(savedL5Inventory.isWebSyncStock())
        && GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI.equals(profileResponse.getCompany().getInventoryFulfillment());
  }

  private void insertInventoryForNewlyAddedL5s(String businessPartnerCode,
      ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap,
      Map<String, String> itemSkuToItemCodeMap, ProfileResponse profileResponse,
      List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests)
      throws Exception {
    Map<String, String> itemSkuAndItemNameMapping = new HashMap<>();
    savedItemPickupPointDataMap.forEach(
      (key, value) -> itemSkuAndItemNameMapping.putIfAbsent(value.getItemSku(),
        value.getItemName()));
    Map<String, Integer> stockUpdatedForNewL5s =
      productVariantUpdateRequest.getAddPickupPoints().stream()
        .filter(itemPickupPointRequest -> Objects.nonNull(itemPickupPointRequest.getStock()))
        .collect(
          Collectors.toMap(ItemPickupPointRequest::getItemSku, (ItemPickupPointRequest::getStock), (a, b) -> a));
    List<ProductLevel3Inventory> productLevel3InventoryList = new ArrayList<>();
    for (ItemPickupPointRequest itemPickupPointRequest : productVariantUpdateRequest.getAddPickupPoints()) {
      getInventoryInsertRequest(businessPartnerCode, productVariantUpdateRequest, itemSkuToItemCodeMap, profileResponse,
          productLevel3InventoryList, itemPickupPointRequest);
    }
    // Stock update for newly added variant L5s
    for (ProductVariantPriceStockAndImagesRequest itemPickupPointRequest : newlyAddedProductItemRequests) {
      for (ItemPickupPointRequest newlyAddedL5s : itemPickupPointRequest.getModifiedItemPickupPoints()) {
        getInventoryInsertRequest(businessPartnerCode, productVariantUpdateRequest, itemSkuToItemCodeMap,
            profileResponse, productLevel3InventoryList, newlyAddedL5s);
      }
    }
    if (CollectionUtils.isNotEmpty(productLevel3InventoryList)) {
      ItemPickupPointListingResponse defaultResponse =
          savedItemPickupPointDataMap.entrySet().iterator().next().getValue();
      List<List<ProductLevel3Inventory>> inventoryParitionRequest =
          Lists.partition(productLevel3InventoryList, inventoryInsertBatchSizeForEdit);
      for (List<ProductLevel3Inventory> inventoryList : inventoryParitionRequest) {
        insertInventoryAndSuppressError(inventoryList);
      }
      for (ProductLevel3Inventory productLevel3Inventory : productLevel3InventoryList) {
        if (stockUpdatedForNewL5s.containsKey(productLevel3Inventory.getWebItemSku())) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode,
            productLevel3Inventory.getWebItemSku(), productLevel3Inventory.getProductSku(),
            itemSkuAndItemNameMapping.getOrDefault(productLevel3Inventory.getWebItemSku(),
              defaultResponse.getItemName()), UpdateProductActivity.STOCK_VALUE.getDesc(),
            Constants.HYPHEN, String.valueOf(productLevel3Inventory.getWebAvailable()), false,
            productLevel3Inventory.getWebPickupPointCode());
          generatePreOrderHistory(businessPartnerCode, profileResponse, productLevel3Inventory, itemSkuAndItemNameMapping,
              defaultResponse);
        }
      }
    }
  }

  private void generatePreOrderHistory(String businessPartnerCode, ProfileResponse profileResponse,
      ProductLevel3Inventory productLevel3Inventory, Map<String, String> itemSkuAndItemNameMapping,
      ItemPickupPointListingResponse defaultResponse) throws Exception {
    if (preOrderConfig.isPoQuotaFeatureSwitch() && CommonUtils.getBusinessPartnerFlagValue(profileResponse,
        Constants.BLIBLI_OMG) && Objects.nonNull(productLevel3Inventory.getInitialPreOrderQuota())) {
      this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode,
          productLevel3Inventory.getWebItemSku(), productLevel3Inventory.getProductSku(),
          itemSkuAndItemNameMapping.getOrDefault(productLevel3Inventory.getWebItemSku(),
              defaultResponse.getItemName()), UpdateProductActivity.PRE_ORDER_QUOTA.getDesc(), Constants.HYPHEN,
          String.valueOf(productLevel3Inventory.getInitialPreOrderQuota()), false,
          productLevel3Inventory.getWebPickupPointCode());
    }
  }

  private void insertInventoryAndSuppressError(List<ProductLevel3Inventory> inventoryList) throws Exception {
    try {
      productLevel3InventoryService.insertInventory(inventoryList);
    } catch (Exception e) {
      if(suppressInsertInventoryException) {
        log.error("Error while trying inventory request for batch : {} ", inventoryList, e);
      } else {
        log.error("Error while trying inventory request for batch : {} ", inventoryList, e);
        throw e;
      }
    }
  }

  private void getInventoryInsertRequest(String businessPartnerCode, ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, String> itemSkuToItemCodeMap, ProfileResponse profileResponse,
      List<ProductLevel3Inventory> productLevel3InventoryList, ItemPickupPointRequest itemPickupPointRequest)
      throws Exception {
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setWebItemSku(itemPickupPointRequest.getItemSku());
    productLevel3Inventory.setWebPickupPointCode(itemPickupPointRequest.getPickupPointId());
    productLevel3Inventory.setWebSyncStock(Boolean.TRUE.equals(itemPickupPointRequest.getSynchronizeStock()));
    productLevel3Inventory.setWebAvailable(itemPickupPointRequest.getStock());
    productLevel3Inventory.setWebMinAlert(itemPickupPointRequest.getMinimumStock());
    productLevel3Inventory.setWebMerchantCode(businessPartnerCode);
    productLevel3Inventory.setWarehouseItemSku(itemSkuToItemCodeMap.get(itemPickupPointRequest.getItemSku()));
    productLevel3Inventory.setProductSku(productVariantUpdateRequest.getProductSku());
    productLevel3Inventory.setFbbPP(Boolean.TRUE.equals(itemPickupPointRequest.getFbbActive()) && mppForWhEnabled);
    productLevel3Inventory.setDistributionPickupPoint(Boolean.TRUE.equals(itemPickupPointRequest.isDistribution()));
    if (Objects.nonNull(productVariantUpdateRequest.getPreOrder())) {
      CommonUtils.setPreOrderFields(preOrderConfig.isPoQuotaFeatureSwitch(), profileResponse,
          productVariantUpdateRequest.getPreOrder().getPreOrderDate(), productLevel3Inventory,
          itemPickupPointRequest.getInitialPreOrderQuota());
    }
    if (isPurchaseOrderPurchaseTerm(profileResponse)) {
      productLevel3Inventory.setWarehouseMerchantCode(GdnBaseLookup.DEFAULT_BUSINESS_PARTNER_CODE);
    } else {
      productLevel3Inventory.setWarehouseMerchantCode(profileResponse.getBusinessPartnerCode());
    }
    productLevel3InventoryList.add(productLevel3Inventory);
  }

  private void sendNotificationForStatusChange(String businessPartnerCode,
      ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap, boolean isMultiPickupPointEnabledSeller) throws Exception {
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest
        .getProductItems()) {
      for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest
          .getModifiedItemPickupPoints()) {
        if(Objects.isNull(itemPickupPointRequest.getItemSku())){
          itemPickupPointRequest.setItemSku(productVariantPriceStockAndImagesRequest.getItemSku());
        }
        ItemPickupPointListingResponse existingData =
          getPickupPointListingResponse(savedItemPickupPointDataMap,itemPickupPointRequest,
            isMultiPickupPointEnabledSeller);
        if (Objects.nonNull(existingData.getViewConfigs())) {
          if (isViewConfigChangedL5(existingData.getViewConfigs().get(0).isBuyable(),
              existingData.getViewConfigs().get(0).isDisplay(), itemPickupPointRequest.isBuyable(),
              itemPickupPointRequest.isDisplay())) {
            String status =
                ConverterUtil.getProductStatus(itemPickupPointRequest.isBuyable(), itemPickupPointRequest.isDisplay());
            if (ProductLevel3Status.B2B.name().equals(status) || ProductLevel3Status.TEASER.name().equals(status)) {
              LOGGER.debug("Send notification if product status is changed");
              productNotificationService
                  .sendNotificationForProductStatus(businessPartnerCode, existingData.getItemName(), status);
            }
          }
        }
      }
    }
  }

  private void getAddPickupPointsRequest(ProductVariantUpdateRequest productVariantUpdateRequest,
      List<ItemPickupPointQuickEditRequest> addItemPickupPoints) {
    for (ItemPickupPointRequest itemPickupPointRequest : productVariantUpdateRequest.getAddPickupPoints()) {
      ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
          getItemPickupPointQuickEditRequest(productVariantUpdateRequest, itemPickupPointRequest, cncForWarehouseFeatureSwitch);
      addItemPickupPoints.add(itemPickupPointQuickEditRequest);
    }
  }

  private static ItemPickupPointQuickEditRequest getItemPickupPointQuickEditRequest(
      ProductVariantUpdateRequest productVariantUpdateRequest, ItemPickupPointRequest itemPickupPointRequest,
      boolean cncForWarehouseFeatureSwitch) {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    BeanUtils.copyProperties(itemPickupPointRequest, itemPickupPointQuickEditRequest);
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setChannel(ChannelName.DEFAULT.name());
    priceDTO.setListPrice(itemPickupPointRequest.getPrice());
    priceDTO.setOfferPrice(itemPickupPointRequest.getSalePrice());
    itemPickupPointQuickEditRequest.setPrice(priceDTO);
    itemPickupPointQuickEditRequest.setStatus(
        ConverterUtil.getProductStatus(itemPickupPointRequest.isBuyable(), itemPickupPointRequest.isDisplay()));
    itemPickupPointQuickEditRequest.setCncStatus(
        ConverterUtil.getProductStatus(itemPickupPointRequest.isCncBuyable(), itemPickupPointRequest.isCncDisplay()));
    itemPickupPointQuickEditRequest.setPickupPointCode(itemPickupPointRequest.getPickupPointId());
    itemPickupPointQuickEditRequest.setCncActivated(itemPickupPointRequest.isCncActive());
    itemPickupPointQuickEditRequest.setFbbActivated(Objects.nonNull(itemPickupPointRequest.getFbbActive()) ?
        itemPickupPointRequest.getFbbActive() : false);
    itemPickupPointQuickEditRequest.setWholeSaleActivated(itemPickupPointRequest.getWholesalePriceActivated());
    itemPickupPointQuickEditRequest.setMerchantSku(itemPickupPointRequest.getSellerSku());
    itemPickupPointQuickEditRequest.setMerchantCode(productVariantUpdateRequest.getBusinessPartnerCode());
    itemPickupPointQuickEditRequest.setDistribution(itemPickupPointRequest.isDistribution());
    setB2bFields(itemPickupPointRequest, itemPickupPointQuickEditRequest);
    CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
      itemPickupPointQuickEditRequest, Collections.emptySet(), null, cncForWarehouseFeatureSwitch);
    return itemPickupPointQuickEditRequest;
  }

  private List<VariantsErrorListResponse> updateWholeSalePriceAndFlag(String businessPartnerCode,
      ProductVariantUpdateRequest productVariantUpdateRequest, List<VariantsErrorListResponse> failedRequests,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap,
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap, Map<String, Double> offerPriceMap,
      Map<String, String> itemSkuAndPickupCodeMapping, Set<String> wholeSaleFlagUpdatedL5s,
      List<ItemPickupPointQuickEditRequest> modifiedItemPickupPoints, Map<String, String> itemSkuToItemCodeMap,
      Map<String, Boolean> addingWholeSale1stTimeL5s, ProfileResponse profileResponse,
      List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests)
      throws Exception {
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse =
        updateBulkWholesalePriceInPricingL5(savedItemPickupPointDataMap, wholesalePriceSkuResponseMap,
            businessPartnerCode, offerPriceMap, productVariantUpdateRequest, failedRequests,
            itemSkuAndPickupCodeMapping, wholeSaleFlagUpdatedL5s, modifiedItemPickupPoints, itemSkuToItemCodeMap,
            addingWholeSale1stTimeL5s, profileResponse, newlyAddedProductItemRequests);
    if (Objects.nonNull(wholesalePriceBulkUpdateResponse)) {
      validateWholesaleResponseForFailedItemSkuL5(wholesalePriceBulkUpdateResponse.getFailedItemReason(),
          failedRequests, savedItemPickupPointDataMap, ApiErrorCode.WHOLESALE_PRICE_UPDATE_FAILED.getCode(),
          ApiErrorCode.WHOLESALE_PRICE_UPDATE_FAILED.getDesc(), itemSkuAndPickupCodeMapping, profileResponse);
    }
    return failedRequests;
  }

  private void getSuccessValidationList(ProductL3Response productL3Response,
      List<ProductPriceStockAndImagesRequest> successValidationVariantList,
      ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest) {
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest1 =
        new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest1.setSkuCode(productPriceStockAndImagesRequest.getSkuCode());
    productPriceStockAndImagesRequest1.setUpcCode(productPriceStockAndImagesRequest.getUpcCode());
    productPriceStockAndImagesRequest1.setProductCode(productL3Response.getProductCode());
    productPriceStockAndImagesRequest1.setItemName(productPriceStockAndImagesRequest.getItemName());
    productPriceStockAndImagesRequest1.setProductSku(productL3Response.getProductSku());
    productPriceStockAndImagesRequest1.setItemSku(productPriceStockAndImagesRequest.getItemSku());
    productPriceStockAndImagesRequest1.setImages(productPriceStockAndImagesRequest.getImages());
    if (Objects.nonNull(productL3Response.getMasterCatalog()) && Objects
        .nonNull(productL3Response.getMasterCatalog().getCategory())) {
      productPriceStockAndImagesRequest1
          .setCategoryCode(productL3Response.getMasterCatalog().getCategory().getCategoryCode());
    }
    successValidationVariantList.add(productPriceStockAndImagesRequest1);
  }

  private void getSuccessValidationList(ItemPickupPointListingResponse parentProductL5Response, List<ProductPriceStockAndImagesRequest> successValidationVariantList,
    ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest) {
    Set<String> uniqueItemNames =
      successValidationVariantList.stream().map(ProductPriceStockAndImagesRequest::getItemName)
        .collect(Collectors.toSet());
    ProductPriceStockAndImagesRequest newProductPriceStockAndImagesRequest;
    if (combinedEditFlowEnabled && !uniqueItemNames.contains(productPriceStockAndImagesRequest.getItemName())) {
      newProductPriceStockAndImagesRequest =
        getProductPriceStockAndImagesRequest(parentProductL5Response, productPriceStockAndImagesRequest);
    }
    else {
      newProductPriceStockAndImagesRequest =
        getProductPriceStockAndImagesRequest(parentProductL5Response, productPriceStockAndImagesRequest);
    }
    successValidationVariantList.add(newProductPriceStockAndImagesRequest);
  }


  private static ProductPriceStockAndImagesRequest getProductPriceStockAndImagesRequest(
    ItemPickupPointListingResponse parentProductL5Response,
    ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest) {
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest1 =
      new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest1.setSkuCode(productPriceStockAndImagesRequest.getSkuCode());
    productPriceStockAndImagesRequest1.setUpcCode(productPriceStockAndImagesRequest.getUpcCode());
    productPriceStockAndImagesRequest1.setProductCode(parentProductL5Response.getProductCode());
    productPriceStockAndImagesRequest1.setItemName(productPriceStockAndImagesRequest.getItemName());
    productPriceStockAndImagesRequest1.setProductSku(parentProductL5Response.getProductSku());
    productPriceStockAndImagesRequest1.setItemSku(productPriceStockAndImagesRequest.getItemSku());
    productPriceStockAndImagesRequest1.setImages(productPriceStockAndImagesRequest.getImages());
    productPriceStockAndImagesRequest1.setCategoryCode(parentProductL5Response.getCategoryCode());
    return productPriceStockAndImagesRequest1;
  }


  private void prepareRequestForUpdateL5(List<ItemPickupPointQuickEditRequest> modifiedItemPickupPoints,
      ItemPickupPointRequest itemPickupPointRequest, ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest,
    Set<String> l5sWithActiveSchedules, EditProductResponse editProductResponse) {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setChannel(ChannelName.DEFAULT.name());
    priceDTO.setListPrice(itemPickupPointRequest.getPrice());
    priceDTO.setOfferPrice(itemPickupPointRequest.getSalePrice());
    itemPickupPointQuickEditRequest.setPrice(priceDTO);
    itemPickupPointQuickEditRequest.setCncActivated(itemPickupPointRequest.isCncActive());
    itemPickupPointQuickEditRequest.setFbbActivated(itemPickupPointRequest.getFbbActive());
    itemPickupPointQuickEditRequest.setStatus(
        ConverterUtil.getProductStatus(itemPickupPointRequest.isBuyable(), itemPickupPointRequest.isDisplay()));
    itemPickupPointQuickEditRequest.setCncStatus(
        ConverterUtil.getProductStatus(itemPickupPointRequest.isCncBuyable(), itemPickupPointRequest.isCncDisplay()));
    itemPickupPointQuickEditRequest.setPickupPointCode(itemPickupPointRequest.getPickupPointId());
    itemPickupPointQuickEditRequest.setItemSku(itemPickupPointRequest.getItemSku());
    itemPickupPointQuickEditRequest.setWholeSaleActivated(itemPickupPointRequest.getWholesalePriceActivated());
    itemPickupPointQuickEditRequest.setMerchantSku(itemPickupPointRequest.getSellerSku());
    itemPickupPointQuickEditRequest
      .setPpCodeChangedForNonMppSeller(itemPickupPointRequest.isPpCodeChangedForNonMppSeller());
    setB2bFields(itemPickupPointRequest, itemPickupPointQuickEditRequest);
    editProductResponse.setScheduleRemovedForStatusUpdate(
      CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
        itemPickupPointQuickEditRequest, l5sWithActiveSchedules,
        editProductResponse.getScheduleRemovedForStatusUpdate(), cncForWarehouseFeatureSwitch));
    modifiedItemPickupPoints.add(itemPickupPointQuickEditRequest);
  }

  private static void setB2bFields(ItemPickupPointRequest itemPickupPointRequest,
      ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest) {
    if (Objects.nonNull(itemPickupPointRequest.getB2bFields())) {
      B2bFields b2bFields = new B2bFields();
      b2bFields.setBasePrice(itemPickupPointRequest.getB2bFields().getPrice());
      b2bFields.setManaged(itemPickupPointRequest.getB2bFields().isManaged());
      b2bFields.setStatus(ConverterUtil.getProductStatus(itemPickupPointRequest.getB2bFields().isBuyable(),
          itemPickupPointRequest.getB2bFields().isDisplay()));
      itemPickupPointQuickEditRequest.setB2bFields(b2bFields);
    }
  }

  private void validateWholeSaleAndSetWholeSaleActivatedFlag(
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
      ItemPickupPointRequest itemPickupPointRequest, ItemPickupPointListingResponse itemPickupPointListingResponse,
      ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest,
      WholesaleMappingResponse wholesaleMappingResponse) {
    WholesalePriceSkuResponse wholesalePriceSkuResponse = wholesalePriceSkuResponseMap.getOrDefault(
      productVariantPriceStockAndImagesRequest.getItemSku() + Constants.HYPHEN
        + itemPickupPointRequest.getPickupPointId(), null);
    if (Objects.nonNull(itemPickupPointRequest.getWholesalePriceActivated()) && itemPickupPointRequest
        .getWholesalePriceActivated()) {
      ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
      productPriceStockAndImagesRequest.setCategoryCode(itemPickupPointListingResponse.getCategoryCode());
      ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
      productLevel3PriceRequest.setPrice(itemPickupPointRequest.getPrice());
      productLevel3PriceRequest.setSalePrice(itemPickupPointRequest.getSalePrice());
      productPriceStockAndImagesRequest.setPrices(Collections.singletonList(productLevel3PriceRequest));
      Boolean isInSameThreshold = variantEditValidationService
          .isSameThreshold(productPriceStockAndImagesRequest, itemPickupPointListingResponse.getCategoryCode(),
              productVariantPriceStockAndImagesRequest.getItemSku() + itemPickupPointRequest.getPickupPointId(),
              wholesalePriceSkuResponseMap.getOrDefault(
                productVariantPriceStockAndImagesRequest.getItemSku() + Constants.HYPHEN
                  + itemPickupPointRequest.getPickupPointId(), null),
              wholesaleMappingResponse);
      itemPickupPointQuickEditRequest.setWholeSaleActivated(isInSameThreshold);
    }
    if (Objects.nonNull(wholesalePriceSkuResponse) && Constants.AUTO_INACTIVE
        .equals(wholesalePriceSkuResponse.getSkuStatus())) {
      itemPickupPointRequest.setWholesalePriceActivated(Boolean.FALSE);
    }
    else {
      itemPickupPointQuickEditRequest.setWholeSaleActivated(itemPickupPointRequest.getWholesalePriceActivated());
    }
  }

  private void validateCampaignLockPrice(List<UpdateDiscountDTO> campaignUpdateDiscountRequestList,
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
      ItemPickupPointRequest itemPickupPointRequest, ItemPickupPointListingResponse itemPickupPointListingResponse)
      throws Exception {
    if (isSalesPriceChangedOrPriceEditDisabledL5(itemPickupPointListingResponse.getPrices(),
        itemPickupPointRequest.getSalePrice(), itemPickupPointListingResponse.isPriceEditDisabled())) {
      UpdateDiscountDTO campaignUpdateDiscountRequest =
          validateItemPickupPointUpdateDiscountPrice(productVariantPriceStockAndImagesRequest.getItemSku(),
              itemPickupPointRequest.getPickupPointId(), itemPickupPointRequest.getSalePrice(),
              itemPickupPointListingResponse.getCategoryCode());
      campaignUpdateDiscountRequestList.add(campaignUpdateDiscountRequest);
    }
  }

  private void validateRequest(boolean productEditable, ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap,
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
      List<VariantsErrorListResponse> failedRequests, SimpleStringResponse errorCode,
      boolean multiPickupPointEnabledForSeller, WholesaleMappingResponse wholesaleMappingResponse,
      ProfileResponse profileResponse, List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests) throws Exception {
    variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, errorCode, profileResponse);
    variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest, errorCode, profileResponse,
        newlyAddedProductItemRequests, savedItemPickupPointDataMap);
    CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(productVariantUpdateRequest, profileResponse, faasFeatureSwitch);
    if (Boolean.TRUE.equals(productVariantUpdateRequest.isFreeSample())) {
      variantEditValidationService.validateStatusAndCNCUpdateAtL5(productVariantUpdateRequest,
        errorCode);
    }
    if (productEditable) {
      variantEditValidationService.validateListOfVariantsL5WithSuccess(productVariantUpdateRequest.getProductItems(),
          savedItemPickupPointDataMap, wholesalePriceSkuResponseMap, failedRequests, errorCode,
        multiPickupPointEnabledForSeller,  wholesaleMappingResponse);
    } else {
      variantEditValidationService
          .validateListOfVariantsL5ForMultiUsedProduct(productVariantUpdateRequest.getProductItems(),
              savedItemPickupPointDataMap, wholesalePriceSkuResponseMap, failedRequests,
            errorCode, multiPickupPointEnabledForSeller, wholesaleMappingResponse);
    }
  }

  private Map<String, ItemPickupPointListingResponse> getL5CodeAndResponseMap(String storeId,
      String businessPartnerCode, ProductVariantUpdateRequest productVariantUpdateRequest,
      List<ItemPickupPointListingRequest> itemPickupPointListingRequestList, ProfileResponse profileResponse) {
    boolean mppEnabled = CommonUtils.isMppEnabled(profileResponse, mppAllowedSellers);
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest
        .getProductItems()) {
      // we will have a check if the item is newly added then we dont need to send that item sku in request
      ItemPickupPointListingRequest itemPickupPointListingRequest = new ItemPickupPointListingRequest();
      List<String> itemPickupPoints = productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints().stream()
          .map(ItemPickupPointRequest::getPickupPointId).collect(Collectors.toList());
      itemPickupPointListingRequest.setItemSku(productVariantPriceStockAndImagesRequest.getItemSku());
      itemPickupPointListingRequest.setProductSku(productVariantUpdateRequest.getProductSku());
      itemPickupPointListingRequest.setBusinessPartnerCode(businessPartnerCode);
      itemPickupPointListingRequest.setPickupPointCodes(new HashSet<>());
      if (CollectionUtils.isNotEmpty(productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints())
          && !productVariantPriceStockAndImagesRequest.isNewlyAddedItem()) {
        itemPickupPointListingRequest.setPickupPointCodes(
            mppEnabled ? new HashSet<>(itemPickupPoints) : new HashSet<>());
      } else {
        itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
      }
      itemPickupPointListingRequestList.add(itemPickupPointListingRequest);
    }
    addMissingL4sFromAddPickupPointsList(businessPartnerCode, productVariantUpdateRequest.getProductSku(),
        itemPickupPointListingRequestList, productVariantUpdateRequest.getAddPickupPoints());
    Map<String, ItemPickupPointListingResponse> map = itemPickupPointListingRequestList.stream().map(
        itemPickupPointListingRequest -> xProductOutbound
            .getItemPickupPointList(storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
                Constants.REQUIRED_ID, Constants.DEFAULT_CLIENT_ID, 0,
                itemPickupPointListingRequest.getPickupPointCodes().size() >= 1 ?
                    itemPickupPointListingRequest.getPickupPointCodes().size() :
                    1, itemPickupPointListingRequest)).filter(Objects::nonNull).flatMap(
      itemPickupPointListingResponsePage -> itemPickupPointListingResponsePage.getContent()
        .stream()).collect(Collectors.toMap(itemPickupPointListingResponse -> mppEnabled ?
        itemPickupPointListingResponse.getItemSku() + Constants.HYPHEN
          + itemPickupPointListingResponse.getPickUpPointCode() :
        itemPickupPointListingResponse.getItemSku(),
      itemPickupPointListingResponse -> itemPickupPointListingResponse, (a, b) -> a));
    if (fetchL5DataForAddPickupPointEnabled) {
      addMissingL5sFromAddPickupPointList(storeId, businessPartnerCode, productVariantUpdateRequest, map);
    }
      return map;
  }

  public void addMissingL5sFromAddPickupPointList(String storeId, String businessPartnerCode,
      ProductVariantUpdateRequest productVariantUpdateRequest, Map<String, ItemPickupPointListingResponse> map) {
    List<ItemPickupPointListingRequest> itemPickupPointListingRequests =
        CommonUtils.getItemPickupPointListingRequestsFromAddPickupPointList(businessPartnerCode,
            productVariantUpdateRequest, map);
    if (CollectionUtils.isNotEmpty(itemPickupPointListingRequests)) {
      itemPickupPointListingRequests.forEach(request -> fetchAndMapItemPickupPoints(storeId, request, map));
    }
  }

  private void fetchAndMapItemPickupPoints(String storeId, ItemPickupPointListingRequest request,
      Map<String, ItemPickupPointListingResponse> map) {
    int pageNumber = 0;
    Page<ItemPickupPointListingResponse> responsePage;
    do {
      responsePage = xProductOutbound.getItemPickupPointList(storeId, GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), pageNumber, newlyAddedPickupPointL5FetchBatchSize, request);
      Optional.ofNullable(responsePage).map(Page::getContent).ifPresent(content -> content.forEach(
          response -> map.putIfAbsent(RequestHelper.toL5Id(response.getItemSku(), response.getPickUpPointCode()),
              response)));
      pageNumber++;
    } while (Objects.nonNull(responsePage) && responsePage.hasNext());
  }

  public void addMissingL4sFromAddPickupPointsList(String businessPartnerCode, String productSku,
      List<ItemPickupPointListingRequest> itemPickupPointListingRequestList,
      List<ItemPickupPointRequest> newlyAddedPickupPoints) {
    Set<String> itemSkus = itemPickupPointListingRequestList.stream()
        .map(ItemPickupPointListingRequest::getItemSku).collect(Collectors.toSet());
    for (ItemPickupPointRequest itemPickupPointRequest : newlyAddedPickupPoints) {
      if (!itemSkus.contains(itemPickupPointRequest.getItemSku())) {
        ItemPickupPointListingRequest itemPickupPointListingRequest = new ItemPickupPointListingRequest();
        itemPickupPointListingRequest.setItemSku(itemPickupPointRequest.getItemSku());
        itemPickupPointListingRequest.setProductSku(productSku);
        itemPickupPointListingRequest.setBusinessPartnerCode(businessPartnerCode);
        itemPickupPointListingRequest.setPickupPointCodes(new HashSet<>());
        itemPickupPointListingRequestList.add(itemPickupPointListingRequest);
        itemSkus.add(itemPickupPointRequest.getItemSku());
      }
    }
  }

  private Map<String, ItemPickupPointListingResponse> getL5CodeAndResponseMapForNeedRevison(String storeId,
      String businessPartnerCode, ProductVariantUpdateRequest productVariantUpdateRequest,
      List<ItemPickupPointListingRequest> itemPickupPointListingRequestList) {
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest.getProductItems()) {
      ItemPickupPointListingRequest itemPickupPointListingRequest = new ItemPickupPointListingRequest();
      List<String> itemPickupPoints = productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints().stream()
          .map(ItemPickupPointRequest::getPickupPointId).collect(Collectors.toList());
      itemPickupPointListingRequest.setItemSku(productVariantPriceStockAndImagesRequest.getItemSku());
      itemPickupPointListingRequest.setProductSku(productVariantUpdateRequest.getProductSku());
      itemPickupPointListingRequest.setBusinessPartnerCode(businessPartnerCode);
      itemPickupPointListingRequest.setPickupPointCodes(new HashSet<>());
      if (CollectionUtils.isNotEmpty(productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints())) {
        itemPickupPointListingRequest.setPickupPointCodes(new HashSet<>(itemPickupPoints));
      }else {
        itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
      }
      itemPickupPointListingRequestList.add(itemPickupPointListingRequest);
    }
    Map<String, ItemPickupPointListingResponse> map = itemPickupPointListingRequestList.stream().map(
            itemPickupPointListingRequest -> xProductOutbound.getItemPickupPointList(storeId, Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, Constants.REQUIRED_ID, Constants.DEFAULT_CLIENT_ID, 0,
                itemPickupPointListingRequest.getPickupPointCodes().size() >= 1 ?
                    itemPickupPointListingRequest.getPickupPointCodes().size() :
                    1, itemPickupPointListingRequest)).filter(Objects::nonNull)
        .flatMap(itemPickupPointListingResponsePage -> itemPickupPointListingResponsePage.getContent().stream())
        .collect(Collectors.toMap(
            itemPickupPointListingResponse -> CommonUtils.toL5Id(itemPickupPointListingResponse.getItemSku(),
                itemPickupPointListingResponse.getPickUpPointCode()),
            itemPickupPointListingResponse -> itemPickupPointListingResponse, (a, b) -> a));
    return map;
  }

  private void generateProductBusinessPartnerFromItemPickupPointRequest(
      ItemPickupPointRequest itemPickupPointRequest, ProductItemBusinessPartner productItemBusinessPartner) {
    productItemBusinessPartner.setBuyable(itemPickupPointRequest.isBuyable());
    productItemBusinessPartner.setDisplay(itemPickupPointRequest.isDisplay());
    productItemBusinessPartner.setCncBuyable(itemPickupPointRequest.isCncBuyable());
    productItemBusinessPartner.setCncDiscoverable(itemPickupPointRequest.isCncDisplay());
    productItemBusinessPartner.setPrice(itemPickupPointRequest.getPrice());
    productItemBusinessPartner.setSalePrice(itemPickupPointRequest.getSalePrice());
    if (!StringUtils.equals(itemPickupPointRequest.getSellerSku(), productItemBusinessPartner.getMerchantSku())) {
      productItemBusinessPartner.setMerchantSku(itemPickupPointRequest.getSellerSku());
    }
  }

  private List<ItemSummaryResponse> getItemSummaryResponseByItemSkus(List<String> itemSkus) throws Exception {
    ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
    itemSummaryRequest.setItemSkus(itemSkus);
    itemSummaryRequest.setMarkForDelete(true);
    return productLevel3Repository
        .findSummaryByFilter(itemSummaryRequest, PageRequest.of(0, itemSkus.size()), new SortOrder()).getContent();
  }

  private UpdateDiscountDTO validateItemPickupPointUpdateDiscountPrice(String itemSku, String pickupPointCode, Double salePrice,
      String masterCategoryCode) throws Exception {
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    UpdateDiscountDTO updateDiscountDTO = new UpdateDiscountDTO();
    updateDiscountDTO.setCategoryCode(masterCategoryCode);
    updateDiscountDTO.setItemSku(itemSku);
    updateDiscountDTO.setSellingPrice(salePrice);
    updateDiscountDTO.setPickUpPointCode(pickupPointCode);
    campaignUpdateDiscountRequest.setDiscountDTOList(Collections.singletonList(updateDiscountDTO));
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        campaignOutbound.validateDiscountPrice(true, campaignUpdateDiscountRequest);
    if (Objects.nonNull(campaignUpdateDiscountResponse) && MapUtils.isNotEmpty(
      campaignUpdateDiscountResponse.getItemSkuStatusMap())
      && campaignUpdateDiscountResponse.getItemSkuStatusMap().containsKey(itemSku)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          campaignUpdateDiscountResponse.getItemSkuStatusMap().get(itemSku));
    }
    return updateDiscountDTO;
  }

  private ProductLevel3Summary generateProductLevel3SummaryForUpdate(ItemSummaryResponse productItem,
      ProductLevel3Inventory inventory) throws Exception {
    PickupPointResponse pickupPointData = new PickupPointResponse();
    pickupPointData.setCode(productItem.getPickupPointCode());
    return generateProductLevel3SummarySingle(productItem, null, pickupPointData, inventory);
  }

  private ProductLevel3Summary generateProductLevel3Summary(ItemSummaryResponse savedProductData,
      ItemSummaryResponse productData, ProductLevel3Inventory inventory) throws Exception {
    List<CategoryResponse> categoryData = this.categoryRepository
        .findHierarchyByCategoryCode(productData.getMasterCatalog().getCategory().getCategoryCode());
    PickupPointResponse pickupPointData = new PickupPointResponse();
    pickupPointData.setCode(savedProductData.getPickupPointCode());
    return generateProductLevel3SummarySingle(productData, categoryData, pickupPointData, inventory);
  }

  public boolean checkIfItemChanged(UpdateItemSummaryRequest productLevel3UpdateSummaryRequest,
      ItemSummaryResponse savedProductData) {
    boolean isItemChanged = false;
    if (CollectionUtils.isNotEmpty(productLevel3UpdateSummaryRequest.getItemViewConfigs()) && isItemViewConfigChange(
        savedProductData.getItemViewConfigs(), productLevel3UpdateSummaryRequest.getItemViewConfigs())) {
      isItemChanged = true;
    }
    if (!StringUtils.equals(productLevel3UpdateSummaryRequest.getMerchantSku(), savedProductData.getMerchantSku())) {
      isItemChanged = true;
    }
    if (CollectionUtils.isNotEmpty(productLevel3UpdateSummaryRequest.getPrice()) && isItemPriceChange(
        productLevel3UpdateSummaryRequest.getPrice(), savedProductData.getPrice())) {
      isItemChanged = true;
    }
    if (Objects.nonNull(productLevel3UpdateSummaryRequest.getOff2OnChannelActive())
        && savedProductData.isOff2OnChannelActive() != productLevel3UpdateSummaryRequest.getOff2OnChannelActive()) {
      isItemChanged = true;
    }
    if (savedProductData.getWholesalePriceActivated() != productLevel3UpdateSummaryRequest
        .getWholesalePriceActivated()) {
      isItemChanged = true;
    }
    return isItemChanged;
  }

  public boolean checkIfItemChangedL5(ItemPickupPointRequest itemPickupPointRequest,
      ItemPickupPointListingResponse itemPickupPointListingResponse) {
    boolean isItemChanged = false;
    if (itemPickupPointRequest.isBuyable() != itemPickupPointListingResponse.getViewConfigs().stream()
        .filter(itemViewConfig -> ChannelName.DEFAULT.name().equals(itemViewConfig.getChannelId())).findFirst()
        .orElse(new ViewConfigResponse()).isBuyable()) {
      isItemChanged = true;
    }
    if (cncForWarehouseFeatureSwitch
        && itemPickupPointRequest.isCncBuyable() != itemPickupPointListingResponse.getViewConfigs().stream()
        .filter(itemViewConfig -> Constants.CNC_CHANNEL.equals(itemViewConfig.getChannelId())).findFirst()
        .orElse(new ViewConfigResponse()).isBuyable()) {
      isItemChanged = true;
    }
    if (itemPickupPointRequest.isDisplay() != itemPickupPointListingResponse.getViewConfigs().stream()
        .filter(itemViewConfig -> ChannelName.DEFAULT.name().equals(itemViewConfig.getChannelId())).findFirst()
        .orElse(new ViewConfigResponse()).isDisplay()) {
      isItemChanged = true;
    }
    if (cncForWarehouseFeatureSwitch && itemPickupPointRequest.isCncDisplay() != itemPickupPointListingResponse.getViewConfigs().stream()
        .filter(itemViewConfig -> Constants.CNC_CHANNEL.equals(itemViewConfig.getChannelId())).findFirst()
        .orElse(new ViewConfigResponse()).isDisplay()) {
      isItemChanged = true;
    }
    if (!StringUtils.equals(itemPickupPointRequest.getSellerSku(), itemPickupPointListingResponse.getMerchantSku())) {
      isItemChanged = true;
    }
    if (itemPickupPointRequest.getPrice() != itemPickupPointListingResponse.getPrices().get(0).getPrice()) {
      isItemChanged = true;
    }
    Set<String> clientIdList =
        Stream.of(validateOnOriginalSellingPriceClientId.split(Constants.COMMA)).collect(Collectors.toSet());
    if (validateOnOriginalSellingPrice && clientIdList.contains(GdnMandatoryRequestParameterUtil.getClientId())) {
      if (itemPickupPointRequest.getSalePrice() != itemPickupPointListingResponse.getOriginalSellingPrice()) {
        isItemChanged = true;
      }
    } else if (itemPickupPointRequest.getSalePrice() != itemPickupPointListingResponse.getPrices().get(0)
        .getSalePrice()) {
      isItemChanged = true;
    }
    isItemChanged =
        CommonUtils.checkIfB2bFieldsChanged(itemPickupPointRequest, itemPickupPointListingResponse, isItemChanged);
    if (!itemPickupPointListingResponse.isWholesalePriceConfigEnabled() && Objects
        .nonNull(itemPickupPointRequest.getWholesalePriceActivated())) {
      isItemChanged = true;
    } else if (Objects.nonNull(itemPickupPointRequest.getWholesalePriceActivated())) {
      if (itemPickupPointListingResponse.getWholesalePriceActivated() != itemPickupPointRequest
          .getWholesalePriceActivated()) {
        isItemChanged = true;
      }
    }
    if (!cncForWarehouseFeatureSwitch && (itemPickupPointListingResponse.isCncActive() != itemPickupPointRequest.isCncActive())) {
      isItemChanged = true;
    }
    if (Boolean.TRUE.equals(itemPickupPointRequest.isPpCodeChangedForNonMppSeller())) {
      isItemChanged = true;
    }
    return isItemChanged;
  }

  public boolean isItemPriceChange(Set<PriceDTO> updatedPrice, Set<PriceDTO> prices) {
    return CollectionUtils.isNotEmpty(prices) && prices.stream().anyMatch(
        updatePrice -> updatedPrice.stream().anyMatch(currentPrice -> isPriceChanged(updatePrice, currentPrice)));
  }

  private boolean isPriceChanged(PriceDTO updatePrice, PriceDTO currentPrice) {
    return currentPrice.getListPrice() != updatePrice.getListPrice() || currentPrice.getOfferPrice() != updatePrice
        .getOfferPrice();
  }

  private boolean isItemViewConfigChange(Set<ItemViewConfigDTO> currItemViewConfig,
      Set<ItemViewConfigDTO> itemViewConfig) {
    return currItemViewConfig.stream().anyMatch(updateViewConfig -> itemViewConfig.stream()
        .anyMatch(currentViewConfig -> isViewConfigChange(currentViewConfig, updateViewConfig)));
  }

  private boolean isViewConfigChange(ItemViewConfigDTO currItemViewConfig, ItemViewConfigDTO itemViewConfig) {
    return currItemViewConfig.isBuyable() != itemViewConfig.isBuyable() || currItemViewConfig.isDiscoverable() != itemViewConfig.isDiscoverable();
  }

  private boolean isSalesPriceChangedOrPriceEditDisabled(Set<PriceDTO> prices, Double salesPrice,
      boolean priceEditDisabled) {
    return (prices.stream().noneMatch(price -> salesPrice.equals(price.getOfferPrice())) && !priceEditDisabled);
  }

  private boolean isSalesPriceChangedOrPriceEditDisabledL5(List<PriceResponse> prices, Double salesPrice,
      boolean priceEditDisabled) {
    if (relaxPriceEditDisabledCheck) {
      return (prices.stream().noneMatch(price -> salesPrice.equals(price.getSalePrice())));
    } else {
      return (prices.stream().noneMatch(price -> salesPrice.equals(price.getSalePrice())) && !priceEditDisabled);
    }
  }

  private boolean isViewConfigChanged(UpdateItemSummaryRequest updateItemSummaryRequest,
      ItemSummaryResponse existingItemSummary) {
    return updateItemSummaryRequest.getItemViewConfigs().stream().anyMatch(
        itemViewConfigDTO -> itemViewConfigDTO.isBuyable() != existingItemSummary.isBuyable()
            || itemViewConfigDTO.isDiscoverable() != existingItemSummary.isDiscoverable());
  }

  private boolean isViewConfigChangedL5(boolean existingBuyable, boolean existingDisplay, boolean updatedBuyable,
      boolean updatedDisplay) {
    return !(existingBuyable == updatedBuyable && existingDisplay == updatedDisplay);
  }

  private L3VersionResponse updateProductScore(String productCode, Map<String, Boolean> itemImagesUpdateStatus,
      boolean isUpcCodeUpdate) {
    if ((itemImagesUpdateStatus.containsKey(NEW) && itemImagesUpdateStatus.get(NEW)) || (
        itemImagesUpdateStatus.containsKey(UPDATE) && itemImagesUpdateStatus.get(UPDATE)) || isUpcCodeUpdate) {
      return xProductOutbound.generateProductScoreByProductSkuOrProductCode(null, productCode, false);
    }
    return new L3VersionResponse();
  }

  public void updateItemStock(String businessPartnerCode, String gdnSku, Integer deltaStock, Integer minimumStock,
      String productSku, String itemName, ProductLevel3Inventory savedInventory,  List<VariantsErrorListResponse> failedRequests) throws Exception {
    checkItemState(gdnSku);
    if (Objects.nonNull(deltaStock)) {
      ApiErrorCode apiErrorCode = this.productLevel3InventoryService.updateStockV2(businessPartnerCode, gdnSku, deltaStock);
      if (Objects.nonNull(apiErrorCode)) {
        failedRequests.add(
            VariantsErrorListResponse.builder().itemSku(gdnSku).itemName(itemName).code(apiErrorCode.getCode())
                .message(apiErrorCode.getDesc()).build());
      } else {
        Integer updatedStock = savedInventory.getWebAvailable() + deltaStock;
        updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, gdnSku, productSku, itemName,
            UpdateProductActivity.STOCK_VALUE.getDesc(), String.valueOf(savedInventory.getWebAvailable()),
            String.valueOf(updatedStock), false, StringUtils.EMPTY);
      }
    }
    if (Objects.nonNull(minimumStock) && !minimumStock.equals(savedInventory.getWebMinAlert())) {
      this.productLevel3InventoryService.updateMinimumStockAlert(businessPartnerCode, gdnSku, minimumStock);
      this.productBusinessPartnerService.updateMinimumStockByGdnProductItemSku(gdnSku, minimumStock);
      updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, gdnSku, productSku, itemName,
          UpdateProductActivity.STOCK_MINIMUM_VALUE.getDesc(), String.valueOf(savedInventory.getWebMinAlert()),
          String.valueOf(minimumStock), false, StringUtils.EMPTY);
    }
  }

  private PostLiveFlagsDTO productGoingForReviewIsPostLive(String storeId, String productSku, String businessPartnerCode,
      List<ProductPriceStockAndImagesRequest> successValidationVariantList, Map<String, Boolean> itemImagesUpdateStatus,
      ProductL3Response productL3Response, boolean takeDownProduct) throws Exception {
    boolean postLive = false;
    Boolean postLiveConfigOfProduct = null;
    ProfileResponse profileResponse =
      businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    takeDownProduct =
      profileResponse.isTrustedSeller() ? Boolean.FALSE : takeDownProduct;
    if ((CollectionUtils.isNotEmpty(successValidationVariantList) && MapUtils.isNotEmpty(itemImagesUpdateStatus)) && (itemImagesUpdateStatus.get(NEW)
        || itemImagesUpdateStatus.get(UPDATE))) {
      ProductCollection productCollection = this.productCollectionRepository.findByStoreIdAndProductCode(storeId,
          successValidationVariantList.stream().findFirst().get().getProductCode());
      boolean relaxRestrictedKeywordCheck = profileResponse.isTrustedSeller() ? Boolean.TRUE :
        !productCollection.isRestrictedKeywordsPresent();
      List<ConfigurationStatusResponse> responseList = new ArrayList<>();
      if (!INTERNAL.equalsIgnoreCase(businessPartnerCode)) {
        responseList = this.productRepository.getConfigurationStatus(ConverterUtil
            .toConfigurationStatusRequestList(businessPartnerCode,
                successValidationVariantList.stream().findFirst().get().getCategoryCode()));
      }
      if (CollectionUtils.isNotEmpty(responseList) && POST_LIVE.equalsIgnoreCase(responseList.get(0).getReviewConfig())) {
        postLive = true;
      }
      List<String> reviewTypeList = new ArrayList<>();
      if (Objects.nonNull(productCollection.getReviewType())) {
        reviewTypeList = Stream.of(productCollection.getReviewType().split(",")).collect(Collectors.toList());
      }
      if (itemImagesUpdateStatus.get(NEW)) {
        if (!postLive && takeDownProduct) {
          takeDownOrReactivateProduct(storeId, productSku, true, null, productL3Response);
        }
        productCollection.setEdited(true);
        if (CollectionUtils.isEmpty(reviewTypeList) || !reviewTypeList.contains(IMAGE)) {
          reviewTypeList.add(IMAGE);
          productCollection.setReviewPending(true);
          productCollection.setReviewType(String.join(",", reviewTypeList));
        }
        //updating flags in PBP prd_product_collection and edit reviewType
        productCollectionRepository.saveAndFlush(productCollection);
        //updating solr review_pending flag is getting changes
        updateSolrProductCollectionDocument(productCollection);
      } else if (itemImagesUpdateStatus.get(UPDATE) && productCollection.isReviewPending()) {
        //publish Image refresh
        if (productCollection.isReviewPending()) {
          productCollection.setEdited(true);
        }
        postLiveConfigOfProduct = productCollection.isPostLive();
        productCollectionRepository.saveAndFlush(productCollection);
        ProductBusinessPartner productBusinessPartner = productBusinessPartnerService
            .findFirstByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
        AddEditedProductToPDTEvent addEditedProductToPDTEvent =
            ConverterUtil.toAddEditedProductToPDTEvent(storeId, IMAGE_REFRESH, productCollection, null,
              profileResponse, productBusinessPartner, priceInfoVendorEditedEnabled, priceInfoMaxVariantLimit);
        SellerDetailResponse sellerDetailResponse = null;
        try {
          sellerDetailResponse = productAnalyticsOutbound.getSellerDetail(productCollection.getBusinessPartnerCode());
        }catch (Exception e)
        {
          log.error("Exception while getting sellerDetail Response from ProductAnalytics for businessPartnerCode : {} ",
              productCollection.getProductCode(), e);
        }
        if (Objects.nonNull(sellerDetailResponse)) {
          addEditedProductToPDTEvent.setSellerBadge(sellerDetailResponse.getSellerBadge());
        }
        kafkaProducer.send(kafkaTopicProperties.getVendorCombinedEventNoPriority(), productCollection.getProductCode(),
            AddProductToVendorCombinedEventModel.builder().addEditedProductToPDTEvent(addEditedProductToPDTEvent)
                .build());
      }
    }
    return new PostLiveFlagsDTO(postLive, postLiveConfigOfProduct);
  }

  private void createAuditLogs(String businessPartnerCode, String itemSku, ItemSummaryResponse updatedProductData,
      ItemSummaryResponse savedProductData, String name, ProductLevel3Inventory inventory) throws Exception {
    ProductLevel3Summary savedProductLevel3Summary =
        generateProductLevel3SummaryForUpdate(savedProductData, inventory);
    ProductLevel3Summary updatedProductLevel3Summary =
        generateProductLevel3Summary(savedProductData, updatedProductData, inventory);
    UpdateProductItemLevel3Model savedData =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Summary(savedProductLevel3Summary);
    UpdateProductItemLevel3Model updatedData =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Summary(updatedProductLevel3Summary);
    if (StringUtils.isEmpty(updatedProductData.getMerchantSku()) && StringUtils
        .isNotEmpty(savedProductData.getMerchantSku()) && !StringUtils
        .equals(updatedProductData.getMerchantSku(), savedProductData.getMerchantSku())) {
      this.updatedProductHistoryService
          .createProductL3AuditLog(businessPartnerCode, itemSku, updatedProductData.getProductSku(), name,
              UpdateProductActivity.MERCHANT_SKU.getDesc(), savedProductData.getMerchantSku(),
              updatedProductData.getMerchantSku(), false, StringUtils.EMPTY);
    }
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(businessPartnerCode, itemSku, savedData, updatedData, StringUtils.EMPTY,
            updatedProductData.getProductSku(), updatedProductData.getGeneratedItemName(), false, StringUtils.EMPTY);
  }

  private List<VariantsErrorListResponse> validateWholesaleResponseForFailedItemSku(Map<String, String> failedItemSkuToFailedReasonMap,
      List<VariantsErrorListResponse> failedRequests, Map<String, ItemSummaryResponse> savedProductDataMap, String code, String message) {
    if(Objects.nonNull(failedItemSkuToFailedReasonMap)) {
      for(String itemSku : failedItemSkuToFailedReasonMap.keySet()) {
        failedRequests.add(new VariantsErrorListResponse(itemSku, savedProductDataMap.get(itemSku).getGeneratedItemName(), code, message));
      }
    }
    return failedRequests;
  }

  private List<VariantsErrorListResponse> validateWholesaleResponseForFailedItemSkuL5(
      List<FailedItemReasonDto> failedItemReasonDtos, List<VariantsErrorListResponse> failedRequests,
      Map<String, ItemPickupPointListingResponse> savedProductDataMap, String code, String message,
      Map<String, String> itemSkuAndPickupCodeMapping, ProfileResponse profileResponse) {
    boolean multiPickupPointEnabledSeller = isMultiPickupPointEnabled(profileResponse);
    if (CollectionUtils.isNotEmpty(failedItemReasonDtos)) {
      for (FailedItemReasonDto failedItemReasonDto : failedItemReasonDtos) {
        if (checkIfItemExists(savedProductDataMap, itemSkuAndPickupCodeMapping, failedItemReasonDto,
            multiPickupPointEnabledSeller)) {
          failedRequests.add(new VariantsErrorListResponse(failedItemReasonDto.getItemSku(),
              multiPickupPointEnabledSeller ?
                  savedProductDataMap.get(failedItemReasonDto.getItemSku() + Constants.HYPHEN + (StringUtils.isBlank(
                      failedItemReasonDto.getPickUpPointCode()) ?
                      itemSkuAndPickupCodeMapping.get(failedItemReasonDto.getItemSku()) :
                      failedItemReasonDto.getPickUpPointCode())).getItemName() :
                  savedProductDataMap.get(failedItemReasonDto.getItemSku()).getItemName(), code, message,
              failedItemReasonDto.getPickUpPointCode()));
        }
      }
    }
    return failedRequests;
  }

  private static boolean checkIfItemExists(Map<String, ItemPickupPointListingResponse> savedProductDataMap,
      Map<String, String> itemSkuAndPickupCodeMapping, FailedItemReasonDto failedItemReasonDto,
      boolean multiPickupPointEnabledSeller) {
    return multiPickupPointEnabledSeller ?
        savedProductDataMap.containsKey(failedItemReasonDto.getItemSku() + Constants.HYPHEN + (StringUtils.isBlank(
            failedItemReasonDto.getPickUpPointCode()) ?
            itemSkuAndPickupCodeMapping.get(failedItemReasonDto.getItemSku()) :
            failedItemReasonDto.getPickUpPointCode())) :
        savedProductDataMap.containsKey(failedItemReasonDto.getItemSku());
  }

  private Pair<Boolean, List<ProductItemUpcCodeUpdateRequest>> updateUpcCode(
      List<ProductPriceStockAndImagesRequest> successValidationVariantList, String merchantCode, boolean needCorrection,
      List<String> skuCodeList, SimpleStringResponse validationErrorCode, boolean validateUpcNewFlow,
      ProductVariantUpdateRequest productVariantUpdateRequest, boolean combinedEditFlowEnabled,
      ProductDetailEditDTO productDetailEditDTO) throws Exception {
    List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests = new ArrayList<>();
    Map<String, String> upcCodeToSkuCodeMap = new HashMap<>();
    boolean isUpcCodeUpdated = false;
    List<String> itemSkuCodes = successValidationVariantList.stream().map(ProductPriceStockAndImagesRequest::getSkuCode)
        .collect(Collectors.toList());
    String productCode = successValidationVariantList.stream().findFirst().get().getProductCode();
    SkuCodesRequest skuCodesRequest = CollectionUtils.isNotEmpty(skuCodeList) ?
        ConverterUtil.convertItemSkusToSkuCodesRequest(skuCodeList) :
        ConverterUtil.convertItemSkusToSkuCodesRequest(itemSkuCodes);
    Map<ProductItemUpcCodeUpdateRequest, Map<String, String>> historyMapping = new HashMap<>();
    List<ProductItemResponse> productItemResponseList = productOutbound.getProductItemBySkuCodes(skuCodesRequest);
    upcCodeToSkuCodeMap = productItemResponseList.stream().collect(Collectors.toMap(ProductItemResponse::getSkuCode,
        productItemResponse -> ofNullable(productItemResponse.getUpcCode()).orElse(StringUtils.EMPTY)));
    if (CollectionUtils.isNotEmpty(productItemResponseList)) {
      if (validateUpcNewFlow) {
        variantEditValidationService.validateUpcCodeL5LevelNew(upcCodeToSkuCodeMap, validationErrorCode,
            productVariantUpdateRequest, successValidationVariantList);
      }
      for (ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
        if (!validateUpcNewFlow) {
          // Skipping validation in new flow as it is already validate above
          variantEditValidationService.validateUpcCodeL5Level(upcCodeToSkuCodeMap, validationErrorCode,
              productPriceStockAndImagesRequest);
        }
        for (ProductItemResponse productItemResponse : productItemResponseList) {
          if (StringUtils.equals(productPriceStockAndImagesRequest.getSkuCode(), productItemResponse.getSkuCode())
              && !StringUtils
              .equals(productPriceStockAndImagesRequest.getUpcCode(), productItemResponse.getUpcCode())) {
            ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest = ConverterUtil
                .convertToProductItemUpcCodeUpdateRequest(productItemResponse.getSkuCode(),
                    productPriceStockAndImagesRequest.getUpcCode());
            productItemUpcCodeUpdateRequests.add(productItemUpcCodeUpdateRequest);
            Map<String ,String> newOldValueMapping = new HashMap<>();
            newOldValueMapping.put(OLD_UPC_CODE, productItemResponse.getUpcCode());
            newOldValueMapping.put(NEW_UPC_CODE, productPriceStockAndImagesRequest.getUpcCode());
            newOldValueMapping.put(ITEM_SKU, productPriceStockAndImagesRequest.getItemSku());
            newOldValueMapping.put(ITEM_NAME, productPriceStockAndImagesRequest.getItemName());
            newOldValueMapping.put(PRODUCT_SKU, productPriceStockAndImagesRequest.getProductSku());
            historyMapping.put(productItemUpcCodeUpdateRequest, newOldValueMapping);
          }
        }
       }
      }
      if (CollectionUtils.isNotEmpty(productItemUpcCodeUpdateRequests)) {
        isUpcCodeUpdated = true;
        if(combinedEditFlowEnabled){
          List<Map<String, String>> historyForUPC = new ArrayList<>(historyMapping.values());
          productDetailEditDTO.setHistoryForUPC(historyForUPC);
          return Pair.of(isUpcCodeUpdated, productItemUpcCodeUpdateRequests);
        }
        productOutbound.editItemUpcCode(productCode, productItemUpcCodeUpdateRequests);
        for (ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest : productItemUpcCodeUpdateRequests)
          createAuditLogsForUpcCodeUpdate(merchantCode, needCorrection, productItemUpcCodeUpdateRequest, historyMapping);
      }
    return Pair.of(isUpcCodeUpdated, productItemUpcCodeUpdateRequests);
  }

  private void createAuditLogsForUpcCodeUpdate(String merchantCode, boolean needCorrection,
    ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest,
    Map<ProductItemUpcCodeUpdateRequest, Map<String, String>> historyMapping) throws Exception {
    createAuditLogsForUpcCodeChanges(merchantCode,
        historyMapping.get(productItemUpcCodeUpdateRequest).get(ITEM_SKU), StringUtils.EMPTY,
        historyMapping.get(productItemUpcCodeUpdateRequest).get(OLD_UPC_CODE),
        historyMapping.get(productItemUpcCodeUpdateRequest).get(NEW_UPC_CODE),
        historyMapping.get(productItemUpcCodeUpdateRequest).get(PRODUCT_SKU),
        historyMapping.get(productItemUpcCodeUpdateRequest).get(ITEM_NAME), needCorrection);
  }

  private boolean updateUpcCodeL5(List<ProductVariantPriceStockAndImagesRequest> successValidationVariantList,
      String merchantCode, boolean needCorrection, ProductVariantUpdateRequest request) throws Exception {
    List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests = new ArrayList<>();
    boolean isUpcCodeUpdated = false;
    List<String> itemSkuCodes =
        successValidationVariantList.stream().map(ProductVariantPriceStockAndImagesRequest::getSkuCode)
            .collect(Collectors.toList());
    String productCode = request.getProductCode();
    SkuCodesRequest skuCodesRequest = ConverterUtil.convertItemSkusToSkuCodesRequest(itemSkuCodes);
    Map<ProductItemUpcCodeUpdateRequest, Map<String, String>> historyMapping = new HashMap<>();
    List<ProductItemResponse> productItemResponseList = productOutbound.getProductItemBySkuCodes(skuCodesRequest);
    if (CollectionUtils.isNotEmpty(productItemResponseList)) {
      for (ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
        for (ProductItemResponse productItemResponse : productItemResponseList) {
          if (StringUtils.equals(productPriceStockAndImagesRequest.getSkuCode(), productItemResponse.getSkuCode())
              && !StringUtils.equals(productPriceStockAndImagesRequest.getUpcCode(),
              productItemResponse.getUpcCode())) {
            ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest =
                ConverterUtil.convertToProductItemUpcCodeUpdateRequest(productItemResponse.getSkuCode(),
                    productPriceStockAndImagesRequest.getUpcCode());
            productItemUpcCodeUpdateRequests.add(productItemUpcCodeUpdateRequest);
            Map<String, String> newOldValueMapping = new HashMap<>();
            newOldValueMapping.put(OLD_UPC_CODE, productItemResponse.getUpcCode());
            newOldValueMapping.put(NEW_UPC_CODE, productPriceStockAndImagesRequest.getUpcCode());
            newOldValueMapping.put(ITEM_SKU, productPriceStockAndImagesRequest.getItemSku());
            newOldValueMapping.put(ITEM_NAME, productPriceStockAndImagesRequest.getItemName());
            newOldValueMapping.put(PRODUCT_SKU, productPriceStockAndImagesRequest.getProductSku());
            historyMapping.put(productItemUpcCodeUpdateRequest, newOldValueMapping);
          }
        }
      }
      if (CollectionUtils.isNotEmpty(productItemUpcCodeUpdateRequests)) {
        isUpcCodeUpdated = true;
        productOutbound.editItemUpcCode(productCode, productItemUpcCodeUpdateRequests);
        for (ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest : productItemUpcCodeUpdateRequests) {
          createAuditLogsForUpcCodeChanges(merchantCode,
              historyMapping.get(productItemUpcCodeUpdateRequest).get(ITEM_SKU), StringUtils.EMPTY,
              historyMapping.get(productItemUpcCodeUpdateRequest).get(OLD_UPC_CODE),
              historyMapping.get(productItemUpcCodeUpdateRequest).get(NEW_UPC_CODE),
              historyMapping.get(productItemUpcCodeUpdateRequest).get(PRODUCT_SKU),
              historyMapping.get(productItemUpcCodeUpdateRequest).get(ITEM_NAME), needCorrection);
        }
      }
    }
    return isUpcCodeUpdated;
  }

  private void createAuditLogsForUpcCodeChanges(String merchantCode, String itemSku, String accessChannel,
      String oldUpcCode, String newUpcCode, String productSku, String name, boolean needCorrection) throws Exception {
    if (needCorrection) {
      this.updatedProductHistoryService.createProductL3AuditLog(merchantCode, itemSku, productSku, name,
          UpdateProductActivity.UPC_CODE.getDesc() + Constants.NEED_REVISION, oldUpcCode, newUpcCode, false,
          StringUtils.EMPTY);
    } else {
      this.updatedProductHistoryService
          .createProductL3AuditLog(merchantCode, itemSku, productSku, name, UpdateProductActivity.UPC_CODE.getDesc(),
              oldUpcCode, newUpcCode, false, StringUtils.EMPTY);
    }
  }

  private ItemsPriceStockImagesUpdateResponse createItemsPriceStockImagesUpdateResponse(
      List<VariantsErrorListResponse> failedRequests, boolean goingToReview, boolean postLive, boolean takeDown) {
    return ItemsPriceStockImagesUpdateResponse.builder().postLive(postLive).productReview(goingToReview)
        .variantsErrorList(failedRequests).takeDown(takeDown).build();
  }

  private EditedResizeAndImagesUpdateStatusResponse updateProductItemsImages(
      List<ProductPriceStockAndImagesRequest> successValidationVariantList,
      List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages, String productCode,
      String businessPartnerCode, boolean needCorrection, boolean activatedBefore,
    ProductDetailEditDTO productDetailEditDTO) throws Exception {
    EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse =
        new EditedResizeAndImagesUpdateStatusResponse();
    List<ProductItemImageRequest> updatedProductItemImageRequestList = new ArrayList<>();
    List<ProductItemImageRequest> newProductItemImageRequestList = new ArrayList<>();
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> productImageEditRequestsForPCB = new ArrayList<>();
    boolean goingForReview = false;
    boolean isImageUpdated = false;
    String productSku = StringUtils.EMPTY;
    Map<String, Boolean> itemImagesUpdateStatus = new HashMap<>();
    Map<String, String> itemSkuItemNameMap = new HashMap<>();
    Map<String, String> skuCodeItemSkuMap = new HashMap<>();
    boolean combinedEditPDPUpdate = combinedEditFlowEnabled && Objects.nonNull(productDetailEditDTO);
    List<ProductLevel3SummaryDetailsImageRequest> productLevel3SummaryDetailsImageRequests = new ArrayList<>();

    LOGGER.info("successValidationVariantList {} ", successValidationVariantList);
    for (ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      productSku = productPriceStockAndImagesRequest.getProductSku();
      itemSkuItemNameMap.put(productPriceStockAndImagesRequest.getItemSku(),
        productPriceStockAndImagesRequest.getItemName());
      skuCodeItemSkuMap.put(productPriceStockAndImagesRequest.getSkuCode(),
        productPriceStockAndImagesRequest.getItemSku());
      LOGGER.info("productPriceStockAndImagesRequest {} ", productPriceStockAndImagesRequest);
      LOGGER.info("copyToAllVariantImages {} ", copyToAllVariantImages);
      for (ProductLevel3SummaryDetailsImageRequest image : productPriceStockAndImagesRequest.getImages()) {
        if (NEW.equalsIgnoreCase(image.getReviewType())) {
          goingForReview = true;
        }
        if (UPDATE.equalsIgnoreCase(image.getReviewType())) {
          isImageUpdated = true;
        }
      }
      if (isImageUpdated) {
        ProductItemImageRequest updatedProductItemImageRequest = ConverterUtil
            .toUpdatedProductItemImageRequest(productPriceStockAndImagesRequest.getImages(),
                productPriceStockAndImagesRequest.getSkuCode());
        updatedProductItemImageRequestList.add(updatedProductItemImageRequest);
      }
      if (goingForReview) {
        productLevel3SummaryDetailsImageRequests.addAll(productPriceStockAndImagesRequest.getImages());
        ProductItemImageRequest newProductItemImageRequest = ConverterUtil
            .toNewProductItemImageRequest(productPriceStockAndImagesRequest.getImages(),
                productPriceStockAndImagesRequest.getSkuCode(), needCorrection);
        newProductItemImageRequestList.add(newProductItemImageRequest);
      }
    }
    if(CollectionUtils.isNotEmpty(copyToAllVariantImages) && CollectionUtils.isNotEmpty(successValidationVariantList)) {
      productLevel3SummaryDetailsImageRequests.addAll(copyToAllVariantImages);
      for (ProductLevel3SummaryDetailsImageRequest image : copyToAllVariantImages) {
        if (NEW.equalsIgnoreCase(image.getReviewType())) {
          goingForReview = true;
        }
        if (UPDATE.equalsIgnoreCase(image.getReviewType())) {
          isImageUpdated = true;
        }
      }
      productImageEditRequestsForPCB =
        ConverterUtil.toProductImageEditRequestList(copyToAllVariantImages, needCorrection,
          activatedBefore, productCode);
    }
    itemImagesUpdateStatus.put(NEW, goingForReview);
    itemImagesUpdateStatus.put(UPDATE, isImageUpdated);
    List<LocationPathAndCommonImage> locationPathAndCommonImages = new ArrayList<>();
    LOGGER.info("goingForReview : {} ", goingForReview);
    LOGGER.info("isImageUpdated : {} ", isImageUpdated);
    if (updateCommonImageBeforeVariantImages && CollectionUtils.isNotEmpty(
      productImageEditRequestsForPCB)) {
      updateCommonImages(businessPartnerCode, needCorrection, productImageEditRequestsForPCB,
        locationPathAndCommonImages, productSku, isImageUpdated);
    }
    if (goingForReview || isImageUpdated) {
      if (combinedEditPDPUpdate) {
        //skip Item image update separately and populate common image update to update item images
        productImageEditRequestsForPCB =
          fetchProductImageEditRequestsForPCBForCombinedEdit(productCode, needCorrection,
            activatedBefore, productDetailEditDTO, updatedProductItemImageRequestList,
            newProductItemImageRequestList, productImageEditRequestsForPCB,
            editedResizeAndImagesUpdateStatusResponse, itemImagesUpdateStatus, itemSkuItemNameMap,
            skuCodeItemSkuMap, productLevel3SummaryDetailsImageRequests);
        log.info("ImagesUpdateStatusResponse for product : {} is {} ", productCode,
          editedResizeAndImagesUpdateStatusResponse);
        return editedResizeAndImagesUpdateStatusResponse;
      }
      // proceed with updating variant images in PCB with switch off
      ItemImageUpdateRequestPCB itemImageUpdateRequestForPCB =
        ItemImageUpdateRequestPCB.builder().productCode(productCode).productSku(productSku)
          .businessPartnerCode(businessPartnerCode).needCorrection(needCorrection)
          .updatedProductItemImageRequestList(updatedProductItemImageRequestList)
          .newProductItemImageRequestList(newProductItemImageRequestList)
          .locationPathAndCommonImages(locationPathAndCommonImages).build();
      locationPathAndCommonImages =
        updateItemImages(itemImageUpdateRequestForPCB, itemSkuItemNameMap, skuCodeItemSkuMap,
          productImageEditRequestsForPCB);
    }

    if (CollectionUtils.isNotEmpty(productImageEditRequestsForPCB) && !updateCommonImageBeforeVariantImages) {
      updateCommonImages(businessPartnerCode, needCorrection, productImageEditRequestsForPCB,
        locationPathAndCommonImages, productSku, isImageUpdated);
    }
    EditedResizeImagesResponse editedResizeImagesResponse = new EditedResizeImagesResponse();
      if (goingForReview && !needCorrection) {
        editedResizeImagesResponse =
          publishEditImagesForResizing(productLevel3SummaryDetailsImageRequests,
            locationPathAndCommonImages, productCode);
      }
    return EditedResizeAndImagesUpdateStatusResponse.builder()
      .itemImagesUpdateStatus(itemImagesUpdateStatus)
      .editedImages(editedResizeImagesResponse.getEditedImages()).build();
  }

  private void updateCommonImages(String businessPartnerCode, boolean needCorrection,
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> productImageEditRequestsForPCB,
    List<LocationPathAndCommonImage> locationPathAndCommonImages, String productSku,
    boolean isImageUpdated) throws Exception {
    //updating common image
    LOGGER.info("listrequest formed is {} ", productImageEditRequestsForPCB);
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> successRequest =
      new ArrayList<>();
    Map<String, Map<String, String>> response = productOutbound.updateCommonImages(
      productImageEditRequestsForPCB);
    LOGGER.info("Updated common images for request : {} response : {} ",
      productImageEditRequestsForPCB, response);
    for (com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest pcbRequest : productImageEditRequestsForPCB) {
      String imageResponse =
        response.get(pcbRequest.getImagePath()).get(Constants.COPY_ALL_STATUS);
      if (Constants.SUCCESS.equals(imageResponse)) {
        successRequest.add(pcbRequest);
        if (pcbRequest.getCopyToAllVariantImages().isAdd()) {
          LocationPathAndCommonImage locationPathAndCommonImage =
            new LocationPathAndCommonImage(pcbRequest.getImagePath(), true);
          locationPathAndCommonImages.add(locationPathAndCommonImage);
        }
      }
    }
    createAuditLogs(businessPartnerCode, successRequest, productSku, needCorrection, isImageUpdated);
  }

  @Override
  public List<LocationPathAndCommonImage> updateItemImages(
    ItemImageUpdateRequestPCB itemImageUpdateRequestPCB, Map<String, String> itemSkuItemNameMap,
    Map<String, String> skuCodeItemSkuMap,
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> productImageEditRequestsForPCB)
    throws Exception {
    List<LocationPathAndCommonImage> locationPathAndCommonImages =
      itemImageUpdateRequestPCB.getLocationPathAndCommonImages();
    if (CommonUtils.eligibleForVariantImageUpdate(
      itemImageUpdateRequestPCB.getUpdatedProductItemImageRequestList(),
      itemImageUpdateRequestPCB.getNewProductItemImageRequestList(), addDeleteVariantSwitch)) {
      locationPathAndCommonImages =
        updateItemImagesinPCB(itemImageUpdateRequestPCB.getProductCode(),
          itemImageUpdateRequestPCB.getBusinessPartnerCode(),
          itemImageUpdateRequestPCB.isNeedCorrection(),
          itemImageUpdateRequestPCB.getUpdatedProductItemImageRequestList(),
          itemImageUpdateRequestPCB.getNewProductItemImageRequestList(),
          locationPathAndCommonImages, itemImageUpdateRequestPCB.getProductSku(),
          itemSkuItemNameMap, skuCodeItemSkuMap, productImageEditRequestsForPCB);
    }
    return locationPathAndCommonImages;
  }

  private List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> fetchProductImageEditRequestsForPCBForCombinedEdit(
    String productCode, boolean needCorrection, boolean activatedBefore, ProductDetailEditDTO productDetailEditDTO,
    List<ProductItemImageRequest> updatedProductItemImageRequestList, List<ProductItemImageRequest> newProductItemImageRequestList,
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> productImageEditRequestsForPCB,
    EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse,
    Map<String, Boolean> itemImagesUpdateStatus, Map<String, String> itemSkuItemNameMap,
    Map<String, String> skuCodeItemSkuMap, List<ProductLevel3SummaryDetailsImageRequest> productLevel3SummaryDetailsImageRequests) {
    Pair<List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest>, Map<String, String>>
      hashCodeLocationMapAndVariantImageEditRequest =
      fetchHashCodeLocationMapAndVariantImageEditRequest(updatedProductItemImageRequestList,
        newProductItemImageRequestList);

    List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest>
      itemImageEditRequests = filterVariantImageEditRequests(hashCodeLocationMapAndVariantImageEditRequest);

    // productImageEditRequestsForPCB request is already populated with common images,
    // process newly added images at variant level
    productImageEditRequestsForPCB =
      getProductImageEditRequestList(productCode, activatedBefore, itemImageEditRequests,
        hashCodeLocationMapAndVariantImageEditRequest, productImageEditRequestsForPCB);

    // final product and variant image request for all variants except newly added variants
    productDetailEditDTO.setProductImageEditRequests(productImageEditRequestsForPCB);

    populateImageRequestForHistoryAndResizeEvents(productCode, needCorrection,
      updatedProductItemImageRequestList, newProductItemImageRequestList,
      editedResizeAndImagesUpdateStatusResponse, itemImagesUpdateStatus, itemSkuItemNameMap,
      skuCodeItemSkuMap, productLevel3SummaryDetailsImageRequests);
    return productImageEditRequestsForPCB;
  }

  private static void populateImageRequestForHistoryAndResizeEvents(String productCode,
   boolean needCorrection,
    List<ProductItemImageRequest> updatedProductItemImageRequestList,
    List<ProductItemImageRequest> newProductItemImageRequestList,
    EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse,
    Map<String, Boolean> itemImagesUpdateStatus, Map<String, String> itemSkuItemNameMap,
    Map<String, String> skuCodeItemSkuMap,
    List<ProductLevel3SummaryDetailsImageRequest> productLevel3SummaryDetailsImageRequests) {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = ConverterUtil.toProductItemImageUpdateRequest(
      updatedProductItemImageRequestList, newProductItemImageRequestList, null, productCode,
      needCorrection);
    editedResizeAndImagesUpdateStatusResponse.setItemImagesUpdateStatus(itemImagesUpdateStatus);
    ProductItemImageHistoryDTO productItemImageHistoryDTO =
      ProductItemImageHistoryDTO.builder().productItemImageUpdateRequest(productItemImageUpdateRequest)
        .itemSkuItemNameMap(itemSkuItemNameMap).skuCodeItemSkuMap(skuCodeItemSkuMap).build();
    editedResizeAndImagesUpdateStatusResponse.setProductItemImageHistoryDTO(
      productItemImageHistoryDTO);
    editedResizeAndImagesUpdateStatusResponse.setProductLevel3SummaryDetailsImageRequests(
      productLevel3SummaryDetailsImageRequests);
  }

  private static List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> getProductImageEditRequestList(
    String productCode, boolean activatedBefore,
    List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest> itemImageEditRequests,
    Pair<List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest>, Map<String, String>> hashCodeLocationMapAndVariantImageEditRequest,
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> listRequest) {

    // variant Image Edit request currently is populated with hash codes, map the corresponding
    // location paths to each image request
    for (com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest imageEditRequest : itemImageEditRequests) {
        com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest
          productItemImageUpdateRequest = new com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest();
        productItemImageUpdateRequest.setProductCode(productCode);
        productItemImageUpdateRequest.setImagePath(
          hashCodeLocationMapAndVariantImageEditRequest.getValue().get(imageEditRequest.getHashCode()));
        productItemImageUpdateRequest.setActivatedBefore(activatedBefore);
        productItemImageUpdateRequest.setProductItems(Collections.singletonList(imageEditRequest));
        productItemImageUpdateRequest.setMarkForDelete(imageEditRequest.isMarkForDelete());
        productItemImageUpdateRequest.setNeedRevision(false);
        listRequest.add(productItemImageUpdateRequest);
      }

    listRequest = new ArrayList<>(listRequest.stream().collect(Collectors.toMap(
      productImageEditRequest -> productImageEditRequest.getImagePath().concat(productImageEditRequest.getProductItems().stream()
        .map(com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest::getItemCode).findAny()
        .orElse(StringUtils.EMPTY)), Function.identity(), (existing, replacement) -> existing))
      .values());
    return listRequest;
  }

  private static List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest> filterVariantImageEditRequests(
    Pair<List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest>, Map<String, String>> hashCodeLocationMapAndVariantImageEditRequest) {
    return hashCodeLocationMapAndVariantImageEditRequest.getKey().stream()
      .filter(image -> StringUtils.isNotEmpty(image.getItemCode()))
      .filter(image -> hashCodeLocationMapAndVariantImageEditRequest.getValue()
        .containsKey(image.getHashCode())).collect(Collectors.toList());
  }
  private List<LocationPathAndCommonImage> updateItemImagesinPCB(String productCode,
    String businessPartnerCode, boolean needCorrection,
    List<ProductItemImageRequest> updatedProductImageRequest,
    List<ProductItemImageRequest> newProductImageRequest,
    List<LocationPathAndCommonImage> locationPathAndCommonImages, String productSku,
    Map<String, String> itemSkuItemNameMap, Map<String, String> skuCodeItemSkuMap,
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> listRequest)
    throws Exception {
    ProductCollection productCollection =
      productCollectionRepository.findByStoreIdAndProductCode(DEFAULT_STORE_ID, productCode);
    ProductItemImageUpdateRequest productItemImageUpdateRequest =
      ConverterUtil.toProductItemImageUpdateRequest(updatedProductImageRequest,
        newProductImageRequest, null, productCode, needCorrection);
    productItemImageUpdateRequest.setActivatedBefore(CommonUtils.isProductActivatedBefore(productCollection));
    //updating variant image
    locationPathAndCommonImages =
      updateItemLevelImages(businessPartnerCode, needCorrection, locationPathAndCommonImages,
        productItemImageUpdateRequest, productSku, itemSkuItemNameMap, skuCodeItemSkuMap,
        listRequest);
    return locationPathAndCommonImages;
  }

  Pair<List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest>, Map<String, String>> fetchHashCodeLocationMapAndVariantImageEditRequest(
    List<ProductItemImageRequest> updatedProductItemImageRequestList,
    List<ProductItemImageRequest> newProductItemImageRequestList) {
    ImmutableMap<String, List<ProductItemImageRequest>> imageStatusXItemImageRequestMap =
      ImmutableMap.of(NEW, newProductItemImageRequestList, UPDATE,
        updatedProductItemImageRequestList);
    List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest> itemImageEditRequest =
      ConverterUtil.toItemImageEditRequest(imageStatusXItemImageRequestMap);
    List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest> finalItemImageEditRequest =
      itemImageEditRequest.stream().filter(image -> StringUtils.isNotEmpty(image.getItemCode()))
        .collect(Collectors.toList());
    Map<String, String> hashcodeLocationPathMap =
      imageStatusXItemImageRequestMap.values().stream().flatMap(List::stream)
        .map(ProductItemImageRequest::getItemImages).flatMap(List::stream).collect(
          Collectors.toMap(Image::getHashCode, Image::getLocationPath,
            (exitingLocation, newLocation) -> exitingLocation));
    return Pair.of(finalItemImageEditRequest, hashcodeLocationPathMap);

  }
  @Override
  public List<LocationPathAndCommonImage> updateItemLevelImages(String businessPartnerCode, boolean needCorrection,
      List<LocationPathAndCommonImage> locationPathAndCommonImages,
      ProductItemImageUpdateRequest productItemImageUpdateRequest, String productSku,
      Map<String, String> itemSkuItemNameMap, Map<String, String> skuCodeItemSkuMap,
      List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> listRequest) throws Exception {
    try {
      locationPathAndCommonImages = productOutbound.updateProductItemImagesByProductCode(productItemImageUpdateRequest);
      createAuditLogs(businessPartnerCode, productItemImageUpdateRequest, productSku, itemSkuItemNameMap,
          skuCodeItemSkuMap, needCorrection);
    } catch (Exception e) {
      log.error("Error while updating item image for productSku : {} ", productSku, e);
      boolean newImageAddedAtCommonImageLevel = false;
      if (suppressItemImageUpdateError) {
        if (StringUtils.isNotEmpty(e.getMessage()) && e.getMessage().contains(NO_MAIN_IMAGE_PRESENT)) {
          if (CollectionUtils.isNotEmpty(listRequest)) {
            for (com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest productImageEditRequest : listRequest) {
              if (Objects.nonNull(productImageEditRequest.getCopyToAllVariantImages())
                  && productImageEditRequest.getCopyToAllVariantImages().isAdd()) {
                newImageAddedAtCommonImageLevel = true;
                break;
              }
            }
          }
        }
      }
      if (newImageAddedAtCommonImageLevel) {
        log.info("Suppressing error of item image update of productSku : {} ", productSku);
      } else {
        throw e;
      }
    }
    return locationPathAndCommonImages;
  }

  private void createAuditLogs(String businessPartnerCode, ProductItemImageUpdateRequest productItemImageUpdateRequest,
    String productSku, Map<String, String> itemSkuItemNameMap, Map<String, String> skuCodeItemSkuMap, boolean needCorrection)
    throws Exception {
    String newImageActivity = needCorrection ?
      (UpdateProductActivity.IMAGE_ADDED.getDesc() + Constants.NEED_REVISION) :
      UpdateProductActivity.IMAGE_ADDED.getDesc();
    String deleteImageActivity = needCorrection ?
      (UpdateProductActivity.IMAGE_DELETED.getDesc() + Constants.NEED_REVISION) :
      UpdateProductActivity.IMAGE_DELETED.getDesc();
    String updateImageActivity = needCorrection ?
      (UpdateProductActivity.IMAGE_UPDATED.getDesc() + Constants.NEED_REVISION) :
      UpdateProductActivity.IMAGE_UPDATED.getDesc();
    if (CollectionUtils.isNotEmpty(productItemImageUpdateRequest.getCopyToAllVariantImages())) {
      String productName =
          productLevel3Repository.getProductNameByProductSku(Arrays.asList(productSku)).get(productSku);
      this.updatedProductHistoryService
          .createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT, productSku, productName, newImageActivity,
              StringUtils.EMPTY, productItemImageUpdateRequest.getCopyToAllVariantImages().toString(), false,
              StringUtils.EMPTY);
    }
    if (CollectionUtils.isNotEmpty(productItemImageUpdateRequest.getNewProductItemImages())) {
      for (ProductItemImageRequest productItemImageRequest : productItemImageUpdateRequest.getNewProductItemImages()) {
        String itemSku = skuCodeItemSkuMap.get(productItemImageRequest.getSkuCode());
        for (Image image : productItemImageRequest.getItemImages()) {
          this.updatedProductHistoryService
              .createProductL3AuditLog(businessPartnerCode, itemSku, productSku, itemSkuItemNameMap.get(itemSku),
                  newImageActivity, StringUtils.EMPTY,
                  image.getLocationPath(), false, StringUtils.EMPTY);
        }
      }
    }
    if (CollectionUtils.isNotEmpty(productItemImageUpdateRequest.getUpdateProductItemImages())) {
      for (ProductItemImageRequest productItemImageRequest : productItemImageUpdateRequest
          .getUpdateProductItemImages()) {
        for (Image image : productItemImageRequest.getItemImages()) {
          String itemSku = skuCodeItemSkuMap.get(productItemImageRequest.getSkuCode());
          if (image.isMarkForDelete()) {
            this.updatedProductHistoryService
                .createProductL3AuditLog(businessPartnerCode, itemSku, productSku, itemSkuItemNameMap.get(itemSku),
                    deleteImageActivity, image.getLocationPath(), DELETE, false, StringUtils.EMPTY);
          } else {
            this.updatedProductHistoryService
                .createProductL3AuditLog(businessPartnerCode, itemSku, productSku, itemSkuItemNameMap.get(itemSku),
                    updateImageActivity,
                    (MAIN_IMAGE + COLONE + !image.isMainImages() + COLONE + image.getLocationPath()),
                    (MAIN_IMAGE + COLONE + image.isMainImages() + COLONE + image.getLocationPath()), false,
                    StringUtils.EMPTY);
          }
        }
      }
    }
  }

  private void createAuditLogs(String businessPartnerCode,
      List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> productImageEditRequests,
      String productSku, boolean needCorrection, boolean isImageUpdated) throws Exception {
    String newImageActivity = needCorrection ?
        (UpdateProductActivity.IMAGE_ADDED.getDesc() + Constants.NEED_REVISION) :
        UpdateProductActivity.IMAGE_ADDED.getDesc();
    String deleteImageActivity = needCorrection ?
        (UpdateProductActivity.IMAGE_DELETED.getDesc() + Constants.NEED_REVISION) :
        UpdateProductActivity.IMAGE_DELETED.getDesc();
    String updateImageActivity = needCorrection ?
        (UpdateProductActivity.IMAGE_UPDATED.getDesc() + Constants.NEED_REVISION) :
        UpdateProductActivity.IMAGE_UPDATED.getDesc();
    if (CollectionUtils.isNotEmpty(productImageEditRequests)) {
      String productName =
          productLevel3Repository.getProductNameByProductSku(Arrays.asList(productSku)).get(productSku);
      for (com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest productImageEditRequest : productImageEditRequests) {
        if (productImageEditRequest.getCopyToAllVariantImages().isAdd()) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT, productSku,
              productName, newImageActivity, StringUtils.EMPTY, productImageEditRequest.getImagePath(), needCorrection,
              StringUtils.EMPTY);
        }
        if (productImageEditRequest.getCopyToAllVariantImages().isMarkForDelete()) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT, productSku,
              productName, deleteImageActivity, StringUtils.EMPTY, productImageEditRequest.getImagePath(),
              needCorrection, StringUtils.EMPTY);
        }
        if (isImageUpdated && productImageEditRequest.getCopyToAllVariantImages().isMainImage()) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT, productSku,
              productName, updateImageActivity, (MAIN_IMAGE + COLONE + Boolean.FALSE + COLONE + productImageEditRequest.getImagePath()),
              (MAIN_IMAGE + COLONE + Boolean.TRUE + COLONE + productImageEditRequest.getImagePath()), needCorrection, StringUtils.EMPTY);
        }
      }
    }
  }

  @Override
  public void updateSolrProductCollectionDocument(ProductCollection productCollection) {
    if (productCollection.isActivated() && productCollection.isViewable()) {
      SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();
      BeanUtils.copyProperties(productCollection, solrProductCollectionDTO);
      SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
          new SolrProductCollectionUpdateEvent(solrProductCollectionDTO);
      kafkaProducer.send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE,
          solrProductCollectionUpdateEvent.getSolrProductCollectionDTO().getProductCode(),
          solrProductCollectionUpdateEvent);
    }
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public ProductCollection updateProductCollection(ProductCollection productCollection) {
    productCollection.setActivated(true);
    productCollection.setViewable(true);
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    productCollection.setState("ACTIVE");
    productCollectionRepository.save(productCollection);
    return productCollection;
  }

  private void generateProductBusinessPartnerFromProductPriceStockAndImagesRequest(
      ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest,
      ProductItemBusinessPartner productItemBusinessPartner) {
    if (CollectionUtils.isNotEmpty(productPriceStockAndImagesRequest.getViewConfigs())) {
      productItemBusinessPartner
          .setBuyable(productPriceStockAndImagesRequest.getViewConfigs().stream().findFirst().get().getBuyable());
      productItemBusinessPartner
          .setDisplay(productPriceStockAndImagesRequest.getViewConfigs().stream().findFirst().get().getDisplay());
    }
    if (CollectionUtils.isNotEmpty(productPriceStockAndImagesRequest.getPrices())) {
      productItemBusinessPartner
          .setPrice(productPriceStockAndImagesRequest.getPrices().stream().findFirst().get().getPrice());
      productItemBusinessPartner
          .setSalePrice(productPriceStockAndImagesRequest.getPrices().stream().findFirst().get().getSalePrice());
      productItemBusinessPartner.setSaleStartDate(
          productPriceStockAndImagesRequest.getPrices().stream().findFirst().get().getDiscountStartDate());
      productItemBusinessPartner.setSaleEndDate(
          productPriceStockAndImagesRequest.getPrices().stream().findFirst().get().getDiscountEndDate());
    }
    if (!StringUtils
        .equals(productPriceStockAndImagesRequest.getMerchantSku(), productItemBusinessPartner.getMerchantSku())) {
      productItemBusinessPartner.setMerchantSku(productPriceStockAndImagesRequest.getMerchantSku());
    }
  }

  private WholesalePriceBulkUpdateResponse updateBulkWholesalePriceInPricing(
      Map<String, ItemSummaryResponse> savedProductDataMap,
      List<ProductPriceStockAndImagesRequest> successValidationVariantList,
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap, String merchantCode, String storeId,
      Map<String, Double> offerPriceMap)
      throws Exception {
    List<WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequestList = new ArrayList<>();
    Map<String, WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequestMap = new HashMap<>();
    Map<String, WholesalePriceSkuResponse> currentWholesalePriceSkuResponseMap = new HashMap<>();
    Map<String, String> itemSkuItemNameMap = new HashMap<>();
    for (ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      if (CollectionUtils.isNotEmpty(productPriceStockAndImagesRequest.getProductItemWholesalePrices())) {
        WholesalePriceSkuResponse wholesalePriceSkuResponse =
            wholesalePriceSkuResponseMap.get(productPriceStockAndImagesRequest.getItemSku());
        WholesalePriceBulkUpdateRequest newWholesalePriceRequest = ConverterUtil
            .toWholesalePriceBulkUpdateRequest(productPriceStockAndImagesRequest,
                savedProductDataMap.get(productPriceStockAndImagesRequest.getItemSku()));
        if (!newWholesalePriceRequest.getWholesalePriceSkuRequests().get(0).getWholesaleRules()
            .equals(wholesalePriceSkuResponse.getWholesaleRules())) {
          wholesalePriceBulkUpdateRequestList.add(newWholesalePriceRequest);
          wholesalePriceBulkUpdateRequestMap
              .put(productPriceStockAndImagesRequest.getItemSku(), newWholesalePriceRequest);
          currentWholesalePriceSkuResponseMap
              .put(productPriceStockAndImagesRequest.getItemSku(), wholesalePriceSkuResponse);
          itemSkuItemNameMap
              .put(productPriceStockAndImagesRequest.getItemSku(), productPriceStockAndImagesRequest.getItemName());
        }
      }
    }
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = new WholesalePriceBulkUpdateResponse();
    if (CollectionUtils.isNotEmpty(wholesalePriceBulkUpdateRequestList)) {
      wholesalePriceBulkUpdateResponse =
          this.productPricingOutbound.bulkUpdateWholesalePrice(wholesalePriceBulkUpdateRequestList, merchantCode);
      wholesalePriceBulkUpdateResponse.setWholesalePriceSkuStatusMap(
          wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatus().stream().collect(
              Collectors.toMap(WholeSalePriceSkuStatusDto::getItemSku, WholeSalePriceSkuStatusDto::getSkuStatus,
                  (v1, v2) -> v2)));
      if (Objects.nonNull(wholesalePriceBulkUpdateResponse)) {
        //Wholesale history update
        for (WholesalePriceBulkUpdateRequest newWholesalePriceBulkUpdateRequest : wholesalePriceBulkUpdateRequestList) {
          String itemSku =
              newWholesalePriceBulkUpdateRequest.getWholesalePriceSkuRequests().stream().findFirst().get().getItemSku();
          Double offerPrice = offerPriceMap.get(itemSku);
          boolean wholesaleRulesUpdated =
              wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatusMap().containsKey(itemSku);
          generateHistoryForWholesaleUpdate(itemSku, wholesaleRulesUpdated, newWholesalePriceBulkUpdateRequest,
              currentWholesalePriceSkuResponseMap.get(itemSku), offerPrice, merchantCode,
              newWholesalePriceBulkUpdateRequest.getProductSku(), itemSkuItemNameMap.get(itemSku), StringUtils.EMPTY);
        }
      }
    }
    return wholesalePriceBulkUpdateResponse;
  }

  private WholesalePriceBulkUpdateResponse updateBulkWholesalePriceInPricingL5(
      Map<String, ItemPickupPointListingResponse> savedProductDataMap,
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap, String merchantCode,
      Map<String, Double> offerPriceMap, ProductVariantUpdateRequest productVariantUpdateRequest,
      List<VariantsErrorListResponse> failedRequests, Map<String, String> itemSkuAndPickupCodeMapping,
      Set<String> wholeSaleFlagUpdatedL5s, List<ItemPickupPointQuickEditRequest> modifiedItemPickupPoints,
      Map<String, String> itemSkuToItemCodeMap, Map<String, Boolean> addingWholeSale1stTimeL5s,
    ProfileResponse profileResponse, List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests) throws Exception {
    List<WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequestList = new ArrayList<>();
    Map<String, WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequestMap = new HashMap<>();
    Map<String, WholesalePriceSkuResponse> currentWholesalePriceSkuResponseMap = new HashMap<>();
    Map<String, String> itemSkuItemNameMap = new HashMap<>();
    List<BulkActivateDeactivateRequest> bulkActivateDeactivateRequests = new ArrayList<>();
    for (ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest : productVariantUpdateRequest
        .getProductItems()) {
      for (ItemPickupPointRequest itemPickupPointRequest : productPriceStockAndImagesRequest
          .getModifiedItemPickupPoints()) {
        String L5Code = productPriceStockAndImagesRequest.getItemSku() + Constants.HYPHEN + itemPickupPointRequest
            .getPickupPointId();
        if (Objects.nonNull(itemPickupPointRequest.getWholesalePriceActivated()) && CollectionUtils
            .isNotEmpty(itemPickupPointRequest.getProductItemWholesalePriceRequests())) {
          WholesalePriceSkuResponse wholesalePriceSkuResponse =
              wholesalePriceSkuResponseMap.getOrDefault(L5Code, new WholesalePriceSkuResponse());
          boolean wholesaleFlagChanged =
              isWholesaleFlagChanged(wholesalePriceSkuResponse, itemPickupPointRequest.getWholesalePriceActivated());
          if (StringUtils.isEmpty(wholesalePriceSkuResponse.getSkuStatus())) {
            addingWholeSale1stTimeL5s.putIfAbsent(L5Code, itemPickupPointRequest.getWholesalePriceActivated());
          } else if (CommonUtils
              .isWholesaleFlagChanged(wholesalePriceSkuResponse, itemPickupPointRequest.getWholesalePriceActivated())) {
            wholeSaleFlagUpdatedL5s.add(L5Code);
          }
          WholesalePriceBulkUpdateRequest newWholesalePriceRequest = ConverterUtil
              .toWholesalePriceBulkUpdateRequestL5(productVariantUpdateRequest.getProductSku(),
                  itemSkuToItemCodeMap.get(itemPickupPointRequest.getItemSku()),
                productPriceStockAndImagesRequest.getItemSku(),
                  itemPickupPointRequest.getPickupPointId(), itemPickupPointRequest.getWholesalePriceActivated(),
                  itemPickupPointRequest.getProductItemWholesalePriceRequests(),
                  wholesaleFlagChanged);
          if (!newWholesalePriceRequest.getWholesalePriceSkuRequests().get(0).getWholesaleRules()
              .equals(wholesalePriceSkuResponse.getWholesaleRules())) {
            wholesalePriceBulkUpdateRequestMap.putIfAbsent(
                itemPickupPointRequest.getItemSku() + Constants.HYPHEN + itemPickupPointRequest.getPickupPointId(),
                newWholesalePriceRequest);
            wholesalePriceBulkUpdateRequestList.add(newWholesalePriceRequest);
            currentWholesalePriceSkuResponseMap.put(L5Code, wholesalePriceSkuResponse);
            itemSkuItemNameMap.put(L5Code, productPriceStockAndImagesRequest.getItemName());
          } else if (wholesaleFlagChanged) {
            BulkActivateDeactivateRequest bulkActivateDeactivateRequest =
                BulkActivateDeactivateRequest.builder().itemSku(itemPickupPointRequest.getItemSku())
                    .pickUpPointCode(itemPickupPointRequest.getPickupPointId()).updateSkuStatus(
                    Boolean.TRUE.equals(itemPickupPointRequest.getWholesalePriceActivated()) ?
                        ACTIVE_STATUS :
                        INACTIVE_STATUS).pickUpPointCode(itemPickupPointRequest.getPickupPointId()).build();
            bulkActivateDeactivateRequests.add(bulkActivateDeactivateRequest);
          }
        }
      }
    }
    for (ItemPickupPointRequest itemPickupPointRequest : productVariantUpdateRequest.getAddPickupPoints()) {
      getWholeSalePriceAdditionRequest(wholesalePriceSkuResponseMap, productVariantUpdateRequest, itemSkuToItemCodeMap,
          wholesalePriceBulkUpdateRequestList, wholesalePriceBulkUpdateRequestMap, currentWholesalePriceSkuResponseMap,
          itemSkuItemNameMap, itemPickupPointRequest);
    }
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : newlyAddedProductItemRequests) {
      for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
        getWholeSalePriceAdditionRequest(wholesalePriceSkuResponseMap, productVariantUpdateRequest,
            itemSkuToItemCodeMap, wholesalePriceBulkUpdateRequestList, wholesalePriceBulkUpdateRequestMap,
            currentWholesalePriceSkuResponseMap, itemSkuItemNameMap, itemPickupPointRequest);
      }
    }
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = new WholesalePriceBulkUpdateResponse();
    Map<String, String> failedPickupPointIdReasonMap = new HashMap<>();
    Map<String, Boolean> wholesalePriceSkuStatusMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(wholesalePriceBulkUpdateRequestList)) {
      wholesalePriceBulkUpdateResponse =
          this.productPricingOutbound.bulkUpdateWholesalePrice(wholesalePriceBulkUpdateRequestList, merchantCode);
      if (Objects.nonNull(wholesalePriceBulkUpdateResponse)) {
        //Wholesale history update
        if (CollectionUtils.isNotEmpty(wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatus())) {
          for (WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto : wholesalePriceBulkUpdateResponse
              .getWholesalePriceSkuStatus()) {
            String pickupPointCode = StringUtils.isBlank(wholeSalePriceSkuStatusDto.getPickUpPointCode()) ?
                itemSkuAndPickupCodeMapping.get(wholeSalePriceSkuStatusDto.getItemSku()) :
                wholeSalePriceSkuStatusDto.getPickUpPointCode();
            String L5Code = wholeSalePriceSkuStatusDto.getItemSku() + Constants.HYPHEN + pickupPointCode;
            generateHistoryForWholesaleUpdate(wholeSalePriceSkuStatusDto.getItemSku(), true,
                wholesalePriceBulkUpdateRequestMap.get(L5Code), currentWholesalePriceSkuResponseMap.get(L5Code),
                offerPriceMap.get(L5Code), merchantCode, productVariantUpdateRequest.getProductSku(),
                itemSkuItemNameMap.get(L5Code), pickupPointCode);
          }
        }
        failedPickupPointIdReasonMap =
            ofNullable(wholesalePriceBulkUpdateResponse.getFailedItemReason()).orElse(new ArrayList<>())
                .stream().collect(Collectors.toMap(
                failedItemReasonDto -> failedItemReasonDto.getItemSku() + Constants.HYPHEN + (StringUtils
                    .isBlank(failedItemReasonDto.getPickUpPointCode()) ?
                    itemSkuAndPickupCodeMapping.get(failedItemReasonDto.getItemSku()) :
                    failedItemReasonDto.getPickUpPointCode()), FailedItemReasonDto::getFailedReason, (a, b) -> a));
        if (MapUtils.isNotEmpty(wholesalePriceBulkUpdateResponse.getFailedItemSkuToFailedReasonMap())) {
          for (Map.Entry<String, String> failedItemSkuToFailedReasonMap : wholesalePriceBulkUpdateResponse
              .getFailedItemSkuToFailedReasonMap().entrySet()) {
            failedPickupPointIdReasonMap.putIfAbsent(
                failedItemSkuToFailedReasonMap.getKey() + Constants.HYPHEN + itemSkuAndPickupCodeMapping
                    .get(failedItemSkuToFailedReasonMap.getKey()), failedItemSkuToFailedReasonMap.getValue());
          }
        }
        wholesalePriceSkuStatusMap =
            ofNullable(wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatus()).orElse(new ArrayList<>())
                .stream().collect(Collectors.toMap(
                wholeSalePriceSkuStatusDto -> wholeSalePriceSkuStatusDto.getItemSku() + Constants.HYPHEN + (StringUtils
                    .isBlank(wholeSalePriceSkuStatusDto.getPickUpPointCode()) ?
                    itemSkuAndPickupCodeMapping.get(wholeSalePriceSkuStatusDto.getItemSku()) :
                    wholeSalePriceSkuStatusDto.getPickUpPointCode()),
                wholeSalePriceSkuStatusDto -> ACTIVE.equals(wholeSalePriceSkuStatusDto.getSkuStatus()), (a, b) -> a));

      }
    }
    BulkActivateDeactivateResponse bulkActivateDeactivateResponse = null;
    if (CollectionUtils.isNotEmpty(bulkActivateDeactivateRequests)) {
      bulkActivateDeactivateResponse =
          productPricingOutbound.bulkActivateOrDeactivateSku(bulkActivateDeactivateRequests);
    }
    Map<String, String> failedPickupPointIdFlagUpdateReasonMap = new HashMap<>();
    if (Objects.nonNull(bulkActivateDeactivateResponse)) {
      failedPickupPointIdFlagUpdateReasonMap =
          ofNullable(bulkActivateDeactivateResponse.getFailedItemReason()).orElse(new ArrayList<>()).stream()
              .collect(Collectors.toMap(
                  failedItemReasonDto -> failedItemReasonDto.getItemSku() + Constants.HYPHEN + (StringUtils
                      .isBlank(failedItemReasonDto.getPickUpPointCode()) ?
                      itemSkuAndPickupCodeMapping.get(failedItemReasonDto.getItemSku()) :
                      failedItemReasonDto.getPickUpPointCode()), FailedItemReasonDto::getFailedReason, (a, b) -> a));
      if (MapUtils.isNotEmpty(bulkActivateDeactivateResponse.getFailedItemSkuToFailedReasonMap())) {
        for (Map.Entry<String, String> failedItemSkuToFailedReasonMap : bulkActivateDeactivateResponse
            .getFailedItemSkuToFailedReasonMap().entrySet()) {
          failedPickupPointIdFlagUpdateReasonMap.putIfAbsent(
              failedItemSkuToFailedReasonMap.getKey() + Constants.HYPHEN + itemSkuAndPickupCodeMapping
                  .get(failedItemSkuToFailedReasonMap.getKey()), failedItemSkuToFailedReasonMap.getValue());
        }
      }
      failedRequests = validateWholesaleResponseForFailedItemSkuL5(bulkActivateDeactivateResponse.getFailedItemReason(),
          failedRequests, savedProductDataMap, ApiErrorCode.WHOLESALE_FLAG_UPDATE_FAILED.getCode(),
          ApiErrorCode.WHOLESALE_FLAG_UPDATE_FAILED.getDesc(), itemSkuAndPickupCodeMapping, profileResponse);
    }
    for (ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest : modifiedItemPickupPoints) {
      String L5Code = itemPickupPointQuickEditRequest.getItemSku() + Constants.HYPHEN + itemPickupPointQuickEditRequest
          .getPickupPointCode();
        if (failedPickupPointIdReasonMap.containsKey(L5Code) || failedPickupPointIdFlagUpdateReasonMap
            .containsKey(L5Code)) {
          itemPickupPointQuickEditRequest.setWholeSaleActivated(false);
        }
        if (wholesalePriceSkuStatusMap.containsKey(L5Code)) {
          itemPickupPointQuickEditRequest.setWholeSaleActivated(wholesalePriceSkuStatusMap.get(L5Code));
        }
      }
    return wholesalePriceBulkUpdateResponse;
  }

  private static void getWholeSalePriceAdditionRequest(Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
      ProductVariantUpdateRequest productVariantUpdateRequest, Map<String, String> itemSkuToItemCodeMap,
      List<WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequestList,
      Map<String, WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequestMap,
      Map<String, WholesalePriceSkuResponse> currentWholesalePriceSkuResponseMap,
      Map<String, String> itemSkuItemNameMap, ItemPickupPointRequest itemPickupPointRequest) {
    String L5Code = getL5Code(itemPickupPointRequest);
    if (Objects.nonNull(itemPickupPointRequest.getWholesalePriceActivated()) && CollectionUtils
        .isNotEmpty(itemPickupPointRequest.getProductItemWholesalePriceRequests())) {
      WholesalePriceSkuResponse wholesalePriceSkuResponse =
          wholesalePriceSkuResponseMap.getOrDefault(L5Code, new WholesalePriceSkuResponse());
      WholesalePriceBulkUpdateRequest newWholesalePriceRequest = ConverterUtil
          .toWholesalePriceBulkUpdateRequestL5(productVariantUpdateRequest.getProductSku(),
              itemSkuToItemCodeMap.get(itemPickupPointRequest.getItemSku()), itemPickupPointRequest.getItemSku(),
              itemPickupPointRequest.getPickupPointId(), itemPickupPointRequest.getWholesalePriceActivated(),
              itemPickupPointRequest.getProductItemWholesalePriceRequests(), true);
      wholesalePriceBulkUpdateRequestMap.putIfAbsent(
          itemPickupPointRequest.getItemSku() + Constants.HYPHEN + itemPickupPointRequest.getPickupPointId(),
          newWholesalePriceRequest);
      wholesalePriceBulkUpdateRequestList.add(newWholesalePriceRequest);
      currentWholesalePriceSkuResponseMap.put(L5Code, wholesalePriceSkuResponse);
      itemSkuItemNameMap.put(L5Code, itemSkuItemNameMap.get(itemPickupPointRequest.getItemSku()));
    }
  }

  private boolean isWholesaleFlagChanged(WholesalePriceSkuResponse wholesalePriceSkuResponse, Boolean wholesalePriceActivated) {
    return (ACTIVE_STATUS.equals(wholesalePriceSkuResponse.getSkuStatus()) && Boolean.FALSE
        .equals(wholesalePriceActivated)) || (INACTIVE_STATUS.equals(wholesalePriceSkuResponse.getSkuStatus())
        && Boolean.TRUE.equals(wholesalePriceActivated)) || StringUtils
        .isEmpty(wholesalePriceSkuResponse.getSkuStatus());
  }

  private void generateHistoryForWholesaleUpdate(String itemSku, boolean wholesaleRulesUpdated,
      WholesalePriceBulkUpdateRequest newWholesalePriceBulkUpdateRequest,
      WholesalePriceSkuResponse currentWholesalePriceSkuResponse, Double offerPrice, String merchantCode,
      String productSku, String name, String pickupPointCode) throws Exception {
    if (wholesaleRulesUpdated) {
      List<ProductItemHistoryForWholesale> productItemHistoryForWholesaleList = new ArrayList<>();
      ofNullable(newWholesalePriceBulkUpdateRequest.getWholesalePriceSkuRequests().get(0).getWholesaleRules())
          .orElse(new HashMap<>()).forEach((key, value) -> {
        productItemHistoryForWholesaleList.add(
            new ProductItemHistoryForWholesale(key, value, Math.floor((offerPrice * (1 - value / 100) * 1000)) / 1000));
      });
      String newWholesaleRules = this.mapperUtil.mapRequestToString(productItemHistoryForWholesaleList);
      productItemHistoryForWholesaleList.clear();
      ofNullable(currentWholesalePriceSkuResponse.getWholesaleRules()).orElse(new HashMap<>())
          .forEach((key, value) -> {
            productItemHistoryForWholesaleList.add(new ProductItemHistoryForWholesale(key, value,
                Math.floor((offerPrice * (1 - value / 100) * 1000)) / 1000));
          });
      String oldWholesaleRules = this.mapperUtil.mapRequestToString(productItemHistoryForWholesaleList);
      updatedProductHistoryService.createProductL3AuditLog(merchantCode, itemSku, productSku, name,
          UpdateProductActivity.UPDATE_WHOLE_SALE_RULES.getDesc(), oldWholesaleRules, newWholesaleRules, false,
          pickupPointCode);
    }
  }

  private Map<String, ProductItemWholesalePrice> findItemWholesalePriceByStoreIdAndItemSkus(String storeId,
      List<String> itemSkus) {
    List<ProductItemWholesalePrice> productItemWholesalePrices = new ArrayList<>();
    Map<String, ProductItemWholesalePrice> productItemWholesalePricesMap = new HashMap<>();
    List<List<String>> itemSkuList = Lists.partition(itemSkus, 10);
    for (List<String> itemSkuSubList : itemSkuList) {
      List<ProductItemWholesalePrice> wholesalePrices =
          productItemWholesalePriceRepository.findByStoreIdAndItemSkuIn(storeId, itemSkuSubList);
      if (CollectionUtils.isNotEmpty(wholesalePrices)) {
        productItemWholesalePrices.addAll(wholesalePrices);
      }
    }
    if (CollectionUtils.isNotEmpty(productItemWholesalePrices)) {
      productItemWholesalePricesMap = productItemWholesalePrices.stream()
          .collect(Collectors.toMap(ProductItemWholesalePrice::getItemSku, Function.identity()));
    }
    return productItemWholesalePricesMap;
  }


  private BulkActivateDeactivateResponse updateBulkWholesaleFlagInPricing(Map<String, ItemSummaryResponse> savedProductDataList,
      List<ProductPriceStockAndImagesRequest> successValidationVariantList,
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap, WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse) throws Exception {
    List<BulkActivateDeactivateRequest> bulkActivateDeactivateRequestList = new ArrayList<>();
    Map<String, String> itemSkuItemNameMap = new HashMap<>();
    for (ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      WholesalePriceSkuResponse wholesalePriceSkuResponse =
          wholesalePriceSkuResponseMap.get(productPriceStockAndImagesRequest.getItemSku());
      Boolean wholesaleFlagValue = null;
      if (MapUtils.isNotEmpty(wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatusMap())) {
        wholesaleFlagValue =
          Constants.ACTIVE.equals(wholesalePriceBulkUpdateResponse.getWholesalePriceSkuStatusMap()
            .get(productPriceStockAndImagesRequest.getItemSku()));
      }
      if (Objects.nonNull(wholesalePriceSkuResponse) && (
          checkIsStatusChanged(productPriceStockAndImagesRequest.getWholesalePriceActivated(),
              wholesalePriceSkuResponse.getSkuStatus()) || (
              StringUtils.isEmpty(wholesalePriceSkuResponse.getSkuStatus()) && Objects
                  .nonNull(productPriceStockAndImagesRequest.getWholesalePriceActivated())
                  && !productPriceStockAndImagesRequest.getWholesalePriceActivated())) && !CommonUtils.checkBooleanEquals(
          wholesaleFlagValue, productPriceStockAndImagesRequest.getWholesalePriceActivated())) {
        BulkActivateDeactivateRequest bulkActivateDeactivateRequest =
            BulkActivateDeactivateRequest.builder().itemSku(productPriceStockAndImagesRequest.getItemSku())
                .updateSkuStatus(Boolean.TRUE.equals(productPriceStockAndImagesRequest.getWholesalePriceActivated()) ?
                    ACTIVE_STATUS : INACTIVE_STATUS).pickUpPointCode(
                savedProductDataList.get(productPriceStockAndImagesRequest.getItemSku()).getPickupPointCode()).build();
        bulkActivateDeactivateRequestList.add(bulkActivateDeactivateRequest);
        itemSkuItemNameMap
            .put(productPriceStockAndImagesRequest.getItemSku(), productPriceStockAndImagesRequest.getItemName());
      }
    }
    BulkActivateDeactivateResponse bulkActivateDeactivateResponse = new BulkActivateDeactivateResponse();
    if (CollectionUtils.isNotEmpty(bulkActivateDeactivateRequestList)) {
      bulkActivateDeactivateResponse =
          productPricingOutbound.bulkActivateOrDeactivateSku(bulkActivateDeactivateRequestList);
    }
    return bulkActivateDeactivateResponse;
  }

  private UpdateItemSummaryRequest generateProductLevel3UpdateSummaryRequestFromProductPriceStockAndImagesRequest(
      ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest, ItemSummaryResponse savedProductData) {
    UpdateItemSummaryRequest productLevel3UpdateSummaryRequest = new UpdateItemSummaryRequest();
    productLevel3UpdateSummaryRequest.setMerchantSku(productPriceStockAndImagesRequest.getMerchantSku());
    productLevel3UpdateSummaryRequest.setPickupPointCode(savedProductData.getPickupPointCode());
    productLevel3UpdateSummaryRequest.setIsLateFulfillment(savedProductData.isLateFulfillment());
    productLevel3UpdateSummaryRequest.setPrice(new HashSet<>());
    productLevel3UpdateSummaryRequest.setItemViewConfigs(new HashSet<>());
    for (ProductLevel3PriceRequest productLevel3Price : productPriceStockAndImagesRequest.getPrices()) {
      PriceDTO price = new PriceDTO();
      price.setOfferPrice(productLevel3Price.getSalePrice());
      price.setListPrice(productLevel3Price.getPrice());
      price.setChannel(productLevel3Price.getChannelId());
      productLevel3UpdateSummaryRequest.getPrice().add(price);
    }
    for (ProductLevel3ViewConfigRequest productLevel3ViewConfig : productPriceStockAndImagesRequest.getViewConfigs()) {
      ItemViewConfigDTO viewConfig = new ItemViewConfigDTO();
      viewConfig.setBuyable(productLevel3ViewConfig.getBuyable());
      viewConfig.setDiscoverable(productLevel3ViewConfig.getDisplay());
      viewConfig.setChannel(productLevel3ViewConfig.getChannelId());
      productLevel3UpdateSummaryRequest.getItemViewConfigs().add(viewConfig);
    }
    productLevel3UpdateSummaryRequest.setOff2OnChannelActive(productPriceStockAndImagesRequest.getOff2OnActiveFlag());
    return productLevel3UpdateSummaryRequest;
  }

  @Override
  public Page<ProductLevel3SummaryDetails> findSummaryDetailsByFilter(String storeId, ProductLevel3SummaryFilterDetails filterRequest,
      Pageable pageRequest) throws Exception {
    ItemsSummaryDetailRequest itemFilterRequest =
        modelConverter.convertProductLevel3SummaryDetailsRequestToItemSummaryRequest(filterRequest);
    //PreLive/postLive in need revision
    if (filterRequest.isNeedCorrection()) {
      return getProductLevel3SummaryDetailsNeedCorrection(storeId, filterRequest, pageRequest);
    }
    Page<ItemSummaryDetailResponse> productDatas =
        xProductOutbound.findSummaryDetailsByFilter(itemFilterRequest, pageRequest);
    List<String> gdnSkus =
        productDatas.getContent().stream().map(ItemSummaryDetailResponse::getItemSku).collect(Collectors.toList());
    List<ProductLevel3Inventory> inventoryList = productLevel3InventoryService
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(), gdnSkus);
    Map<String, ProductLevel3Inventory> inventoryDatas =
        modelConverter.convertProductLevel3InventoryToMapOfGdnSku(inventoryList);
    Map<String, String> itemSkuAndPickupCodeMap = productDatas.getContent().stream().collect(Collectors
        .toMap(ItemSummaryDetailResponse::getItemSku, ItemSummaryDetailResponse::getPickupPointCode, (a, b) -> a));
    Map<String, Double> l5AndOfferPriceMap = productDatas.getContent().stream().collect(Collectors
        .toMap(item -> CommonUtils.toL5Id(item.getItemSku(), item.getPickupPointCode()),
            item -> item.getPrice().stream().findFirst().orElse(new PriceDTO()).getOfferPrice(), (a, b) -> a));
    Set<String> itemSkus =
        productDatas.getContent().stream().map(ItemSummaryDetailResponse::getItemSku).collect(Collectors.toSet());
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap = new HashMap<>();
    List<WholesalePriceSkuResponse> wholesalePriceSkuResponseList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      wholesalePriceSkuResponseList.addAll(this.productPricingOutbound.getWholesalePriceList(itemSkus, itemSkuAndPickupCodeMap));
      wholesalePriceSkuResponseMap = wholesalePriceSkuResponseList.stream()
          .collect(Collectors.toMap(WholesalePriceSkuResponse::getItemSku, Function.identity()));
    }
    List<CategoryResponse> categoriesData = new ArrayList<>();
    String categoryCode = EMPTY_STRING;
    if (CollectionUtils.isNotEmpty(productDatas.getContent())) {
      ProductCollection productCollection = this.productCollectionRepository
          .findByStoreIdAndProductCode(DEFAULT_STORE_ID, productDatas.getContent().get(0).getProductCode());
      categoryCode = productCollection.getCategoryCode();
      categoriesData = this.categoryRepository.findHierarchyByCategoryCode(categoryCode);
    }
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap =
        getItemSkuCampaignPriceResponseMap(gdnSkus, categoryCode, itemSkuAndPickupCodeMap, l5AndOfferPriceMap);
    List<String> itemCodes =
        productDatas.getContent().stream().map(ItemSummaryDetailResponse::getItemCode).collect(Collectors.toList());
    Map<String, ProductItemsCogs> productItemsCogsMap = getProductItemCogsValues(filterRequest.getBusinessPartnerCode(), itemCodes);
    Map<String, ProductItemResponse> masterDataProductItemMap = new HashMap<>();
    try {
      masterDataProductItemMap = getMasterDataProductItemInfo(itemCodes);
    } catch (Exception e) {
     LOGGER.warn("Enable to get masterData for item codes : {} ", itemCodes, e);
    }
    return constructProductLevel3SummaryDetails(productDatas, inventoryDatas, wholesalePriceSkuResponseMap, pageRequest,
        categoriesData, campaignPriceSkuResponseMap, productItemsCogsMap, masterDataProductItemMap, categoryCode);
  }

  private Page<ProductLevel3SummaryDetails> getProductLevel3SummaryDetailsNeedCorrection(String storeId,
      ProductLevel3SummaryFilterDetails filterRequest, Pageable pageRequest) throws Exception {
    LOGGER.info("getProductLevel3SummaryDetailsNeedCorrection ProductLevel3SummaryFilterDetails : {}, Pageable : {}",
        filterRequest, pageRequest);
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerRepository.findFirstByGdnProductSku(filterRequest.getProductSku());
    if (Objects.isNull(productBusinessPartner)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "productSku not found");
    }
    Page<ProductItemBusinessPartner> productItemBusinessPartnerList =
        this.productItemBusinessPartnerRepository.findByStoreIdAndProductBusinessPartnerIdAndMarkForDeleteFalseOrderByGdnProductItemSku(
            storeId, productBusinessPartner.getId(), pageRequest);
    Map<String, ProductItemBusinessPartner> productLevel3ItemWipResponseMap =
        productItemBusinessPartnerList.getContent().stream()
            .collect(Collectors.toMap(ProductItemBusinessPartner::getGdnProductItemSku, Function.identity()));
    List<String> gdnSkus = productLevel3ItemWipResponseMap.keySet().stream().collect(Collectors.toList());
    Map<String, String> itemSkuAndPickupCodeMap = productItemBusinessPartnerList.getContent().stream().collect(
        Collectors.toMap(ProductItemBusinessPartner::getGdnProductItemSku, ProductItemBusinessPartner::getPickupPointId,
            (a, b) -> a));
    Map<String, Double> l5AndOfferPriceMap = productItemBusinessPartnerList.getContent().stream().collect(
        Collectors.toMap(item -> CommonUtils.toL5Id(item.getGdnProductItemSku(), item.getPickupPointId()),
            ProductItemBusinessPartner::getSalePrice, (a, b) -> a));
    ProductCollection productCollection = this.productCollectionRepository
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, filterRequest.getProductCode());
    Map<String, ProductLevel3Inventory> inventoryData = new HashMap<>();
    if (productCollection.isPostLive() || (!productCollection.isPostLive() && productCollection.isEdited())) {
      List<ProductLevel3Inventory> inventoryList =
          productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
              filterRequest.getBusinessPartnerCode(), gdnSkus);
      inventoryData = modelConverter.convertProductLevel3InventoryToMapOfGdnSku(inventoryList);
    }
    List<ProductItemWholesalePrice> wholesalePriceList = findByStoreIdAndItemSkus(storeId, gdnSkus);
    Map<String, ProductItemWholesalePrice> wholesalePriceSkuResponseMap = wholesalePriceList.stream()
        .collect(Collectors.toMap(ProductItemWholesalePrice::getItemSku, Function.identity()));
    Product product = this.productRepository.findOne(productCollection.getProductId());
    List<CategoryResponse> categoriesData = new ArrayList<>();
    String categoryCode = EMPTY_STRING;
    if (CollectionUtils.isNotEmpty(product.getProductCategories()) && Objects
        .nonNull(product.getProductCategories().stream().findFirst().get().getCategory())) {
      categoryCode = product.getProductCategories().stream().findFirst().get().getCategory().getCategoryCode();
      categoriesData = this.categoryRepository.findHierarchyByCategoryCode(categoryCode);
    }
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap =
        getItemSkuCampaignPriceResponseMap(gdnSkus, categoryCode, itemSkuAndPickupCodeMap, l5AndOfferPriceMap);
    VendorNotesResponse VendorNotesResponse =
        productServiceWrapper.getVendorNotes(storeId, filterRequest.getProductCode());
    return constructProductLevel3SummaryDetails(wholesalePriceSkuResponseMap, pageRequest, categoriesData, product,
        productLevel3ItemWipResponseMap, categoryCode, filterRequest.getProductSku(), productCollection, inventoryData,
        campaignPriceSkuResponseMap, productItemBusinessPartnerList.getTotalElements(), VendorNotesResponse);
  }

  private Page<ProductLevel3SummaryDetails> constructProductLevel3SummaryDetails(
      Map<String, ProductItemWholesalePrice> wholesalePriceSkuResponseMap, Pageable pageRequest,
      List<CategoryResponse> categories, Product masterDataProduct,
      Map<String, ProductItemBusinessPartner> productLevel3ItemWipResponseMap, String categoryCode, String productSku,
      ProductCollection productCollection, Map<String, ProductLevel3Inventory> inventoryData,
      Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap, long totalElements,
      VendorNotesResponse vendorNotesResponse) throws Exception {
    List<ProductLevel3SummaryDetails> products = new ArrayList<>();
    List<ProductLevel3SummaryDetails> needCorrectionProduct = new ArrayList<>();
    Map<String, ProductItem> productItemMap =
        masterDataProduct.getProductItems().stream().collect(Collectors.toMap(ProductItem::getId, Function.identity()));
    Map<String, ItemNotesDto> vendorItemNotesMap = new HashMap<>();
    if (Objects.nonNull(vendorNotesResponse)) {
      vendorItemNotesMap = ofNullable(vendorNotesResponse.getItemNotes()).orElse(new ArrayList<>()).stream()
          .collect(Collectors.toMap(ItemNotesDto::getItemSku, Function.identity()));
    }
    for (String itemSku : productLevel3ItemWipResponseMap.keySet()) {
      ProductItemBusinessPartner productData = productLevel3ItemWipResponseMap.get(itemSku);
      ProductItem productItem = productItemMap.get(productData.getProductItemId());
      ProductLevel3SummaryDetails product = new ProductLevel3SummaryDetails();
      product.setItemSku(itemSku);
      product.setSkuCode(productItem.getSkuCode());
      product.setMerchantSku(productData.getMerchantSku());
      product.setMerchantCode(productCollection.getBusinessPartnerCode());
      product.setItemName(productItem.getGeneratedItemName());
      product.setCreatedDate(productData.getCreatedDate());
      product.setCreatedBy(productData.getCreatedBy());
      product.setUpdatedDate(productData.getUpdatedDate());
      product.setUpdatedBy(productData.getUpdatedBy());
      product.setVersion(productData.getVersion());
      product.setIsArchived(Boolean.FALSE);
      product.setProductType(productData.getProductType());
      product.setLateFulfillment(Boolean.FALSE);
      if (MapUtils.isNotEmpty(inventoryData) && inventoryData.containsKey(itemSku)) {
        ProductLevel3Inventory inventory = inventoryData.get(itemSku);
        product.setAvailableStockLevel1(inventory.getWarehouseAvailable());
        product.setReservedStockLevel1(inventory.getWarehouseReserved());
        product.setReservedStockLevel2(inventory.getWebReserved());
        product.setAvailableStockLevel2(inventory.getWebAvailable());
        product.setMinimumStockLevel2(inventory.getWebMinAlert());
        product.setSynchronizeStock(inventory.isWebSyncStock());
        product.setNonDistributionAvailable(inventory.getNonDistributionAvailable());
        product.setNonDistributionReserved(inventory.getNonDistributionReserved());
      } else {
        product.setAvailableStockLevel1(Constants.MINIMUM_STOCK);
        product.setReservedStockLevel1(Constants.MINIMUM_STOCK);
        product.setAvailableStockLevel2(productData.getStock());
        product.setReservedStockLevel2(Constants.MINIMUM_STOCK);
        product.setSynchronizeStock(Boolean.FALSE);
        product.setMinimumStockLevel2(productData.getMinimumStock());
      }
      product.setOff2OnActiveFlag(Boolean.FALSE);
      product.setProductCode(masterDataProduct.getProductCode());
      product.setProductSku(productSku);
      List<ProductLevel3Price> productPrices;
      PriceDTO priceDTO = new PriceDTO();
      priceDTO.setListPrice(productData.getPrice());
      priceDTO.setOfferPrice(productData.getSalePrice());
      priceDTO.setChannel(Constants.DEFAULT);
      productPrices = modelConverter.convertItemPricesToProductLevel3Prices(Arrays.asList(priceDTO));
      List<ProductLevel3ViewConfig> productViewConfigs;
      ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
      itemViewConfigDTO.setDiscoverable(productData.isDisplay());
      itemViewConfigDTO.setBuyable(productData.isBuyable());
      itemViewConfigDTO.setChannel(Constants.DEFAULT);
      productViewConfigs = modelConverter
          .convertItemViewConfigsToProductLevel3ViewConfigs(new HashSet<>(Arrays.asList(itemViewConfigDTO)));
      List<ProductLevel3SummaryDetailsImage> productImages =
          ofNullable(masterDataProduct.getProductImages()).orElse(new ArrayList<>()).stream()
              .map(image -> modelConverter.convertMasterDataProductImagesToProductLevel3SummaryDetailsImage(image))
              .collect(Collectors.toList());
      if (Objects.nonNull(wholesalePriceSkuResponseMap.get(itemSku)) && !wholesalePriceSkuResponseMap.get(itemSku)
          .getWholesaleRules().isEmpty()) {
        ProductItemWholesalePrice wholesalePriceSkuResponse = wholesalePriceSkuResponseMap.get(itemSku);
        product.setWholesalePriceActivated(wholesalePriceSkuResponse.isWholesalePriceActivated());
        List<ProductItemWholesalePriceResponse> wholesalePriceResponseList =
            mapperUtil.mapStringToResponse(wholesalePriceSkuResponse.getWholesaleRules());
        product.setProductItemWholesalePrices(wholesalePriceResponseList.stream().map(
            wholesalePriceResponse -> new ProductItemWholesalePriceVo(wholesalePriceResponse.getQuantity(),
                wholesalePriceResponse.getWholesaleDiscount())).collect(Collectors.toList()));
      } else {
        product.setWholesalePriceActivated(Boolean.FALSE);
      }
      if (CollectionUtils.isNotEmpty(categories)) {
        String[] categoryNameAndHierarchy = generateCategoryNameIdAndHierarchy(categories);
        product.setCategoryName(categoryNameAndHierarchy[0]);
        product.setCategoryHierarchy(categoryNameAndHierarchy[1]);
        product.setCategoryId(categoryNameAndHierarchy[2]);
        product.setWholesalePriceConfigEnabled(categories.get(0).isWholesalePriceConfigEnabled());
      }
      if (MapUtils.isNotEmpty(campaignPriceSkuResponseMap) && campaignPriceSkuResponseMap
          .containsKey(itemSku)) {
        CampaignPriceSkuResponse campaignPriceSkuResponse = campaignPriceSkuResponseMap.get(itemSku);
        product.setItemCampaignMapped(campaignPriceSkuResponse.isRegistered());
        product.setItemCampaignActivated(campaignPriceSkuResponse.isLive());
        product.setCampaignCurrentPrice(campaignPriceSkuResponse.getCampaignPrice());
        product.setCampaignMinPrice(campaignPriceSkuResponse.getMinAllowedPrice());
        product.setCampaignMaxPrice(campaignPriceSkuResponse.getMaxAllowedPrice());
      }
      product.setCategoryCode(categoryCode);
      product.setPrices(productPrices);
      product.setViewConfigs(productViewConfigs);
      product.setImages(productImages);
      product.setPromoBundling(Boolean.FALSE);
      product.setMerchantPromoDiscount(Boolean.FALSE);
      product.setMerchantPromoDiscountActivated(Boolean.FALSE);
      product.setPromoTypes(new ArrayList<>());
      product.setForceReview(Boolean.FALSE);
      product.setFlashSaleActive(Boolean.FALSE);
      product.setRejected(productData.isMarkForDelete());
      product.setSuspended(Boolean.FALSE);
      product.setUpcCode(productItem.getUpcCode());
      product.setImages(
          constructProductLevel3SummaryDetailsImageFromProductItemImages(productItem.getProductItemImages()));
      product.getImages().sort(Comparator.comparingInt(ProductLevel3SummaryDetailsImage::getSequence));
      product.setPriceEditDisabled(Boolean.FALSE);
      product.setPriceEditDisabledReason(StringUtils.EMPTY);
      product.setItemNumber(StringUtils.substringAfterLast(itemSku,SKU_SEPARATOR).replaceFirst(
          LEADINGZERO, StringUtils.EMPTY));
      if (vendorItemNotesMap.containsKey(itemSku)) {
        needCorrectionProduct.add(product);
      } else {
        products.add(product);
      }
    }
    products = getProductSortedBasedOnNeedCorrection(products, needCorrectionProduct);
    LOGGER.info("Total products in page : {} are {}", pageRequest, products.size());
    return new PageImpl<>(products, pageRequest, totalElements);
  }

  private List<ProductLevel3SummaryDetails> getProductSortedBasedOnNeedCorrection(
      List<ProductLevel3SummaryDetails> products, List<ProductLevel3SummaryDetails> needCorrectionProduct) {
    if (CollectionUtils.isNotEmpty(needCorrectionProduct)) {
      needCorrectionProduct =
          needCorrectionProduct.stream().sorted(Comparator.comparing(ProductLevel3SummaryDetails::getItemSku))
              .collect(Collectors.toList());
      products = products.stream().sorted(Comparator.comparing(ProductLevel3SummaryDetails::getItemSku))
          .collect(Collectors.toList());
      needCorrectionProduct.addAll(products);
      return needCorrectionProduct;
    } else {
      return products.stream().sorted(Comparator.comparing(ProductLevel3SummaryDetails::getItemSku))
          .collect(Collectors.toList());
    }
  }

  private List<ProductLevel3SummaryDetailsImage> constructProductLevel3SummaryDetailsImageFromProductItemImages(
      List<ProductItemImage> productItemImages) {
    List<ProductLevel3SummaryDetailsImage> productLevel3SummaryDetailsImages = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productItemImages)) {
      for (ProductItemImage image : productItemImages) {
        if (!image.isMarkForDelete() && filterProcessedProductItemImages(image)) {
          ProductLevel3SummaryDetailsImage productLevel3SummaryDetailsImage = new ProductLevel3SummaryDetailsImage();
          productLevel3SummaryDetailsImage.setLocationPath(image.getLocationPath());
          productLevel3SummaryDetailsImage.setMainImage(image.isMainImages());
          productLevel3SummaryDetailsImage.setSequence(image.getSequence());
          productLevel3SummaryDetailsImage.setActiveLocation(getActiveLocationStatus(image));
          productLevel3SummaryDetailsImages.add(productLevel3SummaryDetailsImage);
        }
      }
    }
    LOGGER.debug("constructProductLevel3SummaryDetailsImage masterDataItemImages {}", productLevel3SummaryDetailsImages);
    return productLevel3SummaryDetailsImages;
  }

  private boolean getActiveLocationStatus(ProductItemImage image) {
    if (image.isEdited()) {
      return true;
    }
    if (image.isRevised()) {
      return false;
    }
    if (Objects.isNull(image.getOriginalImage())) {
      return true;
    } else {
      return image.isActive();
    }
  }

  private boolean filterProcessedProductItemImages(ProductItemImage image) {
    if (image.isEdited()) {
      return image.isActive();
    }
    if (image.isRevised()) {
      return true;
    }
    if (Objects.isNull(image.getOriginalImage())) {
      return true;
    } else {
      return !image.getOriginalImage();
    }
  }

  private List<ProductItemWholesalePrice> findByStoreIdAndItemSkus(String storeId, List<String> itemSkus) {
    List<ProductItemWholesalePrice> productItemWholesalePrices = new ArrayList<>();
    List<List<String>> itemSkuList = Lists.partition(itemSkus, 10);
    for (List<String> itemSkuSubList : itemSkuList) {
      List<ProductItemWholesalePrice> wholesalePrices =
          productItemWholesalePriceRepository.findByStoreIdAndItemSkuIn(storeId, itemSkuSubList);
      if (CollectionUtils.isNotEmpty(wholesalePrices)) {
        productItemWholesalePrices.addAll(wholesalePrices);
      }
    }
    return productItemWholesalePrices;
  }

  private Map<String, CampaignPriceSkuResponse> getItemSkuCampaignPriceResponseMap(List<String> gdnSkus,
      String categoryCode, Map<String, String> itemSkuAndPickupCodeMap, Map<String, Double> l5AndOfferPriceMap) throws Exception {
    ProductSystemParameter productSystemParameter = productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);

    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap = new HashMap<>();
    if(Objects.nonNull(productSystemParameter) && Boolean.valueOf(productSystemParameter.getValue())) {
      //x-campaign call to get price info
      List<List<String>> itemSkusList = Lists.partition(gdnSkus, campaignPriceInfoBatchSize);
      List<CampaignPriceResponse> campaignPriceResponseList = new ArrayList<>();
      for (List<String> itemSkuSubList : itemSkusList) {
        CampaignPriceRequest campaignPriceRequest =
            modelConverter.convertItemSkusToCampaignPriceRequest(itemSkuSubList, categoryCode,
                itemSkuAndPickupCodeMap, l5AndOfferPriceMap);
        CampaignPriceResponse campaignPriceResponse = campaignOutbound.getCampaignPriceInfoV2(campaignPriceRequest);
        if( Objects.nonNull(campaignPriceResponse)) {
          campaignPriceResponseList.add(campaignPriceResponse);
        }
      }
      campaignPriceSkuResponseMap =
          modelConverter.convertCampaignPriceResponseListToCampaignPriceSkuResponseMap(campaignPriceResponseList);
    }
    return campaignPriceSkuResponseMap;
  }

  private Map<String, ProductItemResponse> getMasterDataProductItemInfo(List<String> itemCodes) throws Exception {
    Map<String, ProductItemResponse> productItemResponseMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(itemCodes)) {
      List<ProductItemResponse> responses =
          productOutbound.getProductItemBySkuCodes(ConverterUtil.convertItemSkusToSkuCodesRequest(itemCodes));
      if (CollectionUtils.isNotEmpty(responses)) {
        for (ProductItemResponse productItemResponse : responses) {
          productItemResponseMap.put(productItemResponse.getSkuCode(), productItemResponse);
        }
      }
    }
    return productItemResponseMap;
  }

  @Override
  public Map<String, ProductItemsCogs> getProductItemCogsValues(String businessPartnerCode, List<String> itemCodes)
      throws Exception {
    Double cogsValue = null;
    String cogsErrorCode = null;
    ProfileResponse businessPartner;
    Map<String, ProductItemsCogs> productItemsCogsMap = new HashedMap();
    businessPartner =
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    for(String itemCode : itemCodes) {
      ProductWarehouse productWarehouse = new ProductWarehouse();
      try {
        productWarehouse =
            getProductWareHouseData(itemCode, businessPartner);
      } catch (Exception e) {
        LOGGER.error("error invoking findCogsByProductSku itemCode : {} on open bravo : {} ",itemCode, e);
        cogsErrorCode = Constants.COGS_ERROR_CODE;
      }
      if (Objects.nonNull(productWarehouse) && Objects.nonNull(productWarehouse.getCogs())) {
        cogsValue = productWarehouse.getCogs().getCogs();
      }
      ProductItemsCogs productItemsCogs = ProductItemsCogs.builder().cogs(cogsValue).cogsErrorCode(cogsErrorCode).build();
      productItemsCogsMap.put(itemCode, productItemsCogs);
    }
    return productItemsCogsMap;
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse updateProductItemImages(
    ProductL3UpdateRequest l3UpdateRequest, ProductVariantUpdateRequest variantUpdateRequest) throws Exception {

    Map<String, Boolean> itemImagesUpdateStatus = new HashMap<>();
    List<ProductPriceStockAndImagesRequest> successValidationVariantList;
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    ProductL3Response savedProductData =
      xProductOutbound.getProductDetailsByProductSku(l3UpdateRequest.getProductSku()).getValue();
    ProductItemsImageUpdateRequest productItemsImageUpdateRequest = ProductItemCreationRequestHelper
      .createImageUpdateRequest(l3UpdateRequest, variantUpdateRequest);
    List<ItemSummaryResponse> itemSummaryResponseList = getItemSummaryResponseByItemSkus(
      productItemsImageUpdateRequest.getVariantList().stream()
        .map(ProductPriceStockAndImagesRequest::getItemSku).collect(Collectors.toList()));
    Map<String, ItemSummaryResponse> itemSummaryResponseMap = itemSummaryResponseList.stream()
      .collect(Collectors.toMap(ItemSummaryResponse::getItemSku, Function.identity()));

    if (l3UpdateRequest.isProductEditable()) {
      successValidationVariantList = variantEditValidationService
        .validateListOfVariantsWithSuccess(productItemsImageUpdateRequest.getVariantList(),
          failedRequests, itemSummaryResponseMap);
    } else {
      successValidationVariantList = variantEditValidationService
        .validateListOfVariantsForMultiUsedProduct(productItemsImageUpdateRequest.getVariantList(),
          failedRequests, itemSummaryResponseMap);
    }

    if(l3UpdateRequest.isProductEditable()) {
      EditedResizeAndImagesUpdateStatusResponse updateStatusResponse =
        updateItemVariantImages(productItemsImageUpdateRequest, successValidationVariantList);
      itemImagesUpdateStatus = updateStatusResponse.getItemImagesUpdateStatus();
      boolean postLive = productGoingForReviewIsPostLive(DEFAULT_STORE_ID,
          productItemsImageUpdateRequest.getVariantList().stream().findFirst().get().getProductSku(),
          l3UpdateRequest.getBusinessPartnerCode(),
        successValidationVariantList, itemImagesUpdateStatus, savedProductData, true).isPostLiveConfig();
      if (!updateStatusResponse.getEditedImages().isEmpty()) {
        productPublisherService.publishEditImageResizeEvent(
          new EditedImageResizeEvent(l3UpdateRequest.getProductCode(), DEFAULT_STORE_ID,
            updateStatusResponse.getEditedImages()));
      }
      updateProductScore(l3UpdateRequest.getProductCode(), itemImagesUpdateStatus, false);
      return createItemsPriceStockImagesUpdateResponse(failedRequests, itemImagesUpdateStatus.get(NEW), postLive, false);
    }

    updateProductScore(l3UpdateRequest.getProductCode(), itemImagesUpdateStatus, false);
    return createItemsPriceStockImagesUpdateResponse(failedRequests, false, false, false);
  }

  @Override
  public List<UpsertOfflineItem> upsertL5StockInInventory(String merchantCode,
      List<UpsertOfflineItem> updateOfflineItems, List<UpsertOfflineItemFailedResponse> failedOfflineItemResponses,
      ProfileResponse profileResponse)
      throws Exception {
    List<UpsertOfflineItem> updateL5 = new ArrayList<>();
    List<UpsertOfflineItem> newL5 = new ArrayList<>();
    for (UpsertOfflineItem upsertOfflineItem : updateOfflineItems) {
      if (upsertOfflineItem.isNew()) {
        newL5.add(upsertOfflineItem);
      } else {
        updateL5.add(upsertOfflineItem);
      }
    }

    List<UpsertOfflineItem> successOfflineItems = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(updateL5)) {
      List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList = new ArrayList();
      for (UpsertOfflineItem upsertOfflineItem : updateL5) {
        InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO = new InventoryDetailInfoRequestDTO();
        inventoryDetailInfoRequestDTO.setWebItemSku(upsertOfflineItem.getItemSku());
        inventoryDetailInfoRequestDTO.setWebMerchantCode(merchantCode);
        inventoryDetailInfoRequestDTO.setPickupPointCode(upsertOfflineItem.getPickupPointCode());
        inventoryDetailInfoRequestDTOList.add(inventoryDetailInfoRequestDTO);
      }

      List<ProductLevel3Inventory> savedInventoryList =
          productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
              inventoryDetailInfoRequestDTOList);
      Map<String, ProductLevel3Inventory> savedInventoryListMap = savedInventoryList.stream().collect(Collectors.toMap(
          productLevel3Inventory -> productLevel3Inventory.getWebItemSku() + Constants.HYPHEN
              + productLevel3Inventory.getWebPickupPointCode(), Function.identity()));

      for (UpsertOfflineItem upsertOfflineItem : updateL5) {
        String offlineItemId =
            upsertOfflineItem.getItemSku() + Constants.HYPHEN + upsertOfflineItem.getPickupPointCode();
        if (savedInventoryListMap.containsKey(offlineItemId)) {
          ProductLevel3Inventory productLevel3Inventory = savedInventoryListMap.get(offlineItemId);
          Integer deltaStock = upsertOfflineItem.getStock() - productLevel3Inventory.getWebAvailable();
          if (deltaStock == 0) {
            successOfflineItems.add(upsertOfflineItem);
            continue;
          }
          ApiErrorCode apiErrorCode =
              this.productLevel3InventoryService.updateStockForItemPickupPointWithErrorCode(merchantCode,
                  upsertOfflineItem.getItemSku(), upsertOfflineItem.getPickupPointCode(), deltaStock);
          if (Objects.nonNull(apiErrorCode)) {
            failedOfflineItemResponses.add(new UpsertOfflineItemFailedResponse(upsertOfflineItem.getItemSku(),
                upsertOfflineItem.getPickupPointCode(), apiErrorCode.getDesc()));
          } else {
            successOfflineItems.add(upsertOfflineItem);
            //save history
            this.updatedProductHistoryService.createProductL3AuditLog(merchantCode, upsertOfflineItem.getItemSku(),
                upsertOfflineItem.getProductSku(), upsertOfflineItem.getItemName(),
                UpdateProductActivity.STOCK_VALUE.getDesc(), String.valueOf(productLevel3Inventory.getWebAvailable()),
                String.valueOf(upsertOfflineItem.getStock()), false, upsertOfflineItem.getPickupPointCode());
          }
        } else {
          newL5.add(upsertOfflineItem);
        }
      }
    }

    if (CollectionUtils.isNotEmpty(newL5)) {
      List<ProductLevel3Inventory> productLevel3InventoryList = new ArrayList<>();
      for (UpsertOfflineItem upsertOfflineItem : newL5) {
        ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
        productLevel3Inventory.setWebItemSku(upsertOfflineItem.getItemSku());
        productLevel3Inventory.setWebPickupPointCode(upsertOfflineItem.getPickupPointCode());
        productLevel3Inventory.setWebAvailable(upsertOfflineItem.getStock());
        productLevel3Inventory.setWebMerchantCode(merchantCode);
        productLevel3Inventory.setProductSku(upsertOfflineItem.getProductSku());
        productLevel3Inventory.setWarehouseItemSku(upsertOfflineItem.getItemCode());
        productLevel3Inventory.setProductSku(upsertOfflineItem.getProductSku());
        productLevel3Inventory.setFbbPP(upsertOfflineItem.isFbbActivated() && mppForWhEnabled);
        if (isPurchaseOrderPurchaseTerm(profileResponse)) {
          productLevel3Inventory.setWarehouseMerchantCode(GdnBaseLookup.DEFAULT_BUSINESS_PARTNER_CODE);
        } else {
          productLevel3Inventory.setWarehouseMerchantCode(profileResponse.getBusinessPartnerCode());
        }
        productLevel3InventoryList.add(productLevel3Inventory);
      }
      if (CollectionUtils.isNotEmpty(productLevel3InventoryList)) {
        productLevel3InventoryService.insertInventory(productLevel3InventoryList);
      }
      successOfflineItems.addAll(newL5);
    }
    return successOfflineItems;
  }

  private EditedResizeAndImagesUpdateStatusResponse updateItemVariantImages(
    ProductItemsImageUpdateRequest productItemsImageUpdateRequest,
    List<ProductPriceStockAndImagesRequest> successValidationVariantList) throws Exception {
    new EditedResizeAndImagesUpdateStatusResponse();
    EditedResizeAndImagesUpdateStatusResponse updateStatusResponse;
    ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(DEFAULT_STORE_ID,
        productItemsImageUpdateRequest.getProductCode());
    updateStatusResponse = updateProductItemsImages(successValidationVariantList,
        productItemsImageUpdateRequest.getCopyToAllVariantImages(), productItemsImageUpdateRequest.getProductCode(),
        productItemsImageUpdateRequest.getBusinessPartnerCode(), productItemsImageUpdateRequest.isNeedCorrection(),
        CommonUtils.isProductActivatedBefore(productCollection), null);
    return updateStatusResponse;
  }

  protected Page<ProductLevel3SummaryDetails> constructProductLevel3SummaryDetails(
      Page<ItemSummaryDetailResponse> productDatas, Map<String, ProductLevel3Inventory> inventoryDatas,
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap, Pageable pageRequest,
      List<CategoryResponse> categories, Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap,
      Map<String, ProductItemsCogs> productItemsCogsMap, Map<String, ProductItemResponse> masterDataProductItemMap,
      String categoryCode) throws Exception {
    List<ProductLevel3SummaryDetails> products = new ArrayList<>();
    for (ItemSummaryDetailResponse productData : productDatas) {
      ProductLevel3Inventory inventory = inventoryDatas.get(productData.getItemSku());
      ProductLevel3SummaryDetails product = new ProductLevel3SummaryDetails();
      List<ProductLevel3Price> productPrices = new ArrayList<>();
      List<ProductLevel3ViewConfig> productViewConfigs = new ArrayList<>();
      product.setMerchantSku(productData.getMerchantSku());
      product.setProductType(productData.getProductType().getCode());
      product.setAvailableStockLevel2(inventory.getWebAvailable());
      product.setMinimumStockLevel2(inventory.getWebMinAlert());
      if (CollectionUtils.isNotEmpty(productData.getPrice())) {
        productPrices = modelConverter.convertItemPricesToProductLevel3Prices(productData.getPrice());
      }
      if (CollectionUtils.isNotEmpty(productData.getItemViewConfigs())) {
        productViewConfigs =
            modelConverter.convertItemViewConfigsToProductLevel3ViewConfigs(productData.getItemViewConfigs());
      }
      product.setPrices(productPrices);
      product.setViewConfigs(productViewConfigs);
      product.setItemSku(productData.getItemSku());
      product.setSkuCode(productData.getItemCode());
      product.setMerchantCode(productData.getMerchantCode());
      product.setItemName(productData.getGeneratedItemName());
      product.setCreatedDate(productData.getCreatedDate());
      product.setCreatedBy(productData.getCreatedBy());
      product.setUpdatedDate(productData.getUpdatedDate());
      product.setUpdatedBy(productData.getUpdatedBy());
      product.setVersion(productData.getVersion());
      product.setIsArchived(productData.isArchived());
      product.setLateFulfillment(productData.getIsLateFulfillment());
      product.setAvailableStockLevel1(inventory.getWarehouseAvailable());
      product.setReservedStockLevel1(inventory.getWarehouseReserved());
      product.setReservedStockLevel2(inventory.getWebReserved());
      product.setSynchronizeStock(inventory.isWebSyncStock());
      product.setOff2OnActiveFlag(productData.isOff2OnChannelActive());
      product.setProductCode(productData.getProductCode());
      product.setProductSku(productData.getProductSku());
      product.setWholesalePriceActivated(productData.getWholesalePriceActivated());
      product.setNonDistributionAvailable(inventory.getNonDistributionAvailable());
      product.setNonDistributionReserved(inventory.getNonDistributionReserved());
      product.setProductScore(productData.getProductScore());
      product.setActivePromoBundlings(productData.getActivePromoBundlings());
      product.setFreeSample(productData.isFreeSample());
      List<ProductLevel3SummaryDetailsImage> productImages =
          ofNullable(productData.getMasterDataItemImages()).orElse(new ArrayList<>()).stream()
              .map(image -> modelConverter.convertMasterDataItemImagesToProductLevel3SummaryDetailsImage(image))
              .collect(Collectors.toList());
      if (Objects.nonNull(wholesalePriceSkuResponseMap.get(productData.getItemSku())) && !wholesalePriceSkuResponseMap
          .get(productData.getItemSku()).getWholesaleRules().isEmpty()) {
        WholesalePriceSkuResponse wholesalePriceSkuResponse =
            wholesalePriceSkuResponseMap.get(productData.getItemSku());
        product.setWholesalePromoActivated(wholesalePriceSkuResponse.getPromoActive());
        if (Objects.nonNull(wholesalePriceSkuResponse.getSkuStatus())) {
          product.setWholesalePriceActivated(wholesalePriceSkuResponse.getSkuStatus().equalsIgnoreCase(ACTIVE));
        }
        product.setProductItemWholesalePrices(wholesalePriceSkuResponse.getWholesaleRules().entrySet().stream()
            .map(map -> new ProductItemWholesalePriceVo(map.getKey(), map.getValue())).collect(Collectors.toList()));
      } else {
        product.setWholesalePriceActivated(productData.getWholesalePriceActivated());
      }

      if (CollectionUtils.isNotEmpty(categories)) {
        String[] categoryNameAndHierarchy = generateCategoryNameIdAndHierarchy(categories);
        product.setCategoryName(categoryNameAndHierarchy[0]);
        product.setCategoryHierarchy(categoryNameAndHierarchy[1]);
        product.setCategoryId(categoryNameAndHierarchy[2]);
        product.setWholesalePriceConfigEnabled(categories.get(0).isWholesalePriceConfigEnabled());
      }
      product.setCategoryCode(categoryCode);
      product.setImages(productImages);
      product.setPromoBundling(productData.isPromoBundling());
      product.setMerchantPromoDiscount(productData.isMerchantPromoDiscount());
      product.setMerchantPromoDiscountActivated(productData.isMerchantPromoDiscountActivated());
      product.setPromoTypes(productData.getPromoTypes());
      product.setForceReview(productData.isForceReview());
      product.setFlashSaleActive(productData.isFlashSaleActive());
      product.setRejected(productData.isMarkForDelete());
      product.setSuspended(productData.isSuspended());
      if (MapUtils.isNotEmpty(campaignPriceSkuResponseMap) && campaignPriceSkuResponseMap
          .containsKey(productData.getItemSku())) {
        CampaignPriceSkuResponse campaignPriceSkuResponse = campaignPriceSkuResponseMap.get(productData.getItemSku());
        product.setItemCampaignMapped(campaignPriceSkuResponse.isRegistered());
        product.setItemCampaignActivated(campaignPriceSkuResponse.isLive());
        product.setCampaignCurrentPrice(campaignPriceSkuResponse.getCampaignPrice());
        product.setCampaignMinPrice(campaignPriceSkuResponse.getMinAllowedPrice());
        product.setCampaignMaxPrice(campaignPriceSkuResponse.getMaxAllowedPrice());
      }
      if (productItemsCogsMap.containsKey(productData.getItemCode())) {
        ProductItemsCogs productItemsCogs = productItemsCogsMap.get(productData.getItemCode());
        product.setCogs(productItemsCogs.getCogs());
        product.setCogsErrorCode(productItemsCogs.getCogsErrorCode());
      }
      if (masterDataProductItemMap.containsKey(productData.getItemCode())) {
        ProductItemResponse productItemResponse = masterDataProductItemMap.get(productData.getItemCode());
        product.setUpcCode(productItemResponse.getUpcCode());
        product.setImages(constructProductLevel3SummaryDetailsImage(productItemResponse.getImages()));
      }
      product.getImages().sort(Comparator.comparingInt(ProductLevel3SummaryDetailsImage::getSequence));
      product.setPriceEditDisabled(setPriceEditDisable(productData, campaignPriceSkuResponseMap));
      product.setPriceEditDisabledReason(setPriceEditDisabledReason(productData, campaignPriceSkuResponseMap));
      products.add(product);
    }
    LOGGER.info("Total products in page : {} are {}", pageRequest, products.size());
    return new PageImpl<>(products, pageRequest, productDatas.getTotalElements());
  }

  private String setPriceEditDisabledReason(ItemSummaryDetailResponse productData,
      Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap) {
    if (productData.isMerchantPromoDiscount()) {
      return Constants.MERCHANT_PROMO_DISCOUNT;
    } else if (campaignPriceSkuResponseMap.containsKey(productData.getItemSku())) {
      CampaignPriceSkuResponse campaignPriceSkuResponse = campaignPriceSkuResponseMap.get(productData.getItemSku());
      return campaignPriceSkuResponse.isLockPriceUpdate() ? Constants.CAMPAIGN : StringUtils.EMPTY;
    }
    return StringUtils.EMPTY;
  }

  private List<ProductLevel3SummaryDetailsImage> constructProductLevel3SummaryDetailsImage(List<Image> images) {
    List<ProductLevel3SummaryDetailsImage> productLevel3SummaryDetailsImages = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(images)) {
      for (Image image : images) {
        if (!image.isMarkForDelete() && filterProcessedProductImages(image)) {
          ProductLevel3SummaryDetailsImage productLevel3SummaryDetailsImage = new ProductLevel3SummaryDetailsImage();
          productLevel3SummaryDetailsImage.setLocationPath(image.getLocationPath());
          productLevel3SummaryDetailsImage.setMainImage(image.isMainImages());
          productLevel3SummaryDetailsImage.setSequence(image.getSequence());
          productLevel3SummaryDetailsImages.add(productLevel3SummaryDetailsImage);
        }
      }
    }
    LOGGER.debug("constructProductLevel3SummaryDetailsImage masterDataItemImages {}", productLevel3SummaryDetailsImages);
    return productLevel3SummaryDetailsImages;
  }

  public static boolean filterProcessedProductImages(Image image) {
    if (image.isEdited()) {
      return image.isActive();
    }
    if (image.isRevised()) {
      return image.isActive();
    }
    if (Objects.isNull(image.getOriginalImage())) {
      return true;
    } else {
      return !image.getOriginalImage();
    }
  }

  private boolean setPriceEditDisable(ItemSummaryDetailResponse productData,
      Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap) {
    if (productData.isMerchantPromoDiscount()) {
      return true;
    } else if (campaignPriceSkuResponseMap.containsKey(productData.getItemSku())) {
      CampaignPriceSkuResponse campaignPriceSkuResponse = campaignPriceSkuResponseMap.get(productData.getItemSku());
      return campaignPriceSkuResponse.isLockPriceUpdate();
    }
    return false;
  }

  @Override
  public Page<PickupPointCodeResponse> getPickupPointCodes(String productSku, int page, int size,
      boolean needCorrection, String storeId, String businessPartnerCode, boolean fbbActivated) throws Exception {
    List<PickupPointCodeResponse> pickupPointCodeResponses = new ArrayList<>();
    Map<String, String> ppCodeNameMap = new HashMap<>();
    long totalCount = 0;
    PageRequest pageRequest = PageRequest.of(page, size);
    ProfileResponse profileResponse =
      businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    if (!fbbActivated) {
      if (isMultiPickupPointEnabled(profileResponse)) {
        LOGGER
          .info("Error fetching PP, MPP was found to be enabled for productSKU : {}", productSku);
        throw new ApplicationException(ErrorCategory.INVALID_STATE,
          ApiErrorCode.MULTI_PICKUP_POINT_ENABLED_ERROR.getDesc());
      }
    }
    if (needCorrection) {
      ProductBusinessPartner productBusinessPartner =
          productBusinessPartnerRepository.findFirstByGdnProductSku(productSku);
      if (Objects.isNull(productBusinessPartner)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "productSku not found");
      }
      Page<ProductItemBusinessPartner> productItemBusinessPartnerList;
      Page<ProductItemBusinessPartner> productItemBusinessPartnerListFbb;
      if (fbbActivated) {
        productItemBusinessPartnerList = this.productItemBusinessPartnerRepository
          .findByStoreIdAndProductBusinessPartnerIdAndMarkForDeleteFalseOrderByGdnProductItemSku(
            storeId, productBusinessPartner.getId(),pageRequest);
        List<String> itemSkuList = productItemBusinessPartnerList.getContent().stream()
            .map(ProductItemBusinessPartner::getGdnProductItemSku)
            .distinct()
            .collect(Collectors.toList());
        productItemBusinessPartnerListFbb = productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuInAndFbbActiveTrueAndMarkForDeleteFalse(storeId,itemSkuList,pageRequest);
        productItemBusinessPartnerList = addFbbProductItemBusinessPartnertoProductItemBusinessPartnerList(productItemBusinessPartnerList,productItemBusinessPartnerListFbb,itemSkuList);
      } else {
        productItemBusinessPartnerList = this.productItemBusinessPartnerRepository
          .findByStoreIdAndProductBusinessPartnerIdAndMarkForDeleteFalseOrderByGdnProductItemSku(
            storeId, productBusinessPartner.getId(), pageRequest);
      }
      if (Objects.nonNull(productItemBusinessPartnerList)) {
        Product product = this.productRepository.findOne(productBusinessPartner.getProductId());
        Map<String, ProductItem> productItemMap =
            product.getProductItems().stream().collect(Collectors.toMap(ProductItem::getId, Function.identity()));
        List<String> pickupPointCodes = productItemBusinessPartnerList.getContent().stream()
          .map(ProductItemBusinessPartner::getPickupPointId).collect(Collectors.toList());
        List<PickupPointDetailResponse> pickupPointDetailResponse =
          xProductOutbound.getPickupPointDetailResponse(pickupPointCodes, fbbActivated);
        if (CollectionUtils.isNotEmpty(pickupPointDetailResponse)) {
            ppCodeNameMap = pickupPointDetailResponse.stream().distinct().collect(
            Collectors.toMap(PickupPointDetailResponse::getPickupPointCode, PickupPointDetailResponse::getPickupPointName));
        }
        else {
          LOGGER.info("PickupPoint Detail Response fetched from xProduct was empty");
        }
        for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartnerList.getContent()) {
          ProductItem productItem =
            productItemMap.get(productItemBusinessPartner.getProductItemId());
          pickupPointCodeResponses.add(PickupPointCodeResponse.builder()
            .itemSku(productItemBusinessPartner.getGdnProductItemSku())
            .itemName(productItem.getGeneratedItemName())
            .pickupPointCode(productItemBusinessPartner.getPickupPointId()).pickupPointName(
              ofNullable(ppCodeNameMap)
                .map(name -> name.get(productItemBusinessPartner.getPickupPointId())).orElse(null))
            .build());
        }
        totalCount = productItemBusinessPartnerList.getTotalElements();
      }
    } else {
      Page<ItemPickupPointCodeResponse> response =
          xProductOutbound.getItemPickupPointCodeResponse(productSku, page, size, fbbActivated);
      if (CollectionUtils.isNotEmpty(response.getContent())) {
        for (ItemPickupPointCodeResponse itemPickupPointCodeResponse : response.getContent()) {
          pickupPointCodeResponses.add(
            PickupPointCodeResponse.builder().itemSku(itemPickupPointCodeResponse.getItemSku())
              .itemName(itemPickupPointCodeResponse.getItemName())
              .pickupPointCode(itemPickupPointCodeResponse.getPickupPointCode())
              .pickupPointName(itemPickupPointCodeResponse.getPickupPointName()).build());
        }
        totalCount = response.getTotalElements();
      }
    }
    return new PageImpl<>(pickupPointCodeResponses, PageRequest.of(page, size), totalCount);
  }

  private Page<ProductItemBusinessPartner> addFbbProductItemBusinessPartnertoProductItemBusinessPartnerList(Page<ProductItemBusinessPartner> productItemBusinessPartnerList, Page<ProductItemBusinessPartner> productItemBusinessPartnerListFbb, List<String> itemSkuList) {
    Map<String, ProductItemBusinessPartner> fbbMap = productItemBusinessPartnerListFbb.getContent().stream()
        .collect(Collectors.toMap(ProductItemBusinessPartner::getGdnProductItemSku, item->item));
    Map<String,ProductItemBusinessPartner> allMap = productItemBusinessPartnerList.getContent().stream()
        .collect(Collectors.toMap(ProductItemBusinessPartner::getGdnProductItemSku, item->item,(newItem,item)-> item));
    List<ProductItemBusinessPartner> resultList = itemSkuList.stream()
        .map(sku -> fbbMap.containsKey(sku) ? fbbMap.get(sku) : setProductItemBusinessPartner(allMap,sku))
        .collect(Collectors.toList());
    int totalElements = resultList.size();
    int pageSize = productItemBusinessPartnerList.getSize();
    int pageNumber = productItemBusinessPartnerList.getNumber();
    Sort sort = productItemBusinessPartnerListFbb.getSort();
    PageRequest pageable = PageRequest.of(pageNumber, pageSize, sort);
    Page<ProductItemBusinessPartner> updatedPage = new PageImpl<>(resultList, pageable, totalElements);
    return updatedPage;
  }

  private ProductItemBusinessPartner setProductItemBusinessPartner(Map<String,ProductItemBusinessPartner> map , String itemSku){
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    BeanUtils.copyProperties(map.get(itemSku),productItemBusinessPartner,"pickupPointId");
    return productItemBusinessPartner;
  }

  private boolean isMultiPickupPointEnabled(ProfileResponse profileResponse){
    return (profileResponse.getCompany().isCncActivated() || (
      Boolean.TRUE.equals(profileResponse.getMultiDefaultAddressFlag())
        && (mppAllowedSellers.contains(profileResponse.getCompany().getMerchantType()))));
  }

  private boolean isEligibleForPickupPointUpdate(String businessPartnerCode) throws Exception {
    ProfileResponse profileResponse =
        businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    return (!profileResponse.getCompany().isCncActivated() && (
        !mppAllowedSellers.contains(profileResponse.getCompany().getMerchantType()) || !Boolean.TRUE
            .equals(profileResponse.getMultiDefaultAddressFlag())));
  }

  @Override
  public UniquePickupPointCodeResponse getUniquePickupPointCodes(String productSku) throws Exception {
    Set<String> pickupPointCodes = xProductOutbound.getPickupPointCodesByProductSku(productSku);
    Set<String> itemSkus = new HashSet<>(productBusinessPartnerRepository.findListOfItemSkusbyProductSku(productSku));
    return new UniquePickupPointCodeResponse(itemSkus, pickupPointCodes);
  }

  private Page<ItemSummaryResponse> getItemSummaryResponseSortByIsArchived(String productSku, int page, int size)
      throws Exception {
    ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
    itemSummaryRequest.setProductSkus(Arrays.asList(productSku));
    Page<ItemSummaryResponse> itemSummaryResponses =
        productLevel3Repository.findSummaryByFilter(itemSummaryRequest, PageRequest.of(page, size), new SortOrder());
    return itemSummaryResponses;
  }

  private String generateImageName(String location) {
    String[] splitImageFilenameByDash = location.split(File.separator);
    return splitImageFilenameByDash[splitImageFilenameByDash.length - 1];
  }

  @Override
  public Page<ProductL3SummaryResponse> getProductL3Summary(String storeId, String requestId,
      String businessPartnerCode, int page, int size, ProductL3SummaryRequest productL3SummaryRequest)
      throws Exception {
    ProductSkuSummaryRequest productSkuSummaryRequest = new ProductSkuSummaryRequest();
    BeanUtils.copyProperties(productL3SummaryRequest, productSkuSummaryRequest);
    Page<ProductSkuSummaryResponse> productSkuSummaryResponses = this.xProductOutbound
        .getProductSkuSummary(businessPartnerCode, productSkuSummaryRequest, page, size);
    if (CollectionUtils.isEmpty(productSkuSummaryResponses.getContent())) {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size),
          productSkuSummaryResponses.getTotalElements());
    }
    List<String> distinctCategoryCodeList =
        ofNullable(productSkuSummaryResponses.getContent()).orElse(new ArrayList<>())
            .stream().map(ProductSkuSummaryResponse::getCategoryCode).distinct().collect(Collectors.toList());
    Map<String, List<CategoryResponse>> categoryCodeAndCategoryHierarchyMap = ofNullable(
        this.categoryRepository
            .findHierarchyByCategoryCodes(new CategoryCodeRequest(distinctCategoryCodeList)))
        .orElse(new ArrayList<>()).stream().collect(Collectors
            .toMap(CategoryHierarchyResponse::getCategoryCode, CategoryHierarchyResponse::getCategoryHierarchy));
    return new PageImpl<>(CommonUtils
        .toProductL3SummaryResponse(productSkuSummaryResponses.getContent(),
            categoryCodeAndCategoryHierarchyMap), PageRequest.of(page, size),
        productSkuSummaryResponses.getTotalElements());
  }

  private List<ProductItemWholesalePriceRequest> getProductItemWholesalePriceRequestFromMap(
      Map<Integer, Double> wholesaleRules) {
    List<ProductItemWholesalePriceRequest>  productItemWholesalePriceRequestList = new ArrayList<>();
    ofNullable(wholesaleRules).orElse(new HashMap<>()).forEach((key, value) -> productItemWholesalePriceRequestList
        .add(new ProductItemWholesalePriceRequest(key, value)));
    return productItemWholesalePriceRequestList;
  }

  public static String getFilterUSP(String uniqueSellingPoint) {
    String uspWithoutTagsAndNewLine = getTextWithoutTags(PATTERN_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT,
        getTextWithoutTags(PATTERN_FOR_NON_ASCII_CHARS_AND_NEW_LINE, uniqueSellingPoint, StringUtils.SPACE),
        StringUtils.EMPTY);
    return getTextWithoutTags(PATTERN_FOR_EXTRA_SPACE, uspWithoutTagsAndNewLine, StringUtils.SPACE);
  }

  private static String getTextWithoutTags(Pattern pattern, String usp, String replace) {
    return pattern.matcher(usp).replaceAll(replace);
  }

  @Override
  public EditedResizeImagesResponse publishEditImagesForResizing(
      List<ProductLevel3SummaryDetailsImageRequest> request,
      List<LocationPathAndCommonImage> locationPathAndCommonImages, String productCode) {
    Map<ProductLevel3SummaryDetailsImageRequest, String> imageHashCodeMap = new HashMap<>();
    List<ImageRequest> editedImages = new ArrayList<>();
    Map<String, Boolean> commonImageMap = locationPathAndCommonImages.stream().collect(
        Collectors.toMap(LocationPathAndCommonImage::getLocationPath, LocationPathAndCommonImage::isCommonImage,
            (image1, image2) -> image2));
    for (ProductLevel3SummaryDetailsImageRequest image : request) {
      if (NEW.equals(image.getReviewType())) {
        String hashCode = ApproveProductUtils.generateHashcodeByLocationPath(image.getLocationPath());
        imageHashCodeMap.put(image, hashCode);
        String location = fileStorageService.generateFinalImageFullPath(image.getLocationPath());
        ImageRequest imageRequest =
            new ImageRequest(location.substring(location.lastIndexOf("/") + 1, location.length()), location, hashCode);
        imageRequest.setCommonImage(ofNullable(commonImageMap.get(image.getLocationPath())).orElse(false));
        editedImages.add(imageRequest);
        image.setResizeEventPublished(true);
      }
    }
    return new EditedResizeImagesResponse(imageHashCodeMap, editedImages);
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse editProductItemsPriceStockImages(String storeId,
      UpdateItemsPriceStockImagesRequest request, String businessPartnerCode) throws Exception {
    fileStorageService.editImageNameIfGcsEnabled(request);
    if(request.isNeedCorrection()) {
      return saveItemsPriceStockImagesNeedCorrection(storeId, request, businessPartnerCode);
    }
    String productSku =
        request.getProductItems().stream().findFirst().map(ProductPriceStockAndImagesRequest::getProductSku)
            .orElse(StringUtils.EMPTY);
    LOGGER.info("editProductItemsPriceStockImages productSku : {}, isSynchronize : {}, isProductEditable : {}",
        productSku, request.isSynchronize(), request.isProductEditable());
    ProductL3Response savedProductData = xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
    ApiErrorCode apiErrorCode = checkProductStatus(savedProductData);
    if (savedProductData.isFreeSample() && (
      request.getProductItems().get(0).getViewConfigs().get(0).getBuyable() || request
        .getProductItems().get(0).getViewConfigs().get(0).getDisplay())) {
      LOGGER.error("Error updating free sample product : {} ",
        request.getProductItems().get(0).getItemSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ErrorMessages.FREE_SAMPLE_MUST_NOT_BE_BUYABLE_OR_DISCOVERABLE);
    }
    if (Objects.nonNull(apiErrorCode)) {
      return ItemsPriceStockImagesUpdateResponse.builder().apiErrorCode(apiErrorCode).build();
    }
    if (!request.isSynchronize() && request.isProductEditable()) {
      String productCode =
          request.getProductItems().stream().map(ProductPriceStockAndImagesRequest::getProductCode).findFirst()
              .orElse(StringUtils.EMPTY);
      migrateAndSyncProduct(businessPartnerCode, productSku, productCode);
    }
    return updateItemsPriceStockImages(storeId, request.getProductItems(), businessPartnerCode,
        request.getCopyToAllVariantImages(), request.isProductEditable(), savedProductData);
  }

  private void migrateAndSyncProduct(String businessPartnerCode, String productSku, String productCode)
      throws Exception {
    ProductAndItemsResponse productAndItemsResponse =
        xProductOutbound.getProductAndItemsWithProductData(true, productSku, true, false).getValue();
    com.gdn.x.productcategorybase.dto.request.ProductRequest productRequest =
        ConverterUtil.toProductRequest(productAndItemsResponse);
    productOutbound.migrateProduct(productCode, productCode, businessPartnerCode, productRequest);
    this.productLevel3Repository.synchronizeProduct(productSku);
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse editProductItemsPriceStockImagesL5(String storeId,
      String businessPartnerCode, ProductVariantUpdateRequest productVariantUpdateRequest,
      EditProductResponse editResponse, ProductLevel3 productLevel3, boolean productDetailEdit) throws Exception {
    ProductItemCreationRequestHelper.validateProductItems(productVariantUpdateRequest);
    if (productVariantUpdateRequest.isNeedCorrection()) {
      Pair<ItemsPriceStockImagesUpdateResponse, FbbAndCncDataChangeDto> itemResponseAndFbbChangePair =
          getProductLevel3Service().saveItemsPriceStockImagesNeedCorrectionL5(storeId, productVariantUpdateRequest,
              businessPartnerCode, editResponse);
      deleteL5FromInventory(businessPartnerCode, productVariantUpdateRequest.getDeletePickupPoints());
      applicationContext.getBean(ProductLevel3ServiceBean.class)
          .updateFbbAtL3AndPopulateHistory(storeId, businessPartnerCode, productVariantUpdateRequest,
              itemResponseAndFbbChangePair);
      return itemResponseAndFbbChangePair.getLeft();
    }
    return updateVariantsPriceStockImages(storeId, businessPartnerCode,
        productVariantUpdateRequest.getCopyToAllVariantImages(), productVariantUpdateRequest.isProductEditable(),
        productVariantUpdateRequest, editResponse, productLevel3, productDetailEdit);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void updateFbbAtL3AndPopulateHistory(String storeId, String businessPartnerCode,
      ProductVariantUpdateRequest productVariantUpdateRequest,
      Pair<ItemsPriceStockImagesUpdateResponse, FbbAndCncDataChangeDto> itemResponseAndFbbChangePair) throws Exception {
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getAddPickupPoints()) || CollectionUtils.isNotEmpty(
        productVariantUpdateRequest.getDeletePickupPoints()) || itemResponseAndFbbChangePair.getRight()
        .isFbbChanged()) {
      ProductBusinessPartner productBusinessPartnerExisting =
          productBusinessPartnerService.findByProductSkuList(storeId,
              Arrays.asList(productVariantUpdateRequest.getProductSku())).get(0);
      ProductItemBusinessPartner productItemBusinessPartner =
          productItemBusinessPartnerRepository.findFirstByStoreIdAndProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
              storeId, productBusinessPartnerExisting.getId(), Boolean.TRUE);
      boolean fbbFlagAtL3 = Objects.nonNull(productItemBusinessPartner) ? Boolean.TRUE : Boolean.FALSE;
      log.info("Existing fbb--> {} And New Fbb {} for {} ", productBusinessPartnerExisting.isFbbActivated(),
          fbbFlagAtL3, productBusinessPartnerExisting.getGdnProductSku());
      if (productBusinessPartnerExisting.isFbbActivated() != fbbFlagAtL3) {
        productBusinessPartnerExisting.setFbbActivated(fbbFlagAtL3);
        productBusinessPartnerService.saveProductBusinessPartner(productBusinessPartnerExisting);
        this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT,
            productVariantUpdateRequest.getProductSku(), productVariantUpdateRequest.getProductName(),
            UpdateProductActivity.FBB_FLAG.getDesc(), String.valueOf(!fbbFlagAtL3),
            String.valueOf(fbbFlagAtL3), true, Constants.HYPHEN);
      }
    }
  }

  private void deleteL5FromInventory(String businessPartnerCode,
      List<PickupPointDeleteRequest> deletePickupPoints) throws Exception {
    if (deleteL5sFromInventoryDuringL5DeletionViaNeedRevision && CollectionUtils.isNotEmpty(deletePickupPoints)) {
      List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> deleteL5s =
          deletePickupPoints.stream()
              .map(deletePickupPoint -> new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO(
                  deletePickupPoint.getItemSku(), businessPartnerCode, deletePickupPoint.getPickupPointId()))
              .collect(Collectors.toList());
      productLevel3InventoryService.deleteByItemSkuAndPickupPointCode(deleteL5s);
    }
  }

  private ItemsPriceStockImagesUpdateResponse checkProductStatus(
      ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap) {

    if (MapUtils.isEmpty(savedItemPickupPointDataMap)) {
      return getItemsPriceStockImagesUpdateResponse(productVariantUpdateRequest,
          ApiErrorCode.PRODUCT_NOT_FOUND.getDesc(), ApiErrorCode.PRODUCT_NOT_FOUND.getCode());
    } else {
      ItemPickupPointListingResponse parentProductL5Response =
          savedItemPickupPointDataMap.entrySet().iterator().next().getValue();
      if (parentProductL5Response.isArchiveFlagAtL3Level() && CollectionUtils.isEmpty(
          productVariantUpdateRequest.getAddPickupPoints())) {
        return getItemsPriceStockImagesUpdateResponse(productVariantUpdateRequest,
            ApiErrorCode.PRODUCT_IS_ARCHIVED.getDesc(), ApiErrorCode.PRODUCT_IS_ARCHIVED.getCode());
      }
    }
    RequestHelper.validateAddPickupPointRequest(productVariantUpdateRequest, savedItemPickupPointDataMap, enableAddPickupPointCheck);
    return null;
  }

  private ItemsPriceStockImagesUpdateResponse getItemsPriceStockImagesUpdateResponse(
      ProductVariantUpdateRequest productVariantUpdateRequest, String errorMessage, String errorCode) {
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest
        .getProductItems()) {
      for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest
          .getModifiedItemPickupPoints()) {
        VariantsErrorListResponse variantsErrorListResponse =
            getVariantsErrorListResponse(productVariantPriceStockAndImagesRequest.getItemSku(),
                productVariantPriceStockAndImagesRequest.getItemName(), itemPickupPointRequest.getPickupPointId(),
                errorMessage + StringUtils.SPACE + productVariantUpdateRequest.getProductSku(), errorCode);
        itemsPriceStockImagesUpdateResponse.getVariantsErrorList().add(variantsErrorListResponse);
      }
    }
    for (ItemPickupPointRequest itemPickupPointRequest : productVariantUpdateRequest.getAddPickupPoints()) {
      VariantsErrorListResponse variantsErrorListResponse =
          getVariantsErrorListResponse(itemPickupPointRequest.getItemSku(), StringUtils.EMPTY,
              itemPickupPointRequest.getPickupPointId(), errorMessage + StringUtils.SPACE +
                  productVariantUpdateRequest.getProductSku(), errorCode);
      itemsPriceStockImagesUpdateResponse.getVariantsErrorList().add(variantsErrorListResponse);
    }
    for (PickupPointDeleteRequest itemPickupPointRequest : productVariantUpdateRequest.getDeletePickupPoints()) {
      VariantsErrorListResponse variantsErrorListResponse =
          getVariantsErrorListResponse(itemPickupPointRequest.getItemSku(), StringUtils.EMPTY,
              itemPickupPointRequest.getPickupPointId(), errorMessage + StringUtils.SPACE +
                  productVariantUpdateRequest.getProductSku(), errorCode);
      itemsPriceStockImagesUpdateResponse.getVariantsErrorList().add(variantsErrorListResponse);
    }
    if (CollectionUtils.isEmpty(itemsPriceStockImagesUpdateResponse.getVariantsErrorList())) {
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest
          .getProductItems()) {
        VariantsErrorListResponse variantsErrorListResponse =
            getVariantsErrorListResponse(productVariantPriceStockAndImagesRequest.getItemSku(),
                productVariantPriceStockAndImagesRequest.getItemName(), Constants.HYPHEN,
                errorMessage + StringUtils.SPACE + productVariantUpdateRequest.getProductSku(), errorCode);
        itemsPriceStockImagesUpdateResponse.getVariantsErrorList().add(variantsErrorListResponse);
      }
    }
    return itemsPriceStockImagesUpdateResponse;
  }

  private VariantsErrorListResponse getVariantsErrorListResponse(String itemSku, String itemName,
      String pickupPointCode, String errorMessage, String code) {
    VariantsErrorListResponse variantsErrorListResponse = new VariantsErrorListResponse();
    variantsErrorListResponse.setItemSku(itemSku);
    variantsErrorListResponse.setItemName(itemName);
    variantsErrorListResponse.setPickupPointCode(pickupPointCode);
    variantsErrorListResponse.setMessage(errorMessage);
    variantsErrorListResponse.setCode(code);
    return variantsErrorListResponse;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public Pair<ItemsPriceStockImagesUpdateResponse, FbbAndCncDataChangeDto> saveItemsPriceStockImagesNeedCorrectionL5(
      String storeId, ProductVariantUpdateRequest request, String businessPartnerCode, EditProductResponse editProductResponse) throws Exception {
    LOGGER.info("Saving variants update in need correction flow L5: {} ", request);
    request.setStoreId(storeId);
    List<ProductVariantPriceStockAndImagesRequest> successValidationVariantList;
    ProductL3Response productL3Response1 = new ProductL3Response();
    productL3Response1.setProductSku(request.getProductSku());
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    List<ItemPickupPointListingRequest> itemPickupPointListingRequestList = new ArrayList<>();
    List<L5HistoryDTO> l5HistoryList = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ProfileResponse profileResponse =
      ofNullable(editProductResponse).map(EditProductResponse::getProfileResponse).orElse(null);
    if (Objects.isNull(profileResponse)) {
      profileResponse =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    }
    //Request Validation
    variantEditValidationService
      .validateAndSetFbbActiveFlagAtL5(request, businessPartnerCode, profileResponse);

    successValidationVariantList = variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(request.getProductItems(),
        failedRequests);
    ProductCollection productCollection = this.productCollectionRepository.findByStoreIdAndProductCode(storeId,
        request.getProductCode());
    processSellerPenaltyChecks(request, profileResponse, sellerPenaltyEnabledPhase2);
    LOGGER.info("Successful validation of request L5: {} ", successValidationVariantList);
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    if (productCollection.isPostLive() || productCollection.isEdited()) {
      l5CodeAndResponseMap = getL5CodeAndResponseMapForNeedRevison(storeId, businessPartnerCode, request,
          itemPickupPointListingRequestList);
    }
    Map<String, ProductLevel3Inventory> savedInventoryListMap = new HashMap<>();
    if (productCollection.isPostLive() || (!productCollection.isPostLive() && productCollection.isEdited())) {
      List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList = new ArrayList<>();
      for (ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequestL5 : successValidationVariantList) {
        for (ItemPickupPointRequest modifiedItemPickupPoint : productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints()) {
          InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO = new InventoryDetailInfoRequestDTO();
          inventoryDetailInfoRequestDTO.setWebItemSku(productPriceStockAndImagesRequestL5.getItemSku());
          inventoryDetailInfoRequestDTO.setWebMerchantCode(productPriceStockAndImagesRequestL5.getMerchantCode());
          inventoryDetailInfoRequestDTO.setPickupPointCode(modifiedItemPickupPoint.getPickupPointId());
          inventoryDetailInfoRequestDTOList.add(inventoryDetailInfoRequestDTO);
        }
      }
      List<ProductLevel3Inventory> savedInventoryList = new ArrayList<>();
      if (!inventoryDetailInfoRequestDTOList.isEmpty()) {
        savedInventoryList = this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
            inventoryDetailInfoRequestDTOList);
      }
      for (ProductLevel3Inventory productLevel3Inventory : savedInventoryList) {
        savedInventoryListMap.put(productLevel3Inventory.getWebItemSku() + HYPHEN + productLevel3Inventory.getWebPickupPointCode(),
            productLevel3Inventory);
      }
    }

    List<ProductItemBusinessPartner> productItemsBusinessPartnerList = new ArrayList<>();
    List<ProductItemBusinessPartner> productItemsBusinessPartnerListForMerchantSku = new ArrayList<>();
    for (ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequestL5 : successValidationVariantList) {
      List<String> pickupPoints = new ArrayList<>();
      productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().stream()
          .forEach(modifiedItemPickupPoint -> pickupPoints.add(modifiedItemPickupPoint.getPickupPointId()));
      List<ProductItemBusinessPartner> productItemsBusinessPartners =
          productItemBusinessPartnerRepository.findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuAndPickupPointIdIn(
              storeId, productPriceStockAndImagesRequestL5.getItemSku(), pickupPoints);

      List<ProductItemBusinessPartner> productItemBusinessPartnerList =
          productItemBusinessPartnerRepository.findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(storeId,
              Arrays.asList(productPriceStockAndImagesRequestL5.getItemSku()));
      productItemsBusinessPartnerListForMerchantSku.addAll(productItemBusinessPartnerList);
      productItemsBusinessPartnerList.addAll(productItemsBusinessPartners);
    }

    Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMap = new HashMap<>();
    if (!productItemsBusinessPartnerList.isEmpty()) {
      for (ProductItemBusinessPartner productItemBusinessPartner : productItemsBusinessPartnerList) {
        productItemBusinessPartnerMap.put(
            productItemBusinessPartner.getGdnProductItemSku() + HYPHEN + productItemBusinessPartner.getPickupPointId(),
            productItemBusinessPartner);
      }
    }

    Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMapForMerchantSku = new HashMap<>();
    for (ProductItemBusinessPartner productItemBusinessPartner : productItemsBusinessPartnerListForMerchantSku) {
      productItemBusinessPartnerMapForMerchantSku.put(productItemBusinessPartner.getGdnProductItemSku(),
          productItemBusinessPartner);
    }

    String productSku = request.getProductSku();
    List<ProductItemBusinessPartner> productItemBusinessPartnerUpdatedList = new ArrayList<>();
    for (ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {

      updateMerchantSku(storeId, businessPartnerCode, productItemBusinessPartnerMapForMerchantSku, productSku,
          productPriceStockAndImagesRequest);

      for (ItemPickupPointRequest modifiedItemPickupPoint : productPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
        modifiedItemPickupPoint.setSellerSku(productPriceStockAndImagesRequest.getMerchantSku());
        ProductItemBusinessPartner productItemBusinessPartner = productItemBusinessPartnerMap.get(
            productPriceStockAndImagesRequest.getItemSku() + HYPHEN + modifiedItemPickupPoint.getPickupPointId());
        if (productItemBusinessPartner.getPrice().doubleValue() != modifiedItemPickupPoint.getPrice().doubleValue()) {
          variantEditValidationService.validatePriceLockCampaignRequestL5(productPriceStockAndImagesRequest,
              failedRequests, productCollection.getCategoryCode(), modifiedItemPickupPoint);
        }
        boolean priceChanged = isPriceChanged(productItemBusinessPartner, modifiedItemPickupPoint);
        List<Map<String, String>> historyAuditList = new ArrayList<>();
        String itemSku = productItemBusinessPartner.getGdnProductItemSku();
        String pickupPointCode = productItemBusinessPartner.getPickupPointId();
        modifiedItemPickupPoint.setFbbActive(productItemBusinessPartner.isFbbActive());
        fbbAndCncDataChangeDto =
            regenerateProductItemBusinessPartnerByProductPriceStockAndImagesRequestL5(productItemBusinessPartner,
                historyAuditList, modifiedItemPickupPoint, l5CodeAndResponseMap);
        CommonUtils.populateHistoryAuditMapForSchedulesUpdate(l5CodeAndResponseMap,
          schedulesAddEditEnabled, historyAuditList, modifiedItemPickupPoint,
          fbbAndCncDataChangeDto, ConverterUtil.isMPPEnabled(profileResponse, mppAllowedSellers), cncForWarehouseFeatureSwitch);

        //updating stock in x-inventory
        if (MapUtils.isNotEmpty(savedInventoryListMap) && savedInventoryListMap.containsKey(
            itemSku + HYPHEN + pickupPointCode)) {
          ProductLevel3Inventory savedInventory = savedInventoryListMap.get(itemSku + HYPHEN + pickupPointCode);
          updateMinimumStock(businessPartnerCode, modifiedItemPickupPoint, savedInventory, itemSku);
          updateSyncStock(businessPartnerCode, editProductResponse, productPriceStockAndImagesRequest,
              modifiedItemPickupPoint, savedInventory);
          updateWebInventory(businessPartnerCode, editProductResponse, productPriceStockAndImagesRequest,
              modifiedItemPickupPoint, failedRequests, itemSku, request.getPreOrder());
        }

        //Save in PBP
        if (fbbAndCncDataChangeDto.isFieldChanged()) {
          productItemBusinessPartnerUpdatedList.add(productItemBusinessPartner);
          l5HistoryList.add(new L5HistoryDTO(itemSku, modifiedItemPickupPoint.getPickupPointId(), historyAuditList));
        }
        if (priceChanged) {
          variantEditValidationService.updatePriceLockCampaignRequestL5(productPriceStockAndImagesRequest,
              failedRequests, productCollection.getCategoryCode(), modifiedItemPickupPoint);
        }
      }
    }

    productItemBusinessPartnerUpdatedList = updateBundleRecipeInProductItemBusinessPartner(storeId,
        request.getProductBundleRecipe(), productItemBusinessPartnerUpdatedList, l5HistoryList);

    // save in L4 db and update the history.
    saveProductItemBusinessPartnerAndUpdateHistory(businessPartnerCode, productSku,
        productItemBusinessPartnerUpdatedList, l5HistoryList);

    //upc Code update
    boolean isUpcCodeNotNullForAllItems =
        request.getProductItems().stream().anyMatch(productItem -> Objects.nonNull(productItem.getUpcCode()));
    if (isUpcCodeNotNullForAllItems) {
      updateUpcCodeL5(successValidationVariantList, businessPartnerCode, request.isNeedCorrection(), request);
    }

    //update free sample and cnc and b2b
    List<ProductBusinessPartner> productBusinessPartnerList =
        productBusinessPartnerService.findByProductSkuList(storeId, Arrays.asList(request.getProductSku()));
    if (!productBusinessPartnerList.isEmpty()) {
      ProductBusinessPartner productBusinessPartner = productBusinessPartnerList.get(0);
      if (request.isFreeSample() != productBusinessPartner.isFreeSample()) {
        productBusinessPartner.setFreeSample(request.isFreeSample());
      }
      CommonUtils.updateProductBusinessPartner(productBusinessPartner, request);

      settingCncInL3(request, fbbAndCncDataChangeDto, productBusinessPartner,
          productItemsBusinessPartnerListForMerchantSku);

      updateB2bFlagAtL3(request, productBusinessPartner);
      productBusinessPartnerRepository.save(productBusinessPartner);
      productLevel3V2Service.saveL3NeedRevisionHistory(businessPartnerCode, request, productBusinessPartner);
    }

    List<ItemPickupPointDto> itemPickupPointDtoList = new ArrayList<>();
    for (ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequestL5 : successValidationVariantList) {
      for (ItemPickupPointRequest modifiedItemPickupPoint : productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints()) {
        ItemPickupPointDto itemPickupPointDto = new ItemPickupPointDto();
        itemPickupPointDto.setItemSku(productPriceStockAndImagesRequestL5.getItemSku());
        itemPickupPointDto.setPickupPointCode(modifiedItemPickupPoint.getPickupPointId());
        itemPickupPointDtoList.add(itemPickupPointDto);
      }
    }

    List<ProductItemWholesalePrice> wholesalePriceList = new ArrayList<>();
    if (!itemPickupPointDtoList.isEmpty()) {
      wholesalePriceList = this.productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(storeId,
          itemPickupPointDtoList);
    }

    Map<String, ProductItemWholesalePrice> itemSkuPickupPointToWholesalePriceMap = new HashMap<>();
    wholesalePriceList.stream().forEach(productItemWholesalePrice -> itemSkuPickupPointToWholesalePriceMap.put(
        productItemWholesalePrice.getItemSku() + HYPHEN + productItemWholesalePrice.getPickupPointCode(),
        productItemWholesalePrice));

    //validate wholesale threshold
    if (!successValidationVariantList.isEmpty()) {
      validateWholesaleThresholdL5(successValidationVariantList, productCollection.getCategoryCode());
    }

    //updating wholesale price in PBP
    List<Map<String, String>> historyAuditList = new ArrayList<>();
    List<ProductItemWholesalePrice> productItemWholesalePrices =
        getProductItemWholesalePricesL5(storeId, successValidationVariantList, itemSkuPickupPointToWholesalePriceMap,
            productItemBusinessPartnerMap, historyAuditList, l5CodeAndResponseMap);

    if (CollectionUtils.isNotEmpty(productItemWholesalePrices)) {
      productItemWholesalePriceService.saveWholesalePriceNew(productItemWholesalePrices);
      createAuditLogsForPriceStockChangesL5(businessPartnerCode, null, productSku, historyAuditList, StringUtils.EMPTY);
    }

    boolean mppEnabled = variantEditValidationService.validateL5UpdateRequestWithErrorCode(request);
    if (mppEnabled) {
      // add pickupPoint
      addPickupPoint(request);

      // delete pickupPoint
      deletePickupPoint(storeId, request);
      additionAndDeletionHistory(businessPartnerCode, request, true);
    }

    //update images
    List<ProductPriceStockAndImagesRequest> productPriceStockAndImagesRequestList = new ArrayList<>();
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setProductCode(productCollection.getProductCode());
    productL3Response.setProductSku(productSku);
    productL3Response.setMasterCatalog(new MasterCatalogDTO(null,
        new CategoryDTO(productCollection.getCategoryCode(), productCollection.getCategoryCode())));
    for (ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      // Getting success validation list to reuse existing update upcCode method
      getSuccessValidationList(productL3Response, productPriceStockAndImagesRequestList,
          productPriceStockAndImagesRequest);
    }
    String productCode = request.getProductCode();

    EditedResizeAndImagesUpdateStatusResponse response =
        updateProductItemsImages(productPriceStockAndImagesRequestList, request.getCopyToAllVariantImages(),
            productCode, businessPartnerCode, true, CommonUtils.isProductActivatedBefore(productCollection),
          null);

    Map<String, Boolean> imagesResponse = response.getItemImagesUpdateStatus();
    boolean imageChanged = false;
    for (Map.Entry<String, Boolean> entry : imagesResponse.entrySet()) {
      if (entry.getValue()) {
        imageChanged = true;
        break;
      }
    }
    if (imageChanged) {
      updateNeedRevisionNotes(productCollection, Arrays.asList(Constants.IMAGE));
      this.productCollectionRepository.save(productCollection);
    }

    return Pair.of(
        ItemsPriceStockImagesUpdateResponse.builder().productReview(false).variantsErrorList(failedRequests).build(),
        fbbAndCncDataChangeDto);
  }

  private void updateSyncStock(String businessPartnerCode, EditProductResponse editProductResponse,
      ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest,
      ItemPickupPointRequest modifiedItemPickupPoint, ProductLevel3Inventory savedInventory) throws Exception {
    if (validateSynchronizeStock(businessPartnerCode) && !Objects.equals(
        modifiedItemPickupPoint.getSynchronizeStock(), savedInventory.isWebSyncStock())) {
      this.productLevel3InventoryService.updateSyncStockOrInsertStock(
          RequestHelper.getSyncStockUpdateVo(mppForWhEnabled, businessPartnerCode,
              editProductResponse.getProfileResponse(), productPriceStockAndImagesRequest, modifiedItemPickupPoint, null));
      this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode,
          productPriceStockAndImagesRequest.getItemSku(), productPriceStockAndImagesRequest.getProductSku(),
          productPriceStockAndImagesRequest.getItemName(), UpdateProductActivity.SYNC_STOCK.getDesc(),
          String.valueOf(!modifiedItemPickupPoint.getSynchronizeStock()),
          String.valueOf(modifiedItemPickupPoint.getSynchronizeStock()), true,
          modifiedItemPickupPoint.getPickupPointId());
    }
  }

  private void updateMinimumStock(String businessPartnerCode, ItemPickupPointRequest modifiedItemPickupPoint,
      ProductLevel3Inventory savedInventory, String itemSku) throws Exception {
    if (Objects.nonNull(modifiedItemPickupPoint.getMinimumStock()) && !modifiedItemPickupPoint.getMinimumStock()
        .equals(savedInventory.getWebMinAlert())) {
      this.productLevel3InventoryService.updateMinimumStockAlert(businessPartnerCode, itemSku,
          modifiedItemPickupPoint.getMinimumStock());
    }
  }

  private void updateWebInventory(String businessPartnerCode,
      EditProductResponse editProductResponse,
      ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest,
      ItemPickupPointRequest modifiedItemPickupPoint,
      List<VariantsErrorListResponse> failedRequests, String itemSku, PreOrderRequest preOrderRequest)
      throws Exception {
    if (Objects.nonNull(modifiedItemPickupPoint.getStock())) {
      ApiErrorCode apiErrorCode = this.productLevel3InventoryService.updateOrInsertStock(
          RequestHelper.getUpdateOrInsertStockVo(mppForWhEnabled, businessPartnerCode,
              editProductResponse.getProfileResponse(), productPriceStockAndImagesRequest,
              modifiedItemPickupPoint, preOrderConfig.isPoQuotaFeatureSwitch(), preOrderRequest));
      if (Objects.nonNull(apiErrorCode)) {
        failedRequests.add(VariantsErrorListResponse.builder().itemSku(itemSku)
            .itemName(productPriceStockAndImagesRequest.getItemName()).code(apiErrorCode.getCode())
            .message(apiErrorCode.getDesc()).build());
      }
    }
  }

  private boolean isPriceChanged(ProductItemBusinessPartner productItemBusinessPartner,
      ItemPickupPointRequest itemPickupPointRequest) {
    if ((!productItemBusinessPartner.getPrice().equals(itemPickupPointRequest.getPrice()))
        || (!productItemBusinessPartner.getSalePrice().equals(itemPickupPointRequest.getSalePrice()))) {
      return true;
    }
    return false;
  }

  public List<ProductItemBusinessPartner> updateBundleRecipeInProductItemBusinessPartner(
      String storeId, List<ProductBundleRecipeRequest> productBundleRecipe,
      List<ProductItemBusinessPartner> productItemBusinessPartnerUpdatedList,
      List<L5HistoryDTO> l5HistoryList) throws JsonProcessingException, ApplicationException {
    if (CollectionUtils.isNotEmpty(productBundleRecipe)) {
      Pair<List<ProductItemBusinessPartner>, List<L5HistoryDTO>>
          bundlingRecipeUpdatedProductItemBusinessPartner =
          bundleRecipeService.updateBundleRecipeInProductItemBusinessPartner(storeId,
              productItemBusinessPartnerUpdatedList, productBundleRecipe);
      l5HistoryList.addAll(bundlingRecipeUpdatedProductItemBusinessPartner.getValue());
      return productItemBusinessPartnerService
          .mergeProductItemBusinessPartnerLists(productItemBusinessPartnerUpdatedList,
              bundlingRecipeUpdatedProductItemBusinessPartner.getKey());
    }
    return productItemBusinessPartnerUpdatedList;
  }

  public void saveProductItemBusinessPartnerAndUpdateHistory(String businessPartnerCode,
      String productSku, List<ProductItemBusinessPartner> productItemBusinessPartnerList,
      List<L5HistoryDTO> l5HistoryList) throws Exception {
    if(CollectionUtils.isNotEmpty(productItemBusinessPartnerList)) {
      productItemBusinessPartnerService.save(productItemBusinessPartnerList);
      for (L5HistoryDTO history : l5HistoryList) {
        createAuditLogsForPriceStockChangesL5(businessPartnerCode, history.getItemSku(),
            productSku, history.getHistoryList(), history.getPickupPointCode());
      }
    }
  }

  private void updateB2bFlagAtL3(ProductVariantUpdateRequest request, ProductBusinessPartner productBusinessPartner) {
    if (Objects.nonNull(request.getB2cActivated())) {
      productBusinessPartner.setB2cActivated(request.getB2cActivated());
      if (!request.getB2cActivated() && !instoreNewFlowEnabled) {
        productBusinessPartner.setOff2OnChannelActive(false);
      }
    }
    if (Objects.nonNull(request.getB2bActivated())) {
      productBusinessPartner.setB2bActivated(request.getB2bActivated());
    }
  }

  private void settingCncInL3(ProductVariantUpdateRequest request,
      FbbAndCncDataChangeDto fbbAndCncDataChangeDto, ProductBusinessPartner productBusinessPartner,
      List<ProductItemBusinessPartner> productItemBusinessPartnerList) {
    if (fbbAndCncDataChangeDto.isCncChanged() || CollectionUtils.isNotEmpty(request.getAddPickupPoints())
        || CollectionUtils.isNotEmpty(request.getDeletePickupPoints())) {
      boolean cncActive = productItemBusinessPartnerList.stream().filter(
              productItemBusinessPartner -> productItemBusinessPartner.getProductBusinessPartnerId()
                  .equals(productBusinessPartner.getId())).filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete))
          .anyMatch(productItemBusinessPartner -> CommonUtils.isL5CncActive(productItemBusinessPartner,
              cncForWarehouseFeatureSwitch));
      cncActive = CommonUtils.isCncActive(request, cncActive, cncForWarehouseFeatureSwitch);
      if (productBusinessPartner.isCncActivated() != cncActive) {
        productBusinessPartner.setCncActivated(cncActive);
      }
    }
  }

  private void  deletePickupPoint(String storeId, ProductVariantUpdateRequest request) {
    if (!request.getDeletePickupPoints().isEmpty()) {
      for (PickupPointDeleteRequest pickupPointDeleteRequest : request.getDeletePickupPoints()) {
        productItemBusinessPartnerRepository.updateMarkForDeleteTrueAndFbbFalseByStoreIdAndItemSkuInAndPickupPointId(
            GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getStoreId(),
            Arrays.asList(pickupPointDeleteRequest.getItemSku()), pickupPointDeleteRequest.getPickupPointId());
      }
    }
  }

  private void addPickupPoint(ProductVariantUpdateRequest request) throws Exception {
    if (!request.getAddPickupPoints().isEmpty()) {
      List<ProductItemBusinessPartner> productItemBusinessPartnerList = new ArrayList<>();
      List<ProductItemWholesalePrice> productItemWholesalePriceList = new ArrayList<>();
      for (ItemPickupPointRequest itemPickupPointRequest : request.getAddPickupPoints()) {
        List<ProductItemBusinessPartner> productItemBusinessPartnerListExistingData =
            productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSku(DEFAULT_STORE_ID,
                itemPickupPointRequest.getItemSku());

        Map<String, ProductItemBusinessPartner> productItemBusinessPartnerPickupPointCodeMap =
            productItemBusinessPartnerListExistingData.stream().collect(Collectors
                .toMap(ProductItemBusinessPartner::getPickupPointId, Function.identity()));
        ProductItemBusinessPartner productItemBusinessPartnerExistingData;
        if (productItemBusinessPartnerListExistingData.isEmpty()) {
          throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
              ApiErrorCode.REQUIRED_L4_DATA_NOT_FOUND_IN_DB.getDesc());
        } else {
          productItemBusinessPartnerExistingData = productItemBusinessPartnerListExistingData.get(0);
        }
        for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartnerListExistingData) {
          if (productItemBusinessPartner.getPickupPointId().equals(itemPickupPointRequest.getPickupPointId()) &&
              !productItemBusinessPartner.isMarkForDelete()) {
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                ApiErrorCode.PICKUP_POINT_IS_ALREADY_PRESENT_IN_DB.getDesc());
          }
        }
        ProductItemBusinessPartner productItemBusinessPartner =
            ConverterUtil.getProductItemBusinessPartner(request, itemPickupPointRequest,
                productItemBusinessPartnerExistingData, productItemBusinessPartnerPickupPointCodeMap,
                request.getB2bActivated());
        productItemBusinessPartnerList.add(productItemBusinessPartner);

        if (Objects.nonNull(itemPickupPointRequest.getWholesalePriceActivated())) {
          ProductItemWholesalePrice productItemWholesalePrice =
              ConverterUtil.getProductItemWholesalePrice(request, itemPickupPointRequest,
                  productItemBusinessPartnerExistingData);
          productItemWholesalePrice.setWholesaleRules(
              mapperUtil.mapRequestToString(itemPickupPointRequest.getProductItemWholesalePriceRequests()));
          productItemWholesalePriceList.add(productItemWholesalePrice);
        }
      }
      productItemBusinessPartnerRepository.saveAll(productItemBusinessPartnerList);
      productItemWholesalePriceService.saveWholesalePriceNew(productItemWholesalePriceList);
    }
  }

  private void updateMerchantSku(String storeId, String businessPartnerCode,
      Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMapForMerchantSku, String productSku,
      ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest) throws Exception {
    List<Map<String, String>> historyAuditMapForMerchantSku = new ArrayList<>();
    ProductItemBusinessPartner productItemBusinessPartnerForMerchantSku =
        productItemBusinessPartnerMapForMerchantSku.get(productPriceStockAndImagesRequest.getItemSku());
    if (Objects.nonNull(productItemBusinessPartnerForMerchantSku) && !StringUtils.equals(
        productPriceStockAndImagesRequest.getMerchantSku(),
        productItemBusinessPartnerForMerchantSku.getMerchantSku())) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, productItemBusinessPartnerForMerchantSku.getMerchantSku());
      historyMap.put(CURRENT_VALUE, productPriceStockAndImagesRequest.getMerchantSku());
      historyMap.put(ACTIVITY, UpdateProductActivity.MERCHANT_SKU.getDesc() + Constants.NEED_REVISION);
      historyAuditMapForMerchantSku.add(historyMap);

      this.productItemBusinessPartnerService.updateMerchantSkuByStoreIdAndItemSku(storeId,
          productPriceStockAndImagesRequest.getItemSku(), productPriceStockAndImagesRequest.getMerchantSku());
      createAuditLogsForPriceStockChangesL5(businessPartnerCode, productPriceStockAndImagesRequest.getItemSku(),
          productSku, historyAuditMapForMerchantSku, null);
    }
  }

  private ItemsPriceStockImagesUpdateResponse saveItemsPriceStockImagesNeedCorrection(String storeId,
      UpdateItemsPriceStockImagesRequest request, String businessPartnerCode) throws Exception {
    LOGGER.info("Saving variants update in need correction flow : {}", request);
    List<ProductPriceStockAndImagesRequest> successValidationVariantList;
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    //Request Validation
    successValidationVariantList =
        variantEditValidationService.validateListOfVariantsForNeedCorrection(request.getProductItems(), failedRequests);
    LOGGER.info("Successful validation of request : {}", successValidationVariantList);
    List<String> itemSkus = new ArrayList<>();
    if (Objects.nonNull(successValidationVariantList)) {
      itemSkus = successValidationVariantList.stream().map(ProductPriceStockAndImagesRequest::getItemSku)
          .collect(Collectors.toList());
    }
    ProductCollection productCollection = this.productCollectionRepository
        .findByStoreIdAndProductCode(storeId, request.getProductItems().stream().findFirst().get().getProductCode());
    Map<String, ProductLevel3Inventory> savedInventoryListMap = new HashMap<>();
    if (productCollection.isPostLive() || (!productCollection.isPostLive() && productCollection.isEdited())) {
      List<ProductLevel3Inventory> savedInventoryList = this.productLevel3InventoryService
          .findInventoryByBusinessPartnerCodeAndListOfGdnSku(businessPartnerCode, itemSkus);
      savedInventoryListMap = savedInventoryList.stream()
          .collect(Collectors.toMap(ProductLevel3Inventory::getWebItemSku, Function.identity()));
    }
    List<ProductItemBusinessPartner> productItemsBusinessPartnerList = productItemBusinessPartnerRepository
        .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(storeId, itemSkus);
    Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMap = productItemsBusinessPartnerList.stream()
        .collect(Collectors.toMap(ProductItemBusinessPartner::getGdnProductItemSku, Function.identity()));
    String productSku = request.getProductItems().stream().findFirst().get().getProductSku();
    String productName = productLevel3Repository.getProductNameByProductSku(Arrays.asList(productSku)).get(productSku);
    for (ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      ProductItemBusinessPartner productItemBusinessPartner =
          productItemBusinessPartnerMap.get(productPriceStockAndImagesRequest.getItemSku());
      productPriceStockAndImagesRequest.setPickupPointCode(productItemBusinessPartner.getPickupPointId());
      if (productItemBusinessPartner.getPrice().doubleValue() != productPriceStockAndImagesRequest.getPrices().get(0)
          .getPrice().doubleValue()) {
        variantEditValidationService
            .validatePriceLockCampaignRequest(productPriceStockAndImagesRequest, failedRequests);
      }
      List<Map<String, String>> historyAuditList = new ArrayList<>();
      String itemSku = productItemBusinessPartner.getGdnProductItemSku();
      boolean isChanged =
          regenerateProductItemBusinessPartnerByProductPriceStockAndImagesRequest(productPriceStockAndImagesRequest,
              productItemBusinessPartner, businessPartnerCode, historyAuditList);
      //updating stock in x-inventory
      if (MapUtils.isNotEmpty(savedInventoryListMap) && savedInventoryListMap.containsKey(itemSku)) {
        ProductLevel3Inventory savedInventory = savedInventoryListMap.get(itemSku);
        if (Objects.nonNull(productPriceStockAndImagesRequest.getDeltaStock())) {
          ApiErrorCode apiErrorCode = this.productLevel3InventoryService.updateStockV2(businessPartnerCode, itemSku,
              productPriceStockAndImagesRequest.getDeltaStock());
          if (Objects.nonNull(apiErrorCode)) {
            failedRequests.add(VariantsErrorListResponse.builder().itemSku(itemSku)
                .itemName(productPriceStockAndImagesRequest.getItemName()).code(apiErrorCode.getCode())
                .message(apiErrorCode.getDesc()).build());
          }
        }
        if (Objects.nonNull(productPriceStockAndImagesRequest.getMinimumStockLevel2())
            && !productPriceStockAndImagesRequest.getMinimumStockLevel2().equals(savedInventory.getWebMinAlert())) {
          this.productLevel3InventoryService.updateMinimumStockAlert(businessPartnerCode, itemSku,
              productPriceStockAndImagesRequest.getMinimumStockLevel2());
        }
        //sync stock update in-case of Warehouse merchant
        if (Objects.nonNull(productPriceStockAndImagesRequest.getSynchronizeStock()) && validateSynchronizeStock(
            businessPartnerCode) && !productPriceStockAndImagesRequest.getSynchronizeStock()
            .equals(savedInventory.isWebSyncStock())) {
          this.productLevel3InventoryService.updateSyncStockByBusinessPartnerCodeAndGdnSku(businessPartnerCode,
              productPriceStockAndImagesRequest.getItemSku(), productPriceStockAndImagesRequest.getSynchronizeStock(),
              Collections.singletonList(
                  new ItemSkuPickupPointSyncStockDto(productPriceStockAndImagesRequest.getItemSku(),
                      productPriceStockAndImagesRequest.getPickupPointCode(),
                      productPriceStockAndImagesRequest.getSynchronizeStock())));
          this.updatedProductHistoryService
              .createProductL3AuditLog(businessPartnerCode, productPriceStockAndImagesRequest.getItemSku(),
                  productPriceStockAndImagesRequest.getProductSku(), productPriceStockAndImagesRequest.getItemName(),
                  UpdateProductActivity.SYNC_STOCK.getDesc(),
                  String.valueOf(!productPriceStockAndImagesRequest.getSynchronizeStock()),
                  String.valueOf(productPriceStockAndImagesRequest.getSynchronizeStock()), true, StringUtils.EMPTY);
        }
      }
      //Save in PBP
      if (isChanged) {
        this.productItemBusinessPartnerRepository.save(productItemBusinessPartner);
        createAuditLogsForPriceStockChanges(businessPartnerCode, itemSku, productName, productSku, historyAuditList);
        updateDiscountPriceInCampaign(itemSku, productPriceStockAndImagesRequest.getPrices().get(0).getSalePrice(),
          productPriceStockAndImagesRequest.getCategoryCode(), productPriceStockAndImagesRequest.getPickupPointCode());
      }
    }
    //upc Code update
    updateUpcCode(successValidationVariantList, businessPartnerCode, request.isNeedCorrection(), new ArrayList<>(),
        new SimpleStringResponse(), false, null, false, null);

    List<ProductItemWholesalePrice> wholesalePriceList =
        this.productItemWholesalePriceService.findByStoreIdAndItemSkus(storeId, itemSkus);
    Map<String, ProductItemWholesalePrice> itemSkuToWholesalePriceMap = wholesalePriceList.stream().
        collect(Collectors.toMap(ProductItemWholesalePrice::getItemSku, Function.identity()));

    //validate wholesale threshold
    validateWholesaleThreshold(successValidationVariantList);

    //updating wholesale price in PBP
    List<Map<String, String>> historyAuditList = new ArrayList<>();
    List<ProductItemWholesalePrice> productItemWholesalePrices =
        getProductItemWholesalePrices(storeId, successValidationVariantList, itemSkuToWholesalePriceMap,
            productItemBusinessPartnerMap, historyAuditList);
    if (CollectionUtils.isNotEmpty(productItemWholesalePrices)) {
      productItemWholesalePriceService.saveWholesalePrice(productItemWholesalePrices);
      createAuditLogsForPriceStockChanges(businessPartnerCode, null, productName, productSku, historyAuditList);
    }

    //update images
    String productCode = request.getProductItems().stream().findFirst().get().getProductCode();
    updateProductItemsImages(successValidationVariantList, request.getCopyToAllVariantImages(), productCode,
        businessPartnerCode, request.isNeedCorrection(), CommonUtils.isProductActivatedBefore(productCollection),
      null);
    return ItemsPriceStockImagesUpdateResponse.builder().productReview(false).variantsErrorList(failedRequests).build();
  }

  private void validateWholesaleThreshold(List<ProductPriceStockAndImagesRequest> successValidationVariantList)
      throws Exception {
    //check for wholesale threshold
    Map<String, String> itemSkuAndPickupCodeMap = new HashMap<>();
    for (ProductPriceStockAndImagesRequest priceStockAndImagesRequest : successValidationVariantList) {
      itemSkuAndPickupCodeMap
          .putIfAbsent(priceStockAndImagesRequest.getItemSku(), priceStockAndImagesRequest.getPickupPointCode());
    }
    List<WholesalePriceSkuResponse> wholesalePriceSkuResponseList = productPricingOutbound.getWholesalePriceList(
        successValidationVariantList.stream().map(ProductPriceStockAndImagesRequest::getItemSku)
            .collect(Collectors.toSet()), itemSkuAndPickupCodeMap);
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap = wholesalePriceSkuResponseList.stream()
        .collect(Collectors.toMap(WholesalePriceSkuResponse::getItemSku, Function.identity()));
    for (ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      if (Objects.nonNull(productPriceStockAndImagesRequest.getWholesalePriceActivated())
          && productPriceStockAndImagesRequest.getWholesalePriceActivated()) {
        Boolean isInSameThreshold = variantEditValidationService
            .isSameThreshold(productPriceStockAndImagesRequest, productPriceStockAndImagesRequest.getCategoryCode(),
                productPriceStockAndImagesRequest.getItemSku(),
                wholesalePriceSkuResponseMap.get(productPriceStockAndImagesRequest.getItemSku()), null);
        productPriceStockAndImagesRequest.setWholesalePriceActivated(isInSameThreshold);
      } else {
        productPriceStockAndImagesRequest
            .setWholesalePriceActivated(productPriceStockAndImagesRequest.getWholesalePriceActivated());
      }
    }
  }

  public void validateWholesaleThresholdL5(List<ProductVariantPriceStockAndImagesRequest> successValidationVariantList,
      String categoryCode) throws Exception {
    //check for wholesale threshold
    List<WholesalePriceSkuResponse> wholesalePriceSkuResponseList = new ArrayList<>();
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap = new HashMap<>();
      List<ItemInfoDto> itemInfoDtoList = new ArrayList<>();
      for (ProductVariantPriceStockAndImagesRequest request : successValidationVariantList) {
        for (ItemPickupPointRequest itemPickupPointRequest : ofNullable(request
            .getModifiedItemPickupPoints()).orElse(new ArrayList<>())) {
          ItemInfoDto itemInfoDto = new ItemInfoDto();
          itemInfoDto.setItemPickupPointId(request.getItemSku() + Constants.HYPHEN + itemPickupPointRequest.getPickupPointId());
          itemInfoDto.setItemSku(request.getItemSku());
          itemInfoDto.setPickupPointCode(itemPickupPointRequest.getPickupPointId());
          itemInfoDtoList.add(itemInfoDto);
        }
      }
    getWholesalePriceSkuResponseMapInBatches(itemInfoDtoList, wholesalePriceSkuResponseMap);
    for (ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      for (ItemPickupPointRequest modifiedItemPickupPoint : productPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
        if (Objects.nonNull(modifiedItemPickupPoint.getWholesalePriceActivated())
            && modifiedItemPickupPoint.getWholesalePriceActivated()) {
          String wholesalePriceKey;
            wholesalePriceKey = productPriceStockAndImagesRequest.getItemSku() + Constants.HYPHEN +
                modifiedItemPickupPoint.getPickupPointId();
          Boolean isInSameThreshold = variantEditValidationService.isSameThresholdL5(modifiedItemPickupPoint, categoryCode,
              productPriceStockAndImagesRequest.getItemSku(), wholesalePriceSkuResponseMap.get(wholesalePriceKey));
          modifiedItemPickupPoint.setWholesalePriceActivated(isInSameThreshold);
        } else {
          modifiedItemPickupPoint.setWholesalePriceActivated(modifiedItemPickupPoint.getWholesalePriceActivated());
        }
      }
    }
  }

  private void getWholesalePriceSkuResponseMapInBatches(
      List<ItemInfoDto> itemInfoDtoList, Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap)
      throws Exception {
    List<WholesalePriceSkuResponse> wholesalePriceSkuResponseList;
    if (CollectionUtils.isNotEmpty(itemInfoDtoList)) {
      List<List<ItemInfoDto>> itemInfoDtosPartitions =
          Lists.partition(itemInfoDtoList, wholesalePriceFetchBatchSize);
      for (List<ItemInfoDto> itemInfoDtosPartition : itemInfoDtosPartitions) {
        Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMapForPartition = new HashMap<>();
        wholesalePriceSkuResponseList =
            productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(itemInfoDtosPartition);
        wholesalePriceSkuResponseMapForPartition = wholesalePriceSkuResponseList.stream()
            .collect(Collectors.toMap(WholesalePriceSkuResponse::getItemPickupPointId, Function.identity()));
        wholesalePriceSkuResponseMap.putAll(wholesalePriceSkuResponseMapForPartition);
      }
    }
  }

  private List<ProductItemWholesalePrice> getProductItemWholesalePrices(String storeId,
      List<ProductPriceStockAndImagesRequest> successValidationVariantList,
      Map<String, ProductItemWholesalePrice> itemSkuToWholesalePriceMap,
      Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMap, List<Map<String, String>> historyAuditList)
      throws Exception {
    List<ProductItemWholesalePrice> productItemWholesalePrices = new ArrayList<>();
    for (ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      ProductItemBusinessPartner productItemBusinessPartner =
          productItemBusinessPartnerMap.get(productPriceStockAndImagesRequest.getItemSku());
      ProductItemWholesalePrice productItemWholesalePrice;
      if (itemSkuToWholesalePriceMap.containsKey(productPriceStockAndImagesRequest.getItemSku())) {
        productItemWholesalePrice = itemSkuToWholesalePriceMap.get(productPriceStockAndImagesRequest.getItemSku());
        String newWholesalePrice =
            mapperUtil.mapRequestToString(productPriceStockAndImagesRequest.getProductItemWholesalePrices());
        if (!StringUtils.equals(productItemWholesalePrice.getWholesaleRules(), newWholesalePrice)) {
          Map<String, String> historyMap = new HashMap<>();
          historyMap.put(PREVIOUS_VALUE, productItemWholesalePrice.getWholesaleRules());
          historyMap.put(CURRENT_VALUE, newWholesalePrice);
          historyMap.put(ACTIVITY, UpdateProductActivity.UPDATE_WHOLE_SALE_RULES.getDesc() + Constants.NEED_REVISION);
          historyMap.put(ITEM_SKU, productItemBusinessPartner.getGdnProductItemSku());
          historyAuditList.add(historyMap);
          productItemWholesalePrice.setWholesaleRules(newWholesalePrice);
        }
        if (Boolean.compare(productItemWholesalePrice.isWholesalePriceActivated(),
            productPriceStockAndImagesRequest.getWholesalePriceActivated()) != 0) {
          Map<String, String> historyMap = new HashMap<>();
          historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemWholesalePrice.isWholesalePriceActivated()));
          historyMap.put(CURRENT_VALUE, String.valueOf(productPriceStockAndImagesRequest.getWholesalePriceActivated()));
          historyMap.put(ACTIVITY, UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc() + Constants.NEED_REVISION);
          historyMap.put(ITEM_SKU, productItemBusinessPartner.getGdnProductItemSku());
          historyAuditList.add(historyMap);
          productItemWholesalePrice
              .setWholesalePriceActivated(productPriceStockAndImagesRequest.getWholesalePriceActivated());
        }
        productItemWholesalePrice.setUpdatedDate(new Date());
        productItemWholesalePrice.setUpdatePending(true);
        productItemWholesalePrices.add(productItemWholesalePrice);
      } else if (CollectionUtils.isNotEmpty(productPriceStockAndImagesRequest.getProductItemWholesalePrices())) {
        productItemWholesalePrice = new ProductItemWholesalePrice();
        Map<String, String> historyMap = new HashMap<>();
        historyMap.put(PREVIOUS_VALUE, StringUtils.EMPTY);
        historyMap.put(CURRENT_VALUE,
            mapperUtil.mapRequestToString(productPriceStockAndImagesRequest.getProductItemWholesalePrices()));
        historyMap.put(ACTIVITY, UpdateProductActivity.UPDATE_WHOLE_SALE_RULES.getDesc() + Constants.NEED_REVISION);
        historyMap.put(ITEM_SKU, productItemBusinessPartner.getGdnProductItemSku());
        historyAuditList.add(historyMap);
        productItemWholesalePrice.setItemCode(productPriceStockAndImagesRequest.getSkuCode());
        if (Objects.nonNull(productPriceStockAndImagesRequest.getWholesalePriceActivated())) {
          Map<String, String> historyWholesaleFlagMap = new HashMap<>();
          historyWholesaleFlagMap
              .put(PREVIOUS_VALUE, StringUtils.EMPTY);
          historyWholesaleFlagMap
              .put(CURRENT_VALUE, String.valueOf(productPriceStockAndImagesRequest.getWholesalePriceActivated()));
          historyWholesaleFlagMap
              .put(ACTIVITY, UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc() + Constants.NEED_REVISION);
          historyWholesaleFlagMap.put(ITEM_SKU, productItemBusinessPartner.getGdnProductItemSku());
          historyAuditList.add(historyWholesaleFlagMap);
          productItemWholesalePrice
              .setWholesalePriceActivated(productPriceStockAndImagesRequest.getWholesalePriceActivated());
        }
        productItemWholesalePrice.setProductItemId(productItemBusinessPartner.getProductItemId());
        productItemWholesalePrice.setItemSku(productPriceStockAndImagesRequest.getItemSku());
        productItemWholesalePrice.setStoreId(storeId);
        productItemWholesalePrice.setCreatedDate(new Date());
        productItemWholesalePrice.setUpdatedDate(new Date());
        productItemWholesalePrice.setWholesaleRules(
            mapperUtil.mapRequestToString(productPriceStockAndImagesRequest.getProductItemWholesalePrices()));
        productItemWholesalePrice.setUpdatePending(true);
        productItemWholesalePrices.add(productItemWholesalePrice);
      }
    }
    return productItemWholesalePrices;
  }

  private List<ProductItemWholesalePrice> getProductItemWholesalePricesL5(String storeId,
      List<ProductVariantPriceStockAndImagesRequest> successValidationVariantList,
      Map<String, ProductItemWholesalePrice> itemSkuToWholesalePriceMap,
      Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMap, List<Map<String, String>> historyAuditList,
      Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap) throws Exception {
    List<ProductItemWholesalePrice> productItemWholesalePrices = new ArrayList<>();
    for (ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      for (ItemPickupPointRequest modifiedItemPickupPoint : productPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
        ProductItemBusinessPartner productItemBusinessPartner = productItemBusinessPartnerMap.get(
            productPriceStockAndImagesRequest.getItemSku() + HYPHEN + modifiedItemPickupPoint.getPickupPointId());
        ProductItemWholesalePrice productItemWholesalePrice;
        if (itemSkuToWholesalePriceMap.containsKey(
            productPriceStockAndImagesRequest.getItemSku() + HYPHEN + modifiedItemPickupPoint.getPickupPointId())) {
          productItemWholesalePrice = itemSkuToWholesalePriceMap.get(
              productPriceStockAndImagesRequest.getItemSku() + HYPHEN + modifiedItemPickupPoint.getPickupPointId());
          String newWholesalePrice =
              mapperUtil.mapRequestToString(modifiedItemPickupPoint.getProductItemWholesalePriceRequests());
          if (!StringUtils.equals(productItemWholesalePrice.getWholesaleRules(), newWholesalePrice)) {
            Map<String, String> historyMap = new HashMap<>();
            historyMap.put(PREVIOUS_VALUE, productItemWholesalePrice.getWholesaleRules());
            historyMap.put(CURRENT_VALUE, newWholesalePrice);
            historyMap.put(ACTIVITY, UpdateProductActivity.UPDATE_WHOLE_SALE_RULES.getDesc() + Constants.NEED_REVISION);
            historyMap.put(ITEM_SKU, productItemBusinessPartner.getGdnProductItemSku());
            historyMap.put(PICKUP_POINT_CODE, productItemBusinessPartner.getPickupPointId());
            CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint,
                ITEM_NAME);
            historyAuditList.add(historyMap);
            productItemWholesalePrice.setWholesaleRules(newWholesalePrice);
          }
          if (Objects.nonNull(modifiedItemPickupPoint.getWholesalePriceActivated()) &&
              Boolean.compare(productItemWholesalePrice.isWholesalePriceActivated(),
                  modifiedItemPickupPoint.getWholesalePriceActivated()) != 0) {
            Map<String, String> historyMap = new HashMap<>();
            historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemWholesalePrice.isWholesalePriceActivated()));
            historyMap.put(CURRENT_VALUE, String.valueOf(modifiedItemPickupPoint.getWholesalePriceActivated()));
            historyMap.put(ACTIVITY, UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc() + Constants.NEED_REVISION);
            historyMap.put(ITEM_SKU, productItemBusinessPartner.getGdnProductItemSku());
            historyMap.put(PICKUP_POINT_CODE, productItemBusinessPartner.getPickupPointId());
            CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint,
                ITEM_NAME);
            historyAuditList.add(historyMap);
            productItemWholesalePrice.setWholesalePriceActivated(modifiedItemPickupPoint.getWholesalePriceActivated());
          }
          productItemWholesalePrice.setUpdatedDate(new Date());
          productItemWholesalePrice.setUpdatePending(true);
          productItemWholesalePrices.add(productItemWholesalePrice);
        } else if (CollectionUtils.isNotEmpty(modifiedItemPickupPoint.getProductItemWholesalePriceRequests())) {
          productItemWholesalePrice = new ProductItemWholesalePrice();
          Map<String, String> historyMap = new HashMap<>();
          CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint,
              ITEM_NAME);
          historyMap.put(PREVIOUS_VALUE, StringUtils.EMPTY);
          historyMap.put(CURRENT_VALUE,
              mapperUtil.mapRequestToString(modifiedItemPickupPoint.getProductItemWholesalePriceRequests()));
          historyMap.put(ACTIVITY, UpdateProductActivity.UPDATE_WHOLE_SALE_RULES.getDesc() + Constants.NEED_REVISION);
          historyMap.put(ITEM_SKU, productItemBusinessPartner.getGdnProductItemSku());
          historyMap.put(PICKUP_POINT_CODE, productItemBusinessPartner.getPickupPointId());
          historyAuditList.add(historyMap);
          productItemWholesalePrice.setItemCode(productPriceStockAndImagesRequest.getSkuCode());
          if (Objects.nonNull(modifiedItemPickupPoint.getWholesalePriceActivated())) {
            Map<String, String> historyWholesaleFlagMap = new HashMap<>();
            historyWholesaleFlagMap.put(PREVIOUS_VALUE, StringUtils.EMPTY);
            historyWholesaleFlagMap.put(CURRENT_VALUE,
                String.valueOf(modifiedItemPickupPoint.getWholesalePriceActivated()));
            historyWholesaleFlagMap.put(ACTIVITY,
                UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc() + Constants.NEED_REVISION);
            historyWholesaleFlagMap.put(ITEM_SKU, productItemBusinessPartner.getGdnProductItemSku());
            historyWholesaleFlagMap.put(PICKUP_POINT_CODE, productItemBusinessPartner.getPickupPointId());
            CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyWholesaleFlagMap,
                modifiedItemPickupPoint, ITEM_NAME);
            historyAuditList.add(historyWholesaleFlagMap);
            productItemWholesalePrice.setWholesalePriceActivated(modifiedItemPickupPoint.getWholesalePriceActivated());
          }
          productItemWholesalePrice.setProductItemId(productItemBusinessPartner.getProductItemId());
          productItemWholesalePrice.setItemSku(productPriceStockAndImagesRequest.getItemSku());
          productItemWholesalePrice.setPickupPointCode(modifiedItemPickupPoint.getPickupPointId());
          productItemWholesalePrice.setStoreId(storeId);
          productItemWholesalePrice.setCreatedDate(new Date());
          productItemWholesalePrice.setUpdatedDate(new Date());
          productItemWholesalePrice.setWholesaleRules(
              mapperUtil.mapRequestToString(modifiedItemPickupPoint.getProductItemWholesalePriceRequests()));
          productItemWholesalePrice.setUpdatePending(true);
          productItemWholesalePrice.setStoreId(storeId);
          productItemWholesalePrices.add(productItemWholesalePrice);
        }
      }
    }
    return productItemWholesalePrices;
  }

  private void createAuditLogsForPriceStockChanges(String businessPartnerCode, String itemSku, String productName,
      String productSku, List<Map<String, String>> historyAuditList) throws Exception {
    for (Map<String, String> historyAudit : historyAuditList) {
      if (Objects.isNull(itemSku)) {
        this.updatedProductHistoryService
            .createProductL3AuditLog(businessPartnerCode, historyAudit.get(ITEM_SKU), productSku, productName,
                historyAudit.get(ACTIVITY), historyAudit.get(PREVIOUS_VALUE), historyAudit.get(CURRENT_VALUE), false,
                StringUtils.EMPTY);
      } else {
        this.updatedProductHistoryService
            .createProductL3AuditLog(businessPartnerCode, itemSku, productSku, productName, historyAudit.get(ACTIVITY),
                historyAudit.get(PREVIOUS_VALUE), historyAudit.get(CURRENT_VALUE), false, StringUtils.EMPTY);
      }
    }
  }

  private void createAuditLogsForPriceStockChangesL5(String businessPartnerCode, String itemSku, String productSku,
      List<Map<String, String>> historyAuditList, String pickupPointCode) throws Exception {
    for (Map<String, String> historyAudit : historyAuditList) {
      if (Objects.isNull(itemSku)) {
        this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, historyAudit.get(ITEM_SKU),
            productSku, historyAudit.get(ITEM_NAME), historyAudit.get(ACTIVITY), historyAudit.get(PREVIOUS_VALUE),
            historyAudit.get(CURRENT_VALUE), false, historyAudit.get(PICKUP_POINT_CODE));
      } else {
        this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, itemSku, productSku,
            historyAudit.get(ITEM_NAME), historyAudit.get(ACTIVITY), historyAudit.get(PREVIOUS_VALUE),
            historyAudit.get(CURRENT_VALUE), false, pickupPointCode);
      }
    }
  }

  private boolean regenerateProductItemBusinessPartnerByProductPriceStockAndImagesRequest(
      ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest,
      ProductItemBusinessPartner productItemBusinessPartner, String businessPartnerCode,
      List<Map<String, String>> historyAuditMap) throws Exception {
    boolean isChanged = false;
    if (!StringUtils
        .equals(productPriceStockAndImagesRequest.getMerchantSku(), productItemBusinessPartner.getMerchantSku())) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, productItemBusinessPartner.getMerchantSku());
      historyMap.put(CURRENT_VALUE, productPriceStockAndImagesRequest.getMerchantSku());
      historyMap.put(ACTIVITY, UpdateProductActivity.MERCHANT_SKU.getDesc() + Constants.NEED_REVISION);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setMerchantSku(productPriceStockAndImagesRequest.getMerchantSku());
      isChanged = true;
    }
    if (Objects.nonNull(productPriceStockAndImagesRequest.getDeltaStock())) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.getStock()));
      historyMap.put(CURRENT_VALUE, String.valueOf(productPriceStockAndImagesRequest.getAvailableStockLevel2()));
      historyMap.put(ACTIVITY, UpdateProductActivity.STOCK_VALUE.getDesc() + Constants.NEED_REVISION);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setStock(productPriceStockAndImagesRequest.getAvailableStockLevel2());
      isChanged = true;
    }
    if (!StringUtils.equals(String.valueOf(productItemBusinessPartner.getMinimumStock()),
        String.valueOf(productPriceStockAndImagesRequest.getMinimumStockLevel2()))) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.getMinimumStock()));
      historyMap.put(CURRENT_VALUE, String.valueOf(productPriceStockAndImagesRequest.getMinimumStockLevel2()));
      historyMap.put(ACTIVITY, UpdateProductActivity.MINIMUM_STOCK.getDesc() + Constants.NEED_REVISION);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setMinimumStock(productPriceStockAndImagesRequest.getMinimumStockLevel2());
      isChanged = true;
    }
    if (Boolean.compare(productItemBusinessPartner.isDisplay(),
        productPriceStockAndImagesRequest.getViewConfigs().get(0).getDisplay()) != 0) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isDisplay()));
      historyMap.put(CURRENT_VALUE, String.valueOf(productPriceStockAndImagesRequest.getViewConfigs().get(0).getDisplay()));
      historyMap.put(ACTIVITY, UpdateProductActivity.DISPLAYABLE.getDesc() + Constants.NEED_REVISION);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setDisplay(productPriceStockAndImagesRequest.getViewConfigs().get(0).getDisplay());
      isChanged = true;
    }
    if (Boolean.compare(productItemBusinessPartner.isBuyable(),
        productPriceStockAndImagesRequest.getViewConfigs().get(0).getBuyable()) != 0) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isBuyable()));
      historyMap.put(CURRENT_VALUE, String.valueOf(productPriceStockAndImagesRequest.getViewConfigs().get(0).getBuyable()));
      historyMap.put(ACTIVITY, UpdateProductActivity.BUYABLE.getDesc() + Constants.NEED_REVISION);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setBuyable(productPriceStockAndImagesRequest.getViewConfigs().get(0).getBuyable());
      isChanged = true;
    }
    if (productItemBusinessPartner.getPrice().doubleValue() != productPriceStockAndImagesRequest.getPrices().get(0)
        .getPrice().doubleValue()) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.getPrice()));
      historyMap.put(CURRENT_VALUE, String.valueOf(productPriceStockAndImagesRequest.getPrices().get(0).getPrice()));
      historyMap.put(ACTIVITY, UpdateProductActivity.NORMAL_PRICE.getDesc() + Constants.NEED_REVISION);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setPrice(productPriceStockAndImagesRequest.getPrices().get(0).getPrice());
      isChanged = true;
    }
    if (productItemBusinessPartner.getSalePrice().doubleValue() != productPriceStockAndImagesRequest.getPrices().get(0)
        .getSalePrice().doubleValue()) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.getSalePrice()));
      historyMap.put(CURRENT_VALUE, String.valueOf(productPriceStockAndImagesRequest.getPrices().get(0).getSalePrice()));
      historyMap.put(ACTIVITY, UpdateProductActivity.SELLING_PRICE.getDesc() + Constants.NEED_REVISION);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setSalePrice(productPriceStockAndImagesRequest.getPrices().get(0).getSalePrice());
      isChanged = true;
    }
    return isChanged;
  }

  private FbbAndCncDataChangeDto regenerateProductItemBusinessPartnerByProductPriceStockAndImagesRequestL5(
      ProductItemBusinessPartner productItemBusinessPartner, List<Map<String, String>> historyAuditMap,
      ItemPickupPointRequest modifiedItemPickupPoint, Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap)
      throws Exception {
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    if(Objects.nonNull(modifiedItemPickupPoint.getSellerSku())) {
      productItemBusinessPartner.setMerchantSku(modifiedItemPickupPoint.getSellerSku());
    }
    if (!cncForWarehouseFeatureSwitch && modifiedItemPickupPoint.isCncActive() != productItemBusinessPartner.isCncActivated()) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isCncActivated()));
      historyMap.put(CURRENT_VALUE, String.valueOf(modifiedItemPickupPoint.isCncActive()));
      historyMap.put(ACTIVITY, UpdateProductActivity.CNC.getDesc() + Constants.NEED_REVISION);
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setCncActivated(modifiedItemPickupPoint.isCncActive());
      fbbAndCncDataChangeDto.setCncChanged(Boolean.TRUE);
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
    }
    if (Objects.nonNull(modifiedItemPickupPoint.getFbbActive())
      && modifiedItemPickupPoint.getFbbActive() != productItemBusinessPartner.isFbbActive()) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isFbbActive()));
      historyMap.put(CURRENT_VALUE, String.valueOf(modifiedItemPickupPoint.getFbbActive()));
      if (modifiedItemPickupPoint.getFbbActive()) {
        historyMap.put(ACTIVITY, UpdateProductActivity.FBB_ACTIVATED.getDesc() + Constants.NEED_REVISION);
        productItemBusinessPartner.setFbbActive(modifiedItemPickupPoint.getFbbActive());
      } else {
        historyMap.put(ACTIVITY, UpdateProductActivity.FBB_DEACTIVATED.getDesc() + Constants.NEED_REVISION);
        productItemBusinessPartner.setFbbActive(modifiedItemPickupPoint.getFbbActive());
        productItemBusinessPartner.setMarkForDelete(Boolean.TRUE);
      }
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      LOGGER.info("HistoryAuditMap updated for fbb flag for itemSku : {} {}",
          productItemBusinessPartner.getGdnProductItemSku(), historyAuditMap);
      fbbAndCncDataChangeDto.setFbbChanged(Boolean.TRUE);
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
    }
    if (Objects.nonNull(modifiedItemPickupPoint.getStock()) && modifiedItemPickupPoint.getStock() != 0) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.getStock()));
      historyMap.put(CURRENT_VALUE,
          productItemBusinessPartner.getStock() + String.valueOf(modifiedItemPickupPoint.getStock()));
      historyMap.put(ACTIVITY, UpdateProductActivity.STOCK_VALUE.getDesc() + Constants.NEED_REVISION);
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setStock(productItemBusinessPartner.getStock() + modifiedItemPickupPoint.getStock());
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
    }
    if (Objects.nonNull(modifiedItemPickupPoint.getInitialPreOrderQuota()) && modifiedItemPickupPoint.getInitialPreOrderQuota() != 0) {
      Map<String, String> historyMap = new HashMap<>();
      int existingPreOrderQuota = java.util.Optional.ofNullable(productItemBusinessPartner.getPreOrderQuota()).orElse(0);
      int deltaPreOrderQuota = modifiedItemPickupPoint.getInitialPreOrderQuota();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(existingPreOrderQuota));
      historyMap.put(CURRENT_VALUE, String.valueOf(existingPreOrderQuota + deltaPreOrderQuota));
      historyMap.put(ACTIVITY, UpdateProductActivity.PRE_ORDER_QUOTA.getDesc() + Constants.NEED_REVISION);
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setPreOrderQuota(existingPreOrderQuota + deltaPreOrderQuota);
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
    }
    if (!StringUtils.equals(String.valueOf(productItemBusinessPartner.getMinimumStock()),
        String.valueOf(modifiedItemPickupPoint.getMinimumStock()))) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.getMinimumStock()));
      historyMap.put(CURRENT_VALUE, String.valueOf(modifiedItemPickupPoint.getMinimumStock()));
      historyMap.put(ACTIVITY, UpdateProductActivity.MINIMUM_STOCK.getDesc() + Constants.NEED_REVISION);
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setMinimumStock(modifiedItemPickupPoint.getMinimumStock());
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
    }
    if (Boolean.compare(productItemBusinessPartner.isDisplay(), modifiedItemPickupPoint.isDisplay()) != 0) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isDisplay()));
      historyMap.put(CURRENT_VALUE, String.valueOf(modifiedItemPickupPoint.isDisplay()));
      historyMap.put(ACTIVITY, UpdateProductActivity.DISPLAYABLE.getDesc() + Constants.NEED_REVISION);
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setDisplay(modifiedItemPickupPoint.isDisplay());
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
    }
    if (Boolean.compare(productItemBusinessPartner.isBuyable(), modifiedItemPickupPoint.isBuyable()) != 0) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isBuyable()));
      historyMap.put(CURRENT_VALUE, String.valueOf(modifiedItemPickupPoint.isBuyable()));
      historyMap.put(ACTIVITY, UpdateProductActivity.BUYABLE.getDesc() + Constants.NEED_REVISION);
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setBuyable(modifiedItemPickupPoint.isBuyable());
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
    }
    if (cncForWarehouseFeatureSwitch && Boolean.compare(productItemBusinessPartner.isCncDiscoverable(), modifiedItemPickupPoint.isCncDisplay()) != 0) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isCncDiscoverable()));
      historyMap.put(CURRENT_VALUE, String.valueOf(modifiedItemPickupPoint.isCncDisplay()));
      historyMap.put(ACTIVITY, UpdateProductActivity.CNC_DISPLAYABLE.getDesc() + Constants.NEED_REVISION);
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setCncDiscoverable(modifiedItemPickupPoint.isCncDisplay());
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
      fbbAndCncDataChangeDto.setCncChanged(Boolean.TRUE);
    }
    if (cncForWarehouseFeatureSwitch && Boolean.compare(productItemBusinessPartner.isCncBuyable(), modifiedItemPickupPoint.isCncBuyable()) != 0) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isCncBuyable()));
      historyMap.put(CURRENT_VALUE, String.valueOf(modifiedItemPickupPoint.isCncBuyable()));
      historyMap.put(ACTIVITY, UpdateProductActivity.CNC_BUYABLE.getDesc() + Constants.NEED_REVISION);
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setCncBuyable(modifiedItemPickupPoint.isCncBuyable());
      fbbAndCncDataChangeDto.setCncChanged(Boolean.TRUE);
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
    }
    if (productItemBusinessPartner.getPrice().doubleValue() != modifiedItemPickupPoint.getPrice().doubleValue()) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.getPrice()));
      historyMap.put(CURRENT_VALUE, String.valueOf(modifiedItemPickupPoint.getPrice()));
      historyMap.put(ACTIVITY, UpdateProductActivity.NORMAL_PRICE.getDesc() + Constants.NEED_REVISION);
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setPrice(modifiedItemPickupPoint.getPrice());
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
    }
    if (productItemBusinessPartner.getSalePrice().doubleValue() != modifiedItemPickupPoint.getSalePrice()
        .doubleValue()) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.getSalePrice()));
      historyMap.put(CURRENT_VALUE, String.valueOf(modifiedItemPickupPoint.getSalePrice()));
      historyMap.put(ACTIVITY, UpdateProductActivity.SELLING_PRICE.getDesc() + Constants.NEED_REVISION);
      CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
      historyAuditMap.add(historyMap);
      productItemBusinessPartner.setSalePrice(modifiedItemPickupPoint.getSalePrice());
      fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
    }
    if (Objects.nonNull(modifiedItemPickupPoint.getB2bFields())) {
      B2BFields b2bFields = modifiedItemPickupPoint.getB2bFields();
      if (Objects.isNull(productItemBusinessPartner.getB2bPrice())
          || Double.compare(productItemBusinessPartner.getB2bPrice(), b2bFields.getPrice()) != 0) {
        setB2bPriceHistory(productItemBusinessPartner, historyAuditMap, modifiedItemPickupPoint, l5CodeAndResponseMap,
            fbbAndCncDataChangeDto, b2bFields.getPrice());
      }
      if (productItemBusinessPartner.isB2bBuyable() != b2bFields.isBuyable()) {
        setB2bBuyableHistory(productItemBusinessPartner, historyAuditMap, modifiedItemPickupPoint,
            l5CodeAndResponseMap, fbbAndCncDataChangeDto, b2bFields.isBuyable());
      }
      if (productItemBusinessPartner.isB2bDiscoverable() != b2bFields.isDisplay()) {
        setB2bDiscoverableHistory(productItemBusinessPartner, historyAuditMap, modifiedItemPickupPoint,
            l5CodeAndResponseMap, fbbAndCncDataChangeDto, b2bFields.isDisplay());
      }
      if (productItemBusinessPartner.isB2bManaged() != b2bFields.isManaged()) {
        setB2bManagedHistory(productItemBusinessPartner, historyAuditMap, modifiedItemPickupPoint,
            l5CodeAndResponseMap, fbbAndCncDataChangeDto, b2bFields.isManaged());
      }
    }
    return fbbAndCncDataChangeDto;
  }

  private static void setB2bManagedHistory(ProductItemBusinessPartner productItemBusinessPartner,
      List<Map<String, String>> historyAuditMap, ItemPickupPointRequest modifiedItemPickupPoint,
      Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap, FbbAndCncDataChangeDto fbbAndCncDataChangeDto,
      Boolean b2bManaged) {
    Map<String, String> historyMap = new HashMap<>();
    historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isB2bManaged()));
    historyMap.put(CURRENT_VALUE, String.valueOf(b2bManaged));
    historyMap.put(ACTIVITY, UpdateProductActivity.B2B_MANAGED.getDesc() + Constants.NEED_REVISION);
    CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
    historyAuditMap.add(historyMap);
    productItemBusinessPartner.setB2bManaged(b2bManaged);
    fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
  }

  private static void setB2bDiscoverableHistory(ProductItemBusinessPartner productItemBusinessPartner,
      List<Map<String, String>> historyAuditMap, ItemPickupPointRequest modifiedItemPickupPoint,
      Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap, FbbAndCncDataChangeDto fbbAndCncDataChangeDto,
      Boolean b2bDisplayable) {
    Map<String, String> historyMap = new HashMap<>();
    historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isB2bDiscoverable()));
    historyMap.put(CURRENT_VALUE, String.valueOf(b2bDisplayable));
    historyMap.put(ACTIVITY, UpdateProductActivity.B2B_DISCOVERABLE.getDesc() + Constants.NEED_REVISION);
    CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
    historyAuditMap.add(historyMap);
    productItemBusinessPartner.setB2bDiscoverable(b2bDisplayable);
    fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
  }

  private static void setB2bBuyableHistory(ProductItemBusinessPartner productItemBusinessPartner,
      List<Map<String, String>> historyAuditMap, ItemPickupPointRequest modifiedItemPickupPoint,
      Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap, FbbAndCncDataChangeDto fbbAndCncDataChangeDto,
      Boolean b2bBuyable) {
    Map<String, String> historyMap = new HashMap<>();
    historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.isB2bBuyable()));
    historyMap.put(CURRENT_VALUE, String.valueOf(b2bBuyable));
    historyMap.put(ACTIVITY, UpdateProductActivity.B2B_BUYABLE.getDesc() + Constants.NEED_REVISION);
    CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
    historyAuditMap.add(historyMap);
    productItemBusinessPartner.setB2bBuyable(b2bBuyable);
    fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
  }

  private static void setB2bPriceHistory(ProductItemBusinessPartner productItemBusinessPartner,
      List<Map<String, String>> historyAuditMap, ItemPickupPointRequest modifiedItemPickupPoint,
      Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap, FbbAndCncDataChangeDto fbbAndCncDataChangeDto,
      Double b2bPrice) {
    Map<String, String> historyMap = new HashMap<>();
    historyMap.put(PREVIOUS_VALUE, String.valueOf(productItemBusinessPartner.getB2bPrice()));
    historyMap.put(CURRENT_VALUE, String.valueOf(b2bPrice));
    historyMap.put(ACTIVITY, UpdateProductActivity.B2B_PRICE.getDesc() + Constants.NEED_REVISION);
    CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
    historyAuditMap.add(historyMap);
    productItemBusinessPartner.setB2bPrice(b2bPrice);
    fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
  }

  private Map<String, String> getPickUpPointByProductSku(List<String> itemSkus, int size, boolean fbbActivated)
      throws Exception {
    ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
    itemSummaryRequest.setItemSkus(itemSkus);
    itemSummaryRequest.setFbbActivated(fbbActivated);
    Map<String, String> itemXOldPickUpPoint = new HashMap<>();
    Page<ItemSummaryResponse> itemSummaryResponses =
        productLevel3Repository.findSummaryByFilter(itemSummaryRequest, PageRequest.of(0, size), new SortOrder());
    if (CollectionUtils.isNotEmpty(itemSummaryResponses.getContent())) {
      itemXOldPickUpPoint = new HashMap<>(itemSummaryResponses.getContent().stream().filter(Predicate.not(ItemSummaryResponse::isMarkForDelete))
          .collect(Collectors.toMap(ItemSummaryResponse::getItemSku, ItemSummaryResponse::getPickupPointCode)));
    }
    return itemXOldPickUpPoint;
  }

  public Map<String, Pair<String, String>> getPickUpPointByItemSku(List<String> itemSkus, int batchSize) throws Exception {
    List<List<String>> partitionedItemSkuList = Lists.partition(itemSkus, batchSize);
    List<ItemResponseV2> itemResponses = new ArrayList<>();
    for (List<String> itemSkuList : partitionedItemSkuList) {
      ItemRequestV2 itemRequestV2 = new ItemRequestV2();
      itemRequestV2.setItemSkuList(itemSkuList);
      itemResponses.addAll(xProductOutbound.getItemPickupPointsByItemSku(itemRequestV2));
    }
    return new HashMap<>(ofNullable(itemResponses).orElse(new ArrayList<>()).stream().collect(
        Collectors.toMap(ItemResponseV2::getItemSku,
            itemResponseV2 -> Pair.of(itemResponseV2.getPickUpPointCode(), itemResponseV2.getSkuCode()),
            (itemSku1, itemSku2) -> itemSku1)));
  }

  @Override
  public PickupPointUpdateResponse updatePickupPointCodes(PickupPointUpdateRequest request) throws Exception {
    LOGGER.info("Updating pickup point for product sku : {} : request {}", request.getProductSku(), request);
    Long L3version = null;
    if (!Boolean.TRUE.equals(request.getFbbActivated())) {
      checkArgument(isEligibleForPickupPointUpdate(request.getBusinessPartnerCode()),
          String.format(ErrorMessages.BUSINESS_PARTNER_NOT_ELIGIBLE_FOR_PICKUP_POINT_UPDATE, request.getBusinessPartnerCode()));
    }
    if (request.isNeedCorrection()) {
      return updatePickupPointCodesForNeedCorrection(request);
    }

    PickupPointUpdateResponse pickupPointUpdateResponse1 = getPickupPointUpdateResponse(request);
    if (Objects.nonNull(pickupPointUpdateResponse1))
      return pickupPointUpdateResponse1;

    ProductL3Response productL3Response =
        xProductOutbound.getProductDetailsByProductSku(request.getProductSku()).getValue();
    if (request.isFbbMigration()) {
      productL3Response.setArchived(false);
      productL3Response.setSuspended(false);
      productL3Response.setTakenDown(false);
    }
    ApiErrorCode apiErrorCode = checkProductStatus(productL3Response);
    if (Objects.nonNull(apiErrorCode)) {
      LOGGER.error("Product sku not in correct state : {} , error message : {}", request.getProductSku(),
          apiErrorCode.getDesc());
      PickupPointUpdateResponse pickupPointUpdateResponse = new PickupPointUpdateResponse();
      pickupPointUpdateResponse.setApiErrorCode(apiErrorCode);
      return pickupPointUpdateResponse;
    }
    PickupPointUpdateResponse pickupPointUpdateResponse = null;
    Map<String, String> pickupPointCodesByItemSku = request.getItemsPickupPoint().stream()
        .collect(Collectors.toMap(PickupPointRequest::getItemSku,
            PickupPointRequest::getPickupPointCode, (itemSku1, itemSku2) -> itemSku1));
    int size = Integer.valueOf(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, ITEM_SUMMARY_PAGE_SIZE).getValue());
    if (request.isDifferentLocation()) {
      Map<String, Pair<String, String>> itemXOldPickUpPoint =
          getPickUpPointByItemSku(new ArrayList<>(pickupPointCodesByItemSku.keySet()), size);
      L3version = updatePickupPointsInXProduct(pickupPointCodesByItemSku, itemXOldPickUpPoint, request.getProductSku(),
          request.getBusinessPartnerCode(), request.getFbbActivated());
      WebInventoryUpdatePickupPointResponseDTO updatePickupPointResponse =
          updatePickupPointInInventory(request.getBusinessPartnerCode(), request.getProductSku(),
              ConverterUtil.toWebInventoryUpdatePickupPointRequestDTO(request, itemXOldPickUpPoint).getList(),
              request.getFbbActivated(), itemXOldPickUpPoint);
      pickupPointUpdateResponse = validateInventoryResponse(updatePickupPointResponse);
      pickupPointUpdateResponse.setL3version(L3version);
    } else {
      List<ItemBasicDetailV2Response> itemBasicDetailV2Responses = xProductOutbound
          .findItemBasicDetailsByProductSku(request.getProductSku());
      if (CollectionUtils.isNotEmpty(itemBasicDetailV2Responses)) {
        Map<String, Pair<String, String>> itemXOldPickUpPoint =
            getPickUpPointByItemSku(ofNullable(itemBasicDetailV2Responses)
                .orElse(new ArrayList<>()).stream().map(ItemBasicDetailV2Response::getItemSku).collect(
                    Collectors.toList()), size);
        pickupPointUpdateResponse = updatePickupPointByProductSkuMpp(request.getProductSku(),
            request.getBusinessPartnerCode(), request.getItemsPickupPoint().get(0).getPickupPointCode(),
            Objects.nonNull(request.getFbbActivated()) ? request.getFbbActivated() : false,
            itemXOldPickUpPoint, size);
        }
    }
    return pickupPointUpdateResponse;
  }

  private PickupPointUpdateResponse getPickupPointUpdateResponse(PickupPointUpdateRequest request) {
    try {
      Set<String> uniquePickupPointCodes = new HashSet<>();
      for (PickupPointRequest pickupPointRequest : request.getItemsPickupPoint()) {
        uniquePickupPointCodes.add(pickupPointRequest.getPickupPointCode());
      }
      List<PickupPointResponse> pickupPointResponseList =
          pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID,
              new ArrayList<>(uniquePickupPointCodes));
      if (setWaitingDeletionForDeletePickupPoint) {
        CommonUtils.validatePickupPointsAndWaitingDeletion(request.getProductSku(), uniquePickupPointCodes,
            pickupPointResponseList, new SimpleStringResponse(), request.getBusinessPartnerCode());
      } else {
        CommonUtils.validatePickupPoints(request.getProductSku(), uniquePickupPointCodes, pickupPointResponseList,
            new SimpleStringResponse(), request.getBusinessPartnerCode());
      }
    } catch (Exception e) {
      LOGGER.error("pickup point is invalid : {} , error message : {}",
          request.getItemsPickupPoint().get(0).getPickupPointCode(), new SimpleStringResponse());
      PickupPointUpdateResponse pickupPointUpdateResponse = new PickupPointUpdateResponse();
      pickupPointUpdateResponse.setApiErrorCode(ApiErrorCode.PICKUP_POINT_IS_NOT_VALID);
      return pickupPointUpdateResponse;
    }
    return null;
  }

  private PickupPointUpdateResponse updatePickupPointCodesForNeedCorrection(PickupPointUpdateRequest request) throws Exception {
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerRepository.findFirstByGdnProductSku(request.getProductSku());
    if (Objects.isNull(productBusinessPartner)) {
      LOGGER.error("ProductSku not found : {}", request.getProductSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "productSku not found");
    }
    ProductCollection productCollection = productCollectionRepository
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(productBusinessPartner.getStoreId(), productBusinessPartner.getProductId());
    List<ProductItemBusinessPartner> productItemBusinessPartnerList =
        this.productItemBusinessPartnerRepository.findByStoreIdAndProductBusinessPartnerId(DEFAULT_STORE_ID,
            productBusinessPartner.getId());
    Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    if (Objects.nonNull(request.getFbbActivated()) ? request.getFbbActivated() : false) {
      productItemBusinessPartnerMap =
          productItemBusinessPartnerList.stream().filter(ProductItemBusinessPartner::isFbbActive).collect(Collectors
              .toMap(ProductItemBusinessPartner::getGdnProductItemSku,
                  productItemBusinessPartner -> productItemBusinessPartner, (a, b) -> a));
      itemXOldPickUpPointMap = productItemBusinessPartnerList.stream().filter(ProductItemBusinessPartner::isFbbActive)
          .filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete)).collect(Collectors
              .toMap(ProductItemBusinessPartner::getGdnProductItemSku, ProductItemBusinessPartner::getPickupPointId));
    } else {
      productItemBusinessPartnerMap = productItemBusinessPartnerList.stream().collect(Collectors
          .toMap(ProductItemBusinessPartner::getGdnProductItemSku,
              productItemBusinessPartner -> productItemBusinessPartner, (a, b) -> a));
      itemXOldPickUpPointMap =
          productItemBusinessPartnerList.stream().filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete)).collect(
              Collectors.toMap(ProductItemBusinessPartner::getGdnProductItemSku,
                  ProductItemBusinessPartner::getPickupPointId));
    }
    List<String> itemSkus;
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    if (request.isDifferentLocation()) {
      itemSkus = request.getItemsPickupPoint().stream().map(PickupPointRequest::getItemSku).collect(Collectors.toList());
      itemXNewPickUpPointMap = request.getItemsPickupPoint().stream()
          .filter(pickupPointRequest -> itemSkus.contains(pickupPointRequest.getItemSku()))
          .collect(Collectors.toMap(PickupPointRequest::getItemSku, PickupPointRequest::getPickupPointCode));
    } else {
      itemSkus = new ArrayList<>(itemXOldPickUpPointMap.keySet());
      for (String itemSku : itemSkus) {
        itemXNewPickUpPointMap.put(itemSku, request.getItemsPickupPoint().get(0).getPickupPointCode());
      }
    }
    if (productCollection.isPostLive() || productCollection.isEdited()) {
      WebInventoryUpdatePickupPointResponseDTO response =
          inventoryOutbound.updatePickupPoint(generateMandatoryRequestParam(),
              ConverterUtil.toWebInventoryUpdatePickupPointRequestDTO(request.getBusinessPartnerCode(),
                  itemXNewPickUpPointMap, itemXOldPickUpPointMap));
      for (WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error : ofNullable(
          response.getErrors()).orElse(new ArrayList<>())) {
        itemSkus.remove(error.getWebItemSku());
      }
    }
    updateWholesaleEntriesOnPickupPointChange(productBusinessPartner.getStoreId(), itemXOldPickUpPointMap,
        itemXNewPickUpPointMap, productBusinessPartner.getState());
    updateProductItemBusinessPartner(itemSkus, itemXNewPickUpPointMap, productItemBusinessPartnerList,
        Objects.nonNull(request.getFbbActivated()) ? request.getFbbActivated() : false);
    for (String itemSku : itemSkus) {
      this.createAuditLogsForPickupPointUpdate(request.getBusinessPartnerCode(), itemSku,
          UpdateProductActivity.PICK_POINT_CODE.getDesc(), itemXOldPickUpPointMap.get(itemSku),
          itemXNewPickUpPointMap.get(itemSku), request.getProductSku(), true,
          productItemBusinessPartnerMap.get(itemSku).getProductItemId());
    }
    return PickupPointUpdateResponse.builder().build();
  }

  public void updateWholesaleEntriesOnPickupPointChange(String storeId, Map<String, String> itemXOldPickUpPointMap,
    Map<String, String> itemXNewPickUpPointMap, String productState) {
    List<ProductItemWholesalePrice> updatedWholesaleEntry = new ArrayList<>();
    for (Map.Entry<String, String> mapEntry : itemXOldPickUpPointMap.entrySet()) {
      ProductItemWholesalePrice wholesaleL5Entry = null;
      if (wholesaleL5EntryByStoreId) {
        if (!StringUtils.equals(productState, PRODUCT_STATE_ACTIVE)) {
          wholesaleL5Entry = this.productItemWholesalePriceService.findOneByStoreIdAndItemSkuAndPickupPointCode(storeId,
              mapEntry.getKey(), mapEntry.getValue());
        }
      } else {
        wholesaleL5Entry = this.productItemWholesalePriceService.findOneByItemSkuAndPickupPointCode(mapEntry.getKey(),
            mapEntry.getValue());
      }
      if (Objects.nonNull(wholesaleL5Entry)) {
        wholesaleL5Entry.setPickupPointCode(itemXNewPickUpPointMap.get(mapEntry.getKey()));
        updatedWholesaleEntry.add(wholesaleL5Entry);
      }
    }
    if (CollectionUtils.isNotEmpty(updatedWholesaleEntry)) {
      this.productItemWholesalePriceService.saveWholesalePriceNew(updatedWholesaleEntry);
    }
  }

  private void updateProductItemBusinessPartner(List<String> itemSkus, Map<String, String> itemXNewPickUpPointMap,
      List<ProductItemBusinessPartner> productItemBusinessPartnerList, boolean fbbActivated) {
    Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMapMfdFalse = new HashMap<>();
    if (fbbActivated) {
      productItemBusinessPartnerMapMfdFalse =
          productItemBusinessPartnerList.stream().filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete))
              .filter(ProductItemBusinessPartner::isFbbActive)
              .collect(Collectors.toMap(ProductItemBusinessPartner::getGdnProductItemSku, Function.identity()));
    } else {
      productItemBusinessPartnerMapMfdFalse =
          productItemBusinessPartnerList.stream().filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete))
              .collect(Collectors.toMap(ProductItemBusinessPartner::getGdnProductItemSku, Function.identity()));
    }
    Map<String, Map<String, ProductItemBusinessPartner>> itemSkuPpCodeProductItemBusinessPartnerMap = new HashMap<>();
    for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartnerList) {
      Map<String, ProductItemBusinessPartner> ppCodeProductItemBusinessPartnerMap = itemSkuPpCodeProductItemBusinessPartnerMap
          .getOrDefault(productItemBusinessPartner.getGdnProductItemSku(), new HashMap<>());
      ppCodeProductItemBusinessPartnerMap.put(productItemBusinessPartner.getPickupPointId(), productItemBusinessPartner);
      itemSkuPpCodeProductItemBusinessPartnerMap.put(productItemBusinessPartner.getGdnProductItemSku(), ppCodeProductItemBusinessPartnerMap);
    }
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    for (String itemSku : itemSkus) {
      String pickupPointCode = itemXNewPickUpPointMap.get(itemSku);
      ProductItemBusinessPartner productItemBusinessPartner =
          itemSkuPpCodeProductItemBusinessPartnerMap.get(itemSku).getOrDefault(pickupPointCode, new ProductItemBusinessPartner());
      ProductItemBusinessPartner productItemBusinessPartnerOld = productItemBusinessPartnerMapMfdFalse.get(itemSku);
      if (!productItemBusinessPartnerOld.getId().equals(productItemBusinessPartner.getId())) {
        BeanUtils.copyProperties(productItemBusinessPartnerOld, productItemBusinessPartner,
            "id", "version", "pickupPointId", "createdBy", "createdDate");
        productItemBusinessPartnerOld.setMarkForDelete(true);
        productItemBusinessPartner.setMarkForDelete(false);
        productItemBusinessPartner.setPickupPointId(pickupPointCode);
        productItemBusinessPartners.add(productItemBusinessPartnerOld);
        productItemBusinessPartners.add(productItemBusinessPartner);
      }
    }
    productItemBusinessPartnerRepository.saveAll(productItemBusinessPartners);
  }

  @Override
  public Page<ProductItemNameResponse> getItemSummaryResponseByProductSku(String productSku, int page, int size)
      throws Exception {
    List<ProductItemNameResponse> productItemNameResponses = new ArrayList<>();
    Page<ItemSummaryResponse> responses = getItemSummaryResponseSortByIsArchived(productSku, page, size);
    if (CollectionUtils.isNotEmpty(responses.getContent())) {
      for (ItemSummaryResponse itemSummaryResponse : responses.getContent()) {
        ProductItemNameResponse productItemNameResponse =
            ProductItemNameResponse.builder().itemSku(itemSummaryResponse.getItemSku())
                .itemName(itemSummaryResponse.getGeneratedItemName()).build();
        productItemNameResponses.add(productItemNameResponse);
      }
    }
    return new PageImpl<>(productItemNameResponses, PageRequest.of(page, size), responses.getTotalElements());
  }

  private MandatoryRequestParam generateMandatoryRequestParam()
      throws Exception {
    return MandatoryRequestParam
        .generateMandatoryRequestParam(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            GdnMandatoryRequestParameterUtil.getAuthenticator());
  }

  public PickupPointUpdateResponse updatePickupPointByProductSku(String productSku, String businessPartnerCode, String pickupPointCode)
      throws Exception {
    PickupPointUpdateResponse pickupPointUpdateResponse = new PickupPointUpdateResponse();
    Page<ItemSummaryResponse> itemSummaryResponses;
    ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
    itemSummaryRequest.setProductSkus(Arrays.asList(productSku));
    int page = 0;
    int size = Integer.valueOf(
        productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, ITEM_SUMMARY_PAGE_SIZE)
            .getValue());
    do {
      List<String> itemSkus = new ArrayList<>();
      Map<String, String> itemXOldPickUpPoint = new HashMap<>();
      itemSummaryResponses =
          productLevel3Repository.findSummaryByFilter(itemSummaryRequest, PageRequest.of(page, size), new SortOrder());
      if (CollectionUtils.isNotEmpty(itemSummaryResponses.getContent())) {
        itemSkus.addAll(
            itemSummaryResponses.getContent().stream().map(itemSummaryResponse -> itemSummaryResponse.getItemSku())
                .collect(Collectors.toList()));
        itemXOldPickUpPoint.putAll(itemSummaryResponses.getContent().stream()
            .collect(Collectors.toMap(ItemSummaryResponse::getItemSku, ItemSummaryResponse::getPickupPointCode)));
      }

      if (CollectionUtils.isNotEmpty(itemSkus)) {
        List<WebInventoryUpdatePickupPointRequestDTO> requests = new ArrayList<>();
        Map<String, String> pickupPointCodesByItemSku = new HashMap<>();
        for (String itemSku : itemSkus) {
          WebInventoryUpdatePickupPointRequestDTO webInventoryBulkUpdatePickupPointCodeRequest =
              new WebInventoryUpdatePickupPointRequestDTO();
          webInventoryBulkUpdatePickupPointCodeRequest.setNewPickupPointCode(pickupPointCode);
          webInventoryBulkUpdatePickupPointCodeRequest.setPickupPointCode(itemXOldPickUpPoint.get(itemSku));
          webInventoryBulkUpdatePickupPointCodeRequest.setWebItemSku(itemSku);
          webInventoryBulkUpdatePickupPointCodeRequest.setWebMerchantCode(businessPartnerCode);
          pickupPointCodesByItemSku.put(itemSku, pickupPointCode);
          requests.add(webInventoryBulkUpdatePickupPointCodeRequest);
        }
        com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest pickupPointUpdateRequest =
            new com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest();
        pickupPointUpdateRequest.setPickupPointUpdateItemRequestList(
            Arrays.asList(new PickupPointUpdateItemRequest(itemSkus.get(0), pickupPointCode)));
        pickupPointUpdateRequest.setDifferentLocation(false);
        pickupPointUpdateRequest.setProductSku(productSku);
        try {
          pickupPointUpdateResponse = updatePickupPointInXproductAndInventory(productSku, businessPartnerCode, pickupPointCode, pickupPointUpdateResponse, itemSummaryResponses,
              size, itemSkus, itemXOldPickUpPoint, requests, pickupPointUpdateRequest);
        } catch (Exception ex) {
          LOGGER.error("Exception caught while updating the pickup point codes : {} ", pickupPointUpdateRequest, ex);
          pickupPointUpdateResponse.getResponses().addAll(
              pickupPointUpdateRequest.getPickupPointUpdateItemRequestList().stream().map(
                  request -> new com.gda.mta.product.dto.PickupPointResponse(request.getItemSku(),
                      ErrorCategory.UNSPECIFIED.getMessage(), ex.getMessage())).collect(Collectors.toList()));
        }
      }
      page++;
    } while (page * size < itemSummaryResponses.getTotalElements());
    return pickupPointUpdateResponse;
  }

  private PickupPointUpdateResponse updatePickupPointInXproductAndInventory(String productSku, String businessPartnerCode, String pickupPointCode,
      PickupPointUpdateResponse pickupPointUpdateResponse, Page<ItemSummaryResponse> itemSummaryResponses, int size,
      List<String> itemSkus, Map<String, String> itemXOldPickUpPoint,
      List<WebInventoryUpdatePickupPointRequestDTO> requests,
      com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest pickupPointUpdateRequest) throws Exception {
    pickupPointUpdateResponse.setL3version(xProductOutbound.updatePickupPointCodes(pickupPointUpdateRequest));
    WebInventoryUpdatePickupPointResponseDTO response =
        inventoryOutbound.updatePickupPoint(generateMandatoryRequestParam(), new ListRequestDTO<>(requests));
    if (itemSummaryResponses.getTotalElements() <= size && CollectionUtils.isEmpty(response.getErrors())) {
      this.createAuditLogsForPickupPointUpdate(businessPartnerCode, Constants.DEFAULT,
          UpdateProductActivity.PICK_POINT_CODE.getDesc(), itemXOldPickUpPoint.get(itemSkus.get(0)), pickupPointCode,
          productSku, false, null);
    } else {
      pickupPointUpdateResponse.getResponses().addAll(validateInventoryResponse(response).getResponses());
    }
    return pickupPointUpdateResponse;
  }

  public PickupPointUpdateResponse updatePickupPointByProductSkuMpp(String productSku, String businessPartnerCode,
      String pickupPointCode, boolean fbbActivated, Map<String, Pair<String, String>> itemXOldPickUpPoint, int batchSize)
      throws Exception {
    PickupPointUpdateResponse pickupPointUpdateResponse = new PickupPointUpdateResponse();
    List<String> itemSkus = itemXOldPickUpPoint.keySet().stream().collect(Collectors.toList());
    List<WebInventoryUpdatePickupPointRequestDTO> requests = new ArrayList<>();
    for (String itemSku : itemSkus) {
      WebInventoryUpdatePickupPointRequestDTO webInventoryBulkUpdatePickupPointCodeRequest =
          new WebInventoryUpdatePickupPointRequestDTO();
      webInventoryBulkUpdatePickupPointCodeRequest.setNewPickupPointCode(pickupPointCode);
      webInventoryBulkUpdatePickupPointCodeRequest.setPickupPointCode(
          Optional.ofNullable(itemXOldPickUpPoint.get(itemSku)).map(Pair::getLeft).orElse(null));
      webInventoryBulkUpdatePickupPointCodeRequest.setWebItemSku(itemSku);
      webInventoryBulkUpdatePickupPointCodeRequest.setWebMerchantCode(businessPartnerCode);
      requests.add(webInventoryBulkUpdatePickupPointCodeRequest);
    }
    com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest pickupPointUpdateRequest =
        new com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest();
    pickupPointUpdateRequest.setPickupPointUpdateItemRequestList(
        Arrays.asList(new PickupPointUpdateItemRequest(itemSkus.get(0), pickupPointCode)));
    pickupPointUpdateRequest.setDifferentLocation(false);
    pickupPointUpdateRequest.setProductSku(productSku);
    pickupPointUpdateRequest.setFbbActivated(fbbActivated);
      try {
        pickupPointUpdateResponse.setL3version(xProductOutbound.updatePickupPointCodes(pickupPointUpdateRequest));
        List<List<WebInventoryUpdatePickupPointRequestDTO>> inventoryRequests = Lists.partition(requests, batchSize);
        for (List<WebInventoryUpdatePickupPointRequestDTO> inventoryRequest : inventoryRequests) {
          WebInventoryUpdatePickupPointResponseDTO response =
              updatePickupPointInInventory(businessPartnerCode, productSku, inventoryRequest, fbbActivated,
                  itemXOldPickUpPoint);
          if (CollectionUtils.isNotEmpty(response.getErrors())) {
            pickupPointUpdateResponse.getResponses().addAll(validateInventoryResponse(ConverterUtil.toWebInventoryBulkUpdatePickupPointCodeResponseDTO(response)).getResponses());
          }
        }
        if (CollectionUtils.isEmpty(pickupPointUpdateResponse.getResponses())) {
          createAuditLogsForSameLocationUpdate(productSku, businessPartnerCode, pickupPointCode, itemSkus,
              itemXOldPickUpPoint);
        }
      } catch (Exception e) {
        LOGGER.error("Exception caught while updating the pickup point codes : {} ", pickupPointUpdateRequest, e);
        pickupPointUpdateResponse.getResponses().addAll(
            pickupPointUpdateRequest.getPickupPointUpdateItemRequestList().stream().map(
                request -> new com.gda.mta.product.dto.PickupPointResponse(request.getItemSku(),
                    ErrorCategory.UNSPECIFIED.getMessage(), e.getMessage())).collect(Collectors.toList()));
      }
    return pickupPointUpdateResponse;
  }

  private WebInventoryUpdatePickupPointResponseDTO updatePickupPointInInventory(String businessPartnerCode,
      String productSku, List<WebInventoryUpdatePickupPointRequestDTO> inventoryRequest, Boolean fbbActivated,
      Map<String, Pair<String, String>> itemXOldPickUpPoint) throws Exception {
    try {
      return inventoryOutbound.updatePickupPoint(generateMandatoryRequestParam(),
          new ListRequestDTO<>(inventoryRequest));
    } catch (Exception e) {
      if (deleteInsertInventoryOnPickupPointUpdate && Optional.ofNullable(e.getMessage()).orElse(StringUtils.EMPTY)
          .contains(Constants.INVENTORY_NOT_FOUND_ERROR_MESSAGE)) {
        return productLevel3InventoryService.updatePickupPointInInventory(businessPartnerCode,
            RequestHelper.toInventoryUpsertModels(businessPartnerCode, productSku, inventoryRequest, fbbActivated,
                itemXOldPickUpPoint));
      } else {
        throw e;
      }
    }
  }

  private void createAuditLogsForSameLocationUpdate(String productSku, String businessPartnerCode,
      String pickupPointCode, List<String> itemSkus, Map<String, Pair<String, String>> itemXOldPickUpPoint)
      throws Exception {
    Set<String> distinctPickupPointCodes =
        itemXOldPickUpPoint.entrySet().stream().map(Map.Entry::getValue).map(Pair::getLeft).collect(Collectors.toSet());
    if (distinctPickupPointCodes.size() == 1) {
      this.createAuditLogsForPickupPointUpdate(businessPartnerCode, Constants.DEFAULT,
          UpdateProductActivity.PICK_POINT_CODE.getDesc(),
          Optional.ofNullable(itemXOldPickUpPoint.get(itemSkus.get(0))).map(Pair::getLeft).orElse(null),
          pickupPointCode, productSku, false, null);
    } else {
      for (Map.Entry<String, Pair<String, String>> itemAndOldPickupPointCode : itemXOldPickUpPoint.entrySet()) {
        this.createAuditLogsForPickupPointUpdate(businessPartnerCode, itemAndOldPickupPointCode.getKey(),
            UpdateProductActivity.PICK_POINT_CODE.getDesc(),
            Optional.ofNullable(itemAndOldPickupPointCode.getValue()).map(Pair::getLeft).orElse(null), pickupPointCode,
            productSku, false, null);
      }
    }
  }

  private Long updatePickupPointsInXProduct(Map<String, String> pickupPointCodesByItemSku,
      Map<String, Pair<String, String>> oldPickupPointCodesByItemSku,
      String productSku, String businessPartnerCode, Boolean fbbActivated) throws Exception {
    checkArgument(MapUtils.isNotEmpty(oldPickupPointCodesByItemSku),
        ErrorMessages.ITEM_SKU_AND_PICKUP_POINT_CODE_MAP_MUST_NOT_BE_NULL);
    for (Pair<String, String> pickupPointCode : oldPickupPointCodesByItemSku.values()) {
      checkArgument(Objects.nonNull(pickupPointCode), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_NULL);
    }
    Long L3version = null;
    List<PickupPointUpdateItemRequest> pickupPointUpdateItemRequestList = new ArrayList<>();
    for (Map.Entry<String, String> entry : pickupPointCodesByItemSku.entrySet()) {
      pickupPointUpdateItemRequestList.add(new PickupPointUpdateItemRequest(entry.getKey(), entry.getValue()));
    }
    if (CollectionUtils.isNotEmpty(pickupPointUpdateItemRequestList)) {
      com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest pickupPointUpdateRequest =
          new com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest();
      pickupPointUpdateRequest.setPickupPointUpdateItemRequestList(pickupPointUpdateItemRequestList);
      pickupPointUpdateRequest.setDifferentLocation(true);
      pickupPointUpdateRequest.setProductSku(productSku);
      pickupPointUpdateRequest.setFbbActivated(fbbActivated);
      L3version = xProductOutbound.updatePickupPointCodes(pickupPointUpdateRequest);
      for (PickupPointUpdateItemRequest request : pickupPointUpdateItemRequestList) {
        createAuditLogsForPickupPointUpdate(businessPartnerCode, request.getItemSku(),
            UpdateProductActivity.PICK_POINT_CODE.getDesc(),
            Optional.ofNullable(oldPickupPointCodesByItemSku.get(request.getItemSku())).map(Pair::getLeft).orElse(null),
            pickupPointCodesByItemSku.get(request.getItemSku()), productSku, false, null);
      }
    }
    return L3version;
  }

  private PickupPointUpdateResponse validateInventoryResponse(WebInventoryUpdatePickupPointResponseDTO response) throws Exception {
    List<WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError> errorList =
        response.getErrors();
    PickupPointUpdateResponse pickupPointUpdateResponse = new PickupPointUpdateResponse();
    List<String> errorItems = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(errorList)) {
      for (WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error : errorList) {
        errorItems.add(error.getWebItemSku());
        pickupPointUpdateResponse.getResponses().add(
            new com.gda.mta.product.dto.PickupPointResponse(error.getWebItemSku(), error.getErrorCode(),
                error.getErrorMessage()));
      }
    }
    return pickupPointUpdateResponse;
  }


  private void createAuditLogsForPickupPointUpdate(String businessPartnerCode, String gdnSku, String activity,
      String oldValue, String newValue, String productSku, boolean needRevision, String productItemId) throws Exception {
    String name;
    if (!StringUtils.equals(oldValue, newValue)) {
      if (Constants.DEFAULT.equals(gdnSku)) {
        name = productLevel3Repository.getProductNameByProductSku(Arrays.asList(productSku)).get(productSku);
      } else {
        if(needRevision){
          name = productOutbound.findProductNameByProductItemId(productItemId);
        } else{
          SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
          simpleListStringRequest.getValue().add(gdnSku);
          GdnRestSingleResponse<SimpleMapStringResponse> response =
              xProductOutbound.getItemNameByItemSkus(simpleListStringRequest, true);
          name = response.getValue().getValue().get(gdnSku);
        }
      }
      updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, gdnSku, productSku, name, activity,
          oldValue, newValue, needRevision, newValue);
    }
  }

  @Override
  public ApiErrorCode updateLogistics(ProductLevel3UpdateRequest product, boolean isOnlyExternal,
      ProductLevel3DetailResponse productLevel3DetailResponse, boolean combineContentAndLogisticsPcbUpdate,
      boolean combinePreOrderUpdate, boolean migrateProduct) throws Exception {
    ApiErrorCode apiErrorCode = null;
    ProductLevel3DetailResponse savedProduct = null;
    if (Objects.nonNull(productLevel3DetailResponse)) {
      savedProduct = productLevel3DetailResponse;
    } else {
      savedProduct = this.getL3DetailByProductSku(product.getProductSku(), false, true, new ArrayList<>(), false);
    }
    apiErrorCode = checkProductStatus(savedProduct.getProductL3Response());
    if (Objects.nonNull(apiErrorCode)) {
      LOGGER.error("Product sku not in correct state : {} , error message : {}", product.getProductSku(),
          apiErrorCode.getDesc());
      return apiErrorCode;
    }

    if (migrateProduct) {
      checkUnsyncUpdate(product, syncProductDataOnLogisticUpdate);
    }

    ApiErrorCode apiErrorCode1 = validateShippingType(product);
    if (Objects.nonNull(apiErrorCode1)) {
      return apiErrorCode1;
    }
    ApiErrorCode apiErrorCode2 = validateDimensions(product, savedProduct);
    if (Objects.nonNull(apiErrorCode2)) {
      return apiErrorCode2;
    }
    if (savedProduct.isFreeSample() && (Objects.nonNull(product.getPreOrder()) && Boolean.TRUE
      .equals(product.getPreOrder().getIsPreOrder()))) {
      LOGGER
        .error("Error updating free sample product to pre-order : {} ", product.getProductSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ApiErrorCode.FREE_SAMPLE_CANNOT_BE_SET.getDesc());
    }

    String gdnSku = savedProduct.getDefaultItemSku();
    SimpleMasterProductUpdateResponse simpleMasterProductUpdateResponse = new SimpleMasterProductUpdateResponse();
    boolean isDimensionChanged = false;
    if (!combineContentAndLogisticsPcbUpdate) {
    if (savedProduct.isProductEditable() && isDimensionChanged(savedProduct.getProductL3Response(), product)) {
      LOGGER.info("Updating the dimensions in PCB for request : {}", product);
      if (Objects.isNull(product.getDangerousGoodsLevel())) {
        product.setDangerousGoodsLevel(Constants.DEFAULT_DANGEROUS_GOODS_LEVEL);
      }
      simpleMasterProductUpdateResponse =
          this.productRepository.updateSimpleMasterProduct(toSimpleMasterProductUpdateRequestDTO(product));
      if (!simpleMasterProductUpdateResponse.getUpdateSuccess()) {
        LOGGER.error("Error while updating Dimension to PCB response : {}", simpleMasterProductUpdateResponse);
        apiErrorCode = ApiErrorCode.DIMENSION_UPDATE_FAILED;
        return apiErrorCode;
      }
      isDimensionChanged = true;
    }
    }
    boolean isPreOrderChanged = false;
    if (!product.isProductEditable()) {
      // If product is not editable pre-order will be changed using the below flow
      combinePreOrderUpdate = false;
    }
    if (!combinePreOrderUpdate && isPreOrderChange(product.getPreOrder(),
        savedProduct.getProductL3Response().getPreOrderDTO())) {
      isPreOrderChanged = true;
      String oldValue = null;
      oldValue = getOldValueForPreOrderHistory(savedProduct.getProductL3Response(), oldValue);
      String newValue = String.valueOf(product.getPreOrder()).substring(15);
      newValue = generateNewValueFieldForPreOrderHistory(product.getPreOrder(), newValue);
      updatedProductHistoryService.createProductL3AuditLog(savedProduct.getBusinessPartnerCode(), Constants.DEFAULT,
          savedProduct.getProductSku(), savedProduct.getProductName(), UpdateProductActivity.PRE_ORDER.getDesc(),
          oldValue, newValue, false, StringUtils.EMPTY);
    } else if (Objects.nonNull(savedProduct.getProductL3Response().getPreOrderDTO())) {
      PreOrderRequest preOrderRequest = new PreOrderRequest();
      BeanUtils.copyProperties(savedProduct.getProductL3Response().getPreOrderDTO(), preOrderRequest);
      product.setPreOrder(preOrderRequest);
    }
    boolean productTypeUpdated = !Objects.equals(product.getProductType(), savedProduct.getProductType());
    if(combineContentAndLogisticsPcbUpdate) {
      productTypeUpdated = false;
    }
    if (productTypeUpdated || !product.isProductEditable()
        || isPreOrderChanged || isDimensionChanged) {
      Boolean lateFulfillment = null;
      boolean productTypeAndLateFulfillmentChanged = false;
      if (savedProduct.getProductType() != ProductType.REGULAR.getCode()
          && product.getProductType() == ProductType.REGULAR.getCode()) {
        lateFulfillment = false;
        productTypeAndLateFulfillmentChanged = true;
      } else if (savedProduct.getProductType() == ProductType.REGULAR.getCode()
          && product.getProductType() != ProductType.REGULAR.getCode()) {
        lateFulfillment = true;
        productTypeAndLateFulfillmentChanged = true;
      }
      updateProductAndItemInXProduct(isOnlyExternal, gdnSku, product, lateFulfillment,
          productTypeAndLateFulfillmentChanged, isPreOrderChanged);
    }

    ProductLevel3DetailResponse updatedProduct = this.getL3DetailByProductSku(product.getProductSku(), false, true, new ArrayList<>(), false);
    this.createAuditLogs(savedProduct, updatedProduct, false);

    ApiErrorCode errorCode =  updateLogisticsOption(product.getProductLevel3LogisticsRequest(), savedProduct,
        savedProduct.getProductName());

    // process only if there is type change or dimension change
    if (productTypeUpdated || simpleMasterProductUpdateResponse
        .isDimensionOrDgLevelUpdated()) {
      productService.publishDimensionRefreshEventForReviewPendingProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
          product.getProductCode(), ConverterUtil
              .toDimensionRefreshRequest(updatedProduct.getLength(), updatedProduct.getWidth(),
                  updatedProduct.getHeight(), updatedProduct.getWeight(), updatedProduct.getShippingWeight(),
                  product.getDangerousGoodsLevel(), updatedProduct.getProductType()));
    }

    return errorCode;
  }

  private String generateNewValueFieldForPreOrderHistory(PreOrderRequest preOrderRequest, String newValue) {
    if (convertPreOrderDateToJKT) {
      PreOrderRequest preOrderRequestCopy = new PreOrderRequest();
      BeanUtils.copyProperties(preOrderRequest, preOrderRequestCopy);
      if (Objects.nonNull(preOrderRequestCopy.getPreOrderDate())) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(preOrderRequestCopy.getPreOrderDate());
        calendar.add(Calendar.HOUR_OF_DAY, -7);
        preOrderRequestCopy.setPreOrderDate(calendar.getTime());
      }
      newValue = String.valueOf(preOrderRequestCopy).substring(15);
    }
    return newValue;
  }

  private ApiErrorCode validateDimensions(ProductLevel3UpdateRequest product, ProductLevel3DetailResponse savedProduct) {
    if (validateDimensionInL3Update) {
      int productType =
          Objects.nonNull(product.getProductType()) ? product.getProductType() : savedProduct.getProductType();
      if (ValidationUtil.isProductTypeValidAndRequiresShippingWeight(productType)) {
        double weightInKgs = product.getWeight() / GMS_TO_KG_FACTOR;
        ApiErrorCode apiErrorCode =
            ValidationUtil.validateMinAndMaxDimension(savedProduct.getProductCode(), maxProductDimensionLimit,
                weightInKgs, product.getLength(), product.getWidth(), product.getHeight(), product.getWeight(),
                product.getShippingWeight(), product.isOff2OnChannelActive(),
              product.getB2cActivated(), instoreNewFlowEnabled);
        if (Objects.nonNull(apiErrorCode)) {
          return apiErrorCode;
        }
      }
    }
    return null;
  }

  public ApiErrorCode validateShippingType(ProductLevel3UpdateRequest product) throws Exception {
    ApiErrorCode apiErrorCode;
    if (Objects.nonNull(product.getProductType())) {
      if (CommonUtils.isInvalidShipping(product, instoreNewFlowEnabled)) {
        apiErrorCode = ApiErrorCode.SHIPPING_WEIGHT_NOT_VALID;
        return apiErrorCode;
      }
    }
    return null;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ApiErrorCode updateLogisticsForNeedRevision(ProductLevel3UpdateRequest product) throws Exception {
    ApiErrorCode apiErrorCode = null;
    ProductLevel3DetailResponse savedProduct = this.getL3DetailByProductSku(product.getProductSku(), true, true, new ArrayList<>(), false);
    ApiErrorCode apiErrorCode1 = validateShippingType(product);
    if (Objects.nonNull(apiErrorCode1)) {
      return apiErrorCode1;
    }
    if (isDimensionChanged(savedProduct, product)) {
      LOGGER.info("Updating the dimensions in PCB for request : {}", product);
      if (Objects.isNull(product.getDangerousGoodsLevel())) {
        product.setDangerousGoodsLevel(Constants.DEFAULT_DANGEROUS_GOODS_LEVEL);
      }
      SimpleMasterProductUpdateResponse simpleMasterProductUpdateResponse =
          this.productRepository.updateSimpleMasterProduct(toSimpleMasterProductUpdateRequestDTO(product));
      if (!simpleMasterProductUpdateResponse.getUpdateSuccess()) {
        LOGGER.error("Error while updating Dimension to PCB response : {}", simpleMasterProductUpdateResponse);
        apiErrorCode = ApiErrorCode.DIMENSION_UPDATE_FAILED;
        return apiErrorCode;
      }
      ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(
          savedProduct.getStoreId(), savedProduct.getProductCode());
      updateNeedRevisionNotes(productCollection, Arrays.asList(Constants.PACKAGE_DIMENSION, Constants.CONTENT));
      this.productCollectionRepository.save(productCollection);
    }
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerRepository.findFirstByGdnProductSku(product.getProductSku());
    boolean isPreOrderChanged = false;
    boolean isProductTypeChanged = false;
    if (isPreOrderChange(product.getPreOrder(), savedProduct.getProductL3Response().getPreOrderDTO())) {
      String oldValue = null;
      isPreOrderChanged = true;
      oldValue = getOldValueForPreOrderHistory(savedProduct.getProductL3Response(), oldValue);
      PreOrderRequest preOrderRequest = product.getPreOrder();
      if (shouldUpdatePreOrderDateOnInventory(product.isSellerOmg())) {
        updatePreOrderDateOnInventory(product.getProductSku(),
            com.gdn.partners.pbp.commons.util.CommonUtils.resolvePreOrderDate(preOrderRequest.getIsPreOrder(), preOrderRequest.getPreOrderDate()));
      }
      setPreOrderRequest(productBusinessPartner, preOrderRequest);
      String newValue = String.valueOf(product.getPreOrder()).substring(15);
      PreOrderRequest preOrderRequestForHistory = new PreOrderRequest();
      BeanUtils.copyProperties(product.getPreOrder(), preOrderRequestForHistory);
      newValue = generateNewValueFieldForPreOrderHistory(preOrderRequestForHistory, newValue);
      updatedProductHistoryService.createProductL3AuditLog(savedProduct.getBusinessPartnerCode(), Constants.DEFAULT,
          savedProduct.getProductSku(), savedProduct.getProductName(), UpdateProductActivity.PRE_ORDER.getDesc(),
          oldValue, newValue, true, StringUtils.EMPTY);
    }
    if (!savedProduct.getProductType().equals(product.getProductType())) {
      isProductTypeChanged = true;
      for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
          .getProductItemBusinessPartners()) {
        productItemBusinessPartner.setProductType(product.getProductType());
      }
    }
    if (isPreOrderChanged || isProductTypeChanged) {
      LOGGER
          .info("Saving updated productbusiness partners details in PBP for productSku : {}", product.getProductSku());
      productBusinessPartnerService.saveProductBusinessPartner(productBusinessPartner);

    }
    ProductLevel3DetailResponse updatedProduct = this.getL3DetailByProductSku(product.getProductSku(), true, false, new ArrayList<>(), false );
    this.createAuditLogs(savedProduct, updatedProduct, true);
    return updateLogisticsOption(product.getProductLevel3LogisticsRequest(), savedProduct,
        savedProduct.getProductName());
  }

  private boolean shouldUpdatePreOrderDateOnInventory(boolean sellerOmg) {
    return preOrderConfig.isPoQuotaFeatureSwitch() && sellerOmg;
  }

  private void updatePreOrderDateOnInventory(String productSku, Date preOrderDate)
      throws Exception {
    UpdatePoDateByL3RequestDTO updatePoDateByL3RequestDTO =
        getUpdatePoDateByL3RequestDTO(productSku, preOrderDate);
    inventoryOutbound.updatePoDateByL3(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(), updatePoDateByL3RequestDTO);
  }

  private void setPreOrderRequest(ProductBusinessPartner productBusinessPartner, PreOrderRequest preOrderRequest) {
    productBusinessPartner.setPreOrder(preOrderRequest.getIsPreOrder());
    productBusinessPartner.setPreOrderType(preOrderRequest.getPreOrderType());
    productBusinessPartner.setPreOrderValue(preOrderRequest.getPreOrderValue());
    productBusinessPartner.setPreOrderDate(preOrderRequest.getPreOrderDate());
  }

  private UpdatePoDateByL3RequestDTO getUpdatePoDateByL3RequestDTO(String productSku,
      Date preOrderEndDate) {
    UpdatePoDateByL3RequestDTO updatePoDateByL3RequestDTO = new UpdatePoDateByL3RequestDTO();
    updatePoDateByL3RequestDTO.setProductSku(productSku);
    updatePoDateByL3RequestDTO.setPreOrderEndDate(
        com.gdn.partners.pbp.commons.util.CommonUtils.atJakartaStartOfDay(preOrderEndDate));
    updatePoDateByL3RequestDTO.setInventoryBaseRequest(
        CommonUtils.generateInventoryBaseRequest(Constants.UPDATE_PO_DATE_BY_L3));
    return updatePoDateByL3RequestDTO;
  }

  private void updateProductAndItemInXProduct(boolean isOnlyExternal, String gdnSku, ProductLevel3UpdateRequest product,
      Boolean lateFulfillment, boolean productTypeAndLateFulfillmentChanged, boolean isPreOrderChanged)
      throws Exception {
    LOGGER.info("Updating the logistics changes in x-product  of product sku : {}", product.getProductSku());
    ProductAndItemsResponse productData = this.productLevel3Repository.findDetailByGdnSku(gdnSku);
    PickupPointResponse pickupPointData = new PickupPointResponse();
    pickupPointData.setCode(productData.getItems().get(0).getPickupPointCode());
    ProductLevel3 savedProductData =
        generateProductLevel3(productData, null, pickupPointData, null, null, null, new HashMap<>(), new ArrayList<>(), false);
    savedProductData.setVersion(productData.getItems().get(0).getVersion());
    PreOrderRequest preOrderRequest = product.getPreOrder();
    ProductLevel3 updatedProductData = copyRequest(savedProductData, product);
    if (Objects.nonNull(lateFulfillment)) {
      for (ProductItemLevel3 item : updatedProductData.getItems()) {
        item.setLateFulfillment(lateFulfillment);
      }
    }
    ItemRequest itemRequest = generateItemRequest(updatedProductData.getItems().get(0), product.getProductSku(),
        updatedProductData.getAttributes());
    itemRequest.setContentChanged(false);
    ProductRequest productRequest = generateProductRequest(updatedProductData);

    if (isPreOrderChanged || Objects.nonNull(preOrderRequest)) {
      PreOrderDTO preOrderDTO = new PreOrderDTO();
      BeanUtils.copyProperties(preOrderRequest, preOrderDTO);
      productRequest.setPreOrder(preOrderDTO);
    }
    this.xProductOutbound.updateProduct(isOnlyExternal, productRequest);
    this.xProductOutbound
        .updateItem(isOnlyExternal, productTypeAndLateFulfillmentChanged, isPreOrderChanged, itemRequest);
    this.xProductOutbound.generateProductScoreByProductSkuOrProductCode(product.getProductSku(), null, false);
  }

  private boolean isPreOrderChange(PreOrderRequest preOrder, PreOrderDTO preOrderDTO) {
    LOGGER.info("Checking for preorder changes : preOrder : {}, preOrderDTO : {}", preOrder, preOrderDTO);
    if (Objects.isNull(preOrder)) {
      return false;
    }
    if (Objects.isNull(preOrderDTO) || Objects.isNull(preOrderDTO.getIsPreOrder())) {
      preOrderDTO = new PreOrderDTO(false, null, null, null, convertPreOrderDateToJKT);
    }

    if (!preOrder.getIsPreOrder() && !preOrderDTO.getIsPreOrder()) {
      return false;
    }

    if (!preOrder.getIsPreOrder().equals(preOrderDTO.getIsPreOrder())) {
      return true;
    } else if (!StringUtils.equals(preOrder.getPreOrderType(), preOrderDTO.getPreOrderType())) {
      return true;
    }

    if (!Constants.DATE.equals(preOrder.getPreOrderType())) {
      if (!preOrder.getPreOrderValue().equals(preOrderDTO.getPreOrderValue())) {
        return true;
      }
    } else if (!preOrder.getPreOrderDate().equals(preOrderDTO.getPreOrderDate())) {
      return true;
    }
    return false;
  }

  private void createAuditLogs(ProductLevel3DetailResponse savedProduct, ProductLevel3DetailResponse updatedProduct,
      boolean needCorrection)
      throws Exception {
    UpdateProductItemLevel3Model savedModel =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Detail(savedProduct, false);
    UpdateProductItemLevel3Model updatedModel =
        updateProductItemLevel3ModelConverter.convertFromProductLevel3Detail(updatedProduct, false);
    updatedProductHistoryService
        .saveUpdateProductLevel3Audit(updatedProduct.getBusinessPartnerCode(), Constants.DEFAULT, savedModel,
            updatedModel, updatedProduct.getAccessChannel(), updatedProduct.getProductSku(),
            updatedProduct.getProductName(), needCorrection, StringUtils.EMPTY);
  }


  private void checkUnsyncUpdate(ProductLevel3UpdateRequest product, boolean syncProduct) throws Exception {
    if (product.isProductEditable() && !product.isSynchronize()) {
      LOGGER.info("Migrating the product sku : {}", product.getProductSku());
      if (syncProduct) {
        migrateAndSyncProduct(product.getBusinessPartnerCode(), product.getProductSku(), product.getProductCode());
      } else {
        migrateProductWithoutSync(product.getBusinessPartnerCode(), product.getProductSku(), product.getProductCode());
      }
    }
  }

  private void migrateProductWithoutSync(String businessPartnerCode, String productSku, String productCode)
      throws Exception {
    ProductAndItemsResponse productAndItemsResponse =
        xProductOutbound.getProductAndItemsWithProductData(true, productSku, true, true).getValue();
    com.gdn.x.productcategorybase.dto.request.ProductRequest productRequest =
        ConverterUtil.toProductRequest(productAndItemsResponse);
    productOutbound.migrateProduct(productCode, productCode, businessPartnerCode, productRequest);
  }

  private ProductLevel3 copyRequest(ProductLevel3 productLevel3, ProductLevel3UpdateRequest product) {
    productLevel3.setProductType(product.getProductType());
    if (!product.isSynchronize()) {
      ProductItemLevel3 productItemLevel3 = productLevel3.getItems().get(0);
      productItemLevel3.setHeight(product.getHeight());
      productItemLevel3.setLength(product.getLength());
      productItemLevel3.setWeight(product.getWeight());
      productItemLevel3.setWidth(product.getWidth());
      productItemLevel3.setShippingWeight(product.getShippingWeight());
    }
    return productLevel3;
  }


  private SimpleMasterProductUpdateRequestDTO toSimpleMasterProductUpdateRequestDTO(ProductLevel3UpdateRequest product) {
    SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO = new SimpleMasterProductUpdateRequestDTO();
    BeanUtils.copyProperties(product, simpleMasterProductUpdateRequestDTO);
    simpleMasterProductUpdateRequestDTO.setUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
    simpleMasterProductUpdateRequestDTO.setUpdatedDate(new Date());
    simpleMasterProductUpdateRequestDTO.setName(product.getProductName());
    return simpleMasterProductUpdateRequestDTO;
  }

  private ProductLevel3UpdateRequest toProductLevel3UpdateRequest(ProductLevel3 product) {
    ProductLevel3UpdateRequest productLevel3UpdateRequest = new ProductLevel3UpdateRequest();
    BeanUtils.copyProperties(product, productLevel3UpdateRequest);
    productLevel3UpdateRequest.setSynchronize(product.getSynchronize());
    productLevel3UpdateRequest.setShippingWeight(product.getItems().get(0).getShippingWeight());
    productLevel3UpdateRequest.setLength(product.getItems().get(0).getLength());
    productLevel3UpdateRequest.setWeight(product.getItems().get(0).getWeight());
    productLevel3UpdateRequest.setWidth(product.getItems().get(0).getWidth());
    productLevel3UpdateRequest.setHeight(product.getItems().get(0).getHeight());
    productLevel3UpdateRequest.setLateFulfillment(product.getItems().get(0).getLateFulfillment());
    productLevel3UpdateRequest.setDangerousGoodsLevel(product.getItems().get(0).getDangerousGoodsLevel());
    productLevel3UpdateRequest.setProductLevel3LogisticsRequest(product.getItems().get(0).getLogistics());
    return productLevel3UpdateRequest;
  }

  private boolean isDimensionChanged(ProductL3Response productLevel3, ProductLevel3UpdateRequest product) {
    boolean isDimensionChanged = false;
    LOGGER.info("Checking for dimension changes for request {}", product);
    if (!product.getLength().equals(productLevel3.getLength())) {
      isDimensionChanged = true;
    }
    if (!product.getWidth().equals(productLevel3.getWidth())) {
      isDimensionChanged = true;
    }
    if (!product.getHeight().equals(productLevel3.getHeight())) {
      isDimensionChanged = true;
    }
    if (!product.getWeight().equals(productLevel3.getWeight())) {
      isDimensionChanged = true;
    }
    if (!product.getShippingWeight().equals(productLevel3.getShippingWeight())) {
      isDimensionChanged = true;
    }
    return isDimensionChanged;
  }

  private boolean isDimensionChanged(ProductLevel3DetailResponse productLevel3, ProductLevel3UpdateRequest product) {
    LOGGER.info("Checking for dimension changes for request {}", product);
    boolean dimensionChanged = false;
    if (!product.getLength().equals(productLevel3.getLength())) {
      dimensionChanged = true;
    }
    if (!product.getWidth().equals(productLevel3.getWidth())) {
      dimensionChanged = true;
    }
    if (!product.getHeight().equals(productLevel3.getHeight())) {
      dimensionChanged = true;
    }
    if (!product.getWeight().equals(productLevel3.getWeight())) {
      dimensionChanged = true;
    }
    if (!product.getShippingWeight().equals(productLevel3.getShippingWeight())) {
      dimensionChanged = true;
    }
    return dimensionChanged;
  }


  private boolean isLogisticOptionChanged(List<String> updatedLogisticsOptions, List<String> existingLogisticsOptions) {
    LOGGER.info("Checking for logistic option changes : updatedLogisticsOptions : {}, existingLogisticsOptions : {}",
        updatedLogisticsOptions, existingLogisticsOptions);
    if (updatedLogisticsOptions.size() != existingLogisticsOptions.size()) {
      return true;
    }
    for (String logisticOption : updatedLogisticsOptions) {
      if (!existingLogisticsOptions.contains(logisticOption)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public ProductLevel3DetailResponse getL3DetailByProductSku(String productSku, boolean isNeedCorrection,
      boolean fullFetch, List<PickupPointDeleteRequest> pickupPointDeleteRequests, boolean addingPickupPoints)
      throws Exception {
    LOGGER.info("getL3DetailByProductSku request productSku : {}, isNeedCorrection : {}", productSku, isNeedCorrection);
    String categoryCode, merchantCode, itemSku, productCode;
    ProductL3Response productL3Response = new ProductL3Response();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductCollection productCollection = new ProductCollection();
    if (isNeedCorrection) {
      productCode = productCollectionRepository.getProductCodeByGdnSku(productSku);
      productDetailResponse =
          productOutbound.getProductDetailByProductCode(productCode, true, false);
      productBusinessPartner =
          productBusinessPartnerRepository.findFirstByGdnProductSku(productSku);
      categoryCode = productBusinessPartner.getCategoryCode();
      merchantCode = productBusinessPartner.getBusinessPartnerId();
      itemSku = ofNullable(productBusinessPartner.getProductItemBusinessPartners()).orElse(new ArrayList<>())
          .stream().findFirst().get().getGdnProductItemSku();
      productCollection =
          this.productCollectionRepository.findByStoreIdAndProductCode(DEFAULT_STORE_ID, productCode);
      validateDeletePickupPoint(pickupPointDeleteRequests, addingPickupPoints, productBusinessPartner);
    } else {
      productL3Response = xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
      checkState(Objects.nonNull(productL3Response), ErrorMessages.PRODUCT_SKU_NOT_FOUND);
      checkState(Objects.nonNull(productL3Response.getMasterCatalog()), ErrorMessages.MASTER_CATALOG_NOT_FOUND);
      categoryCode = productL3Response.getMasterCatalog().getCategory().getCategoryCode();
      merchantCode = productL3Response.getMerchantCode();
      itemSku = productL3Response.getDefaultItemSku();
      productCode=productL3Response.getProductCode();
    }
    return getProductLevel3DetailResponse(productSku, isNeedCorrection, fullFetch, categoryCode,
      merchantCode, itemSku, productCode, productL3Response, productDetailResponse,
      productBusinessPartner, productCollection);
  }

  private ProductLevel3DetailResponse getProductLevel3DetailResponse(String productSku,
    boolean isNeedCorrection, boolean fullFetch, String categoryCode, String merchantCode,
    String itemSku, String productCode, ProductL3Response productL3Response,
    ProductDetailResponse productDetailResponse, ProductBusinessPartner productBusinessPartner,
    ProductCollection productCollection) throws Exception {
    List<CategoryResponse> categoriesData = null;
    List<ProductLevel3Logistics> productLevel3Logistics = null;
    ProfileResponse profileResponse = null;
    if(fullFetch) {
      categoriesData = this.categoryRepository.findHierarchyByCategoryCode(categoryCode);
      productLevel3Logistics = new ArrayList<>();
      profileResponse = this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
        merchantCode);
      if (Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany)
        .map(CompanyDTO::getMerchantDeliveryType).map(StringUtils::trimToEmpty)
        .filter(StringUtils::isNotBlank).isPresent()) {
        productLevel3Logistics = productLevel3LogisticsService
            .findLogisticsByItemSku(itemSku, merchantCode,
              Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany
                ).map(CompanyDTO::getMerchantDeliveryType).orElse(StringUtils.EMPTY));
      }
    }
    if (isNeedCorrection) {
      return generateProductLevel3DetailByProductDetailResponse(productDetailResponse, categoriesData,
          productLevel3Logistics, profileResponse, productBusinessPartner, productSku, productCode, productCollection);
    } else {
      return generateProductLevel3Detail(productL3Response, categoriesData, productLevel3Logistics, profileResponse);
    }
  }

  private static void validateDeletePickupPoint(List<PickupPointDeleteRequest> pickupPointDeleteRequests,
    boolean addingPickupPoints, ProductBusinessPartner productBusinessPartner) {
    Map<String, List<String>> itemSkuToPickupPointIdsMap =
        productBusinessPartner.getProductItemBusinessPartners().stream()
            .filter(productItemBusinessPartner -> !productItemBusinessPartner.isMarkForDelete()).collect(Collectors
            .groupingBy(ProductItemBusinessPartner::getGdnProductItemSku,
                Collectors.mapping(ProductItemBusinessPartner::getPickupPointId, Collectors.toList())));
    Map<String, List<String>> itemSkuToPickupPointIdsMapInDeleteRequest;
    if (CollectionUtils.isNotEmpty(pickupPointDeleteRequests) && !addingPickupPoints) {
      itemSkuToPickupPointIdsMapInDeleteRequest = pickupPointDeleteRequests.stream().collect(Collectors
          .groupingBy(PickupPointDeleteRequest::getItemSku,
              Collectors.mapping(PickupPointDeleteRequest::getPickupPointId, Collectors.toList())));
      for (Map.Entry<String, List<String>> entry : itemSkuToPickupPointIdsMapInDeleteRequest.entrySet()) {
        if (entry.getValue().containsAll(itemSkuToPickupPointIdsMap.get(entry.getKey()))) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.CANNOT_DELETE_PICKUP_POINT);
        }
      }
    }
  }

  private ProductLevel3DetailResponse generateProductLevel3DetailByProductDetailResponse(
      ProductDetailResponse productDetailResponse, List<CategoryResponse> categories,
      List<ProductLevel3Logistics> productLevel3Logistics, ProfileResponse profileResponse,
      ProductBusinessPartner productBusinessPartner, String productSku, String productCode,
      ProductCollection productCollection) throws Exception {
    ProductLevel3DetailResponse product = new ProductLevel3DetailResponse();
    BeanUtils.copyProperties(productDetailResponse, product);
    product.setProductL3Response(new ProductL3Response());
    product.setProfileResponse(profileResponse);
    product.setProductCode(productCode);
    product.setProductSku(productSku);
    product.setBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    product.setMerchantCode(productBusinessPartner.getBusinessPartnerId());
    product.setSynchronize(true);
    product.setSuspended(false);
    product.setArchived(false);
    product.setProductType(productBusinessPartner.getProductItemBusinessPartners().get(0).getProductType());
    product.setProductEditable(true);
    product.setMerchantPromoDiscount(false);
    product.setMerchantPromoDiscountActive(false);
    product.setDisableUnSync(false);
    product.setRejected(false);
    product.setIsLateFulfillment(false);
    product.setOff2OnChannelActive(productBusinessPartner.isOff2OnChannelActive());
    product.setCategoryCode(productBusinessPartner.getCategoryCode());

    if (CollectionUtils.isNotEmpty(categories)) {
      String[] categoryNameAndHierarchy = generateCategoryNameIdAndHierarchy(categories);
      product.setCategoryName(categoryNameAndHierarchy[0]);
      product.setCategoryHierarchy(categoryNameAndHierarchy[1]);
      product.setCategoryId(categoryNameAndHierarchy[2]);
      product.setWholesalePriceConfigEnabled(categories.get(0).isWholesalePriceConfigEnabled());
    }

    product.setProductName(productDetailResponse.getName());
    product.setBrand(productDetailResponse.getBrand());
    product.setDescription(new String(productDetailResponse.getDescription()));
    product.setSpecificationDetail(productDetailResponse.getSpecificationDetail());
    product.setUniqueSellingPoint(productDetailResponse.getUniqueSellingPoint());
    product.setProductStory(productDetailResponse.getProductStory());
    product.setUrl(productDetailResponse.getUrl());

    product.setInstallationRequired(false);
    product.setAttributes(new ArrayList<>());
    product.setImages(new ArrayList<>());

    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses())) {
      for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
        for (ProductAttributeValueResponse productAttributeValueResponse : productAttributeResponse.getProductAttributeValues()) {
          ProductLevel3AttributeResponse productLevel3Attribute = new ProductLevel3AttributeResponse();
          productLevel3Attribute.setValues(new ArrayList<>());
          productLevel3Attribute.setAttributeCode(productAttributeResponse.getAttribute().getAttributeCode());
          productLevel3Attribute.setAttributeName(productAttributeResponse.getAttribute().getName());
          productLevel3Attribute.setAttributeType(productAttributeResponse.getAttribute().getAttributeType());
          productLevel3Attribute.setVariantCreation(productAttributeResponse.getAttribute().isVariantCreation());
          productLevel3Attribute.setBasicView(productAttributeResponse.getAttribute().isBasicView());
          productLevel3Attribute.setMandatory(productAttributeResponse.getAttribute().isMandatory());
          productLevel3Attribute.setSkuValue(productAttributeResponse.getAttribute().isSkuValue());
          if (Objects.nonNull(productAttributeValueResponse.getAllowedAttributeValue())) {
            productLevel3Attribute.getValues().add(productAttributeValueResponse.getAllowedAttributeValue().getValue());
          } else if (Objects.nonNull(productAttributeValueResponse.getPredefinedAllowedAttributeValue())) {
            productLevel3Attribute.getValues()
                .add(productAttributeValueResponse.getPredefinedAllowedAttributeValue().getValue());
          } else if (Objects.nonNull(productAttributeValueResponse.getDescriptiveAttributeValue())) {
            productLevel3Attribute.getValues().add(productAttributeValueResponse.getDescriptiveAttributeValue());
          }
          product.getAttributes().add(productLevel3Attribute);
        }
      }
    }
    for (Image imageData : productDetailResponse.getImages()) {
      ProductLevel3ImageResponse image = new ProductLevel3ImageResponse();
      image.setMainImage(imageData.isMainImages());
      image.setSequence(imageData.getSequence());
      image.setLocationPath(imageData.getLocationPath());
      product.getImages().add(image);
    }

    if (CollectionUtils.isNotEmpty(productLevel3Logistics)) {
      for (ProductLevel3Logistics productLevel3LogisticsResponse : productLevel3Logistics) {
        ProductItemLevel3LogisticResponse productItemLevel3LogisticResponse = new ProductItemLevel3LogisticResponse();
        BeanUtils.copyProperties(productLevel3LogisticsResponse, productItemLevel3LogisticResponse);
        product.getProductLevel3Logistics().add(productItemLevel3LogisticResponse);
      }
    }

    PreOrderResponse preOrderResponse = PreOrderResponse.builder().isPreOrder(productBusinessPartner.isPreOrder())
        .preOrderType(productBusinessPartner.getPreOrderType()).preOrderValue(productBusinessPartner.getPreOrderValue())
        .preOrderDate(productBusinessPartner.getPreOrderDate()).build();
    product.setPreOrder(preOrderResponse);
    product.setProductL3Response(new ProductL3Response());

    PreOrderDTO preOrderDTO = PreOrderDTO.builder().isPreOrder(productBusinessPartner.isPreOrder())
        .preOrderType(productBusinessPartner.getPreOrderType()).preOrderValue(productBusinessPartner.getPreOrderValue())
        .preOrderDate(productBusinessPartner.getPreOrderDate()).build();
    product.getProductL3Response().setPreOrderDTO(preOrderDTO);
    product.setPickupPointCodes(productBusinessPartner.getProductItemBusinessPartners().stream()
        .filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete)).map(ProductItemBusinessPartner::getPickupPointId)
        .distinct().collect(Collectors.toList()));
    product.setItemSkus(new ArrayList<>(productBusinessPartner.getProductItemBusinessPartners().stream()
        .map(ProductItemBusinessPartner::getGdnProductItemSku).collect(Collectors.toSet())));
    // Item count will be same as unique list of items
    product.setItemCount(product.getItemSkus().size());
    product.setResubmitCount(productCollection.getResubmitCount());
    product.setFreeSample(productBusinessPartner.isFreeSample());
    return product;
  }

  @Override
  public ProductLevel3DetailResponse generateProductLevel3Detail(ProductL3Response productData,
    List<CategoryResponse> categories, List<ProductLevel3Logistics> productLevel3Logistics,
    ProfileResponse profileResponse) throws Exception {
    ProductLevel3DetailResponse product = new ProductLevel3DetailResponse();
    BeanUtils.copyProperties(productData, product);
    product.setProductL3Response(productData);
    product.setProfileResponse(profileResponse);
    product.setBusinessPartnerCode(productData.getMerchantCode());
    product.setSynchronize(productData.isSynchronized());
    product.setProductType(productData.getProductType().getCode());
    product.setSuspended(productData.isSuspended());
    product.setArchived(productData.isArchived());
    product.setProductEditable(productData.isProductEditable());
    product.setMerchantPromoDiscount(productData.isMerchantPromoDiscount());
    product.setMerchantPromoDiscountActive(productData.isMerchantPromoDiscountActive());
    product.setDisableUnSync(productData.isDisableUnSync());
    if (productData.isMarkForDelete() && !productData.isSuspended()) {
      product.setRejected(true);
    }
    product.setIsLateFulfillment(productData.getIsLateFulfillment());
    product.setOff2OnChannelActive(productData.isOff2OnChannelActive());
    product.setCategoryCode(productData.getMasterCatalog().getCategory().getCategoryCode());
    if (Objects.nonNull(productData.getProductScore())) {
      ProductScoreResponse productScore = new ProductScoreResponse();
      BeanUtils.copyProperties(productData.getProductScore(), productScore);
      product.setProductScore(productScore);
    }

    if (CollectionUtils.isNotEmpty(categories)) {
      String[] categoryNameAndHierarchy = generateCategoryNameIdAndHierarchy(categories);
      product.setCategoryName(categoryNameAndHierarchy[0]);
      product.setCategoryHierarchy(categoryNameAndHierarchy[1]);
      product.setCategoryId(categoryNameAndHierarchy[2]);
      product.setWholesalePriceConfigEnabled(categories.get(0).isWholesalePriceConfigEnabled());
    }

    if (Objects.nonNull(productData.getMasterDataProduct())) {
      product.setProductName(productData.getMasterDataProduct().getProductName());
      product.setBrand(productData.getMasterDataProduct().getBrand());
      product.setDescription(new String(productData.getMasterDataProduct().getDescription()));
      product.setSpecificationDetail(productData.getMasterDataProduct().getSpecificationDetail());
      product.setUniqueSellingPoint(productData.getMasterDataProduct().getUniqueSellingPoint());
      product.setProductStory(productData.getMasterDataProduct().getProductStory());
      product.setUrl(productData.getMasterDataProduct().getUrl());
    }
    product.setInstallationRequired(productData.isInstallationRequired());
    product.setAttributes(new ArrayList<>());
    product.setImages(new ArrayList<>());

    Map<String, MasterDataAttributeDTO> attributeDatas = new HashMap<String, MasterDataAttributeDTO>();
    if (Objects.nonNull(productData.getMasterDataProduct())) {
      for (MasterDataProductAttributeDTO attributeData : productData.getMasterDataProduct()
          .getMasterDataProductAttributes()) {
        attributeDatas
            .put(attributeData.getMasterDataAttribute().getAttributeCode(), attributeData.getMasterDataAttribute());
      }
    }

    List<String> specialAttributes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productData.getProductSpecialAttributes())) {
      for (ProductSpecialAttributeDTO attributeSpecialData : productData.getProductSpecialAttributes()) {
        ProductLevel3AttributeResponse productLevel3Attribute = new ProductLevel3AttributeResponse();
        productLevel3Attribute.setValues(new ArrayList<>());
        productLevel3Attribute.setAttributeCode(attributeSpecialData.getAttributeCode());
        setProductLevel3SpecialAttributes(attributeDatas, attributeSpecialData, productLevel3Attribute);
        productLevel3Attribute.getValues().add(attributeSpecialData.getAttributeValue());
        product.getAttributes().add(productLevel3Attribute);
        specialAttributes.add(productLevel3Attribute.getAttributeCode());
      }
    }

    MasterDataAttributeDTO masterDataAttributeDTO = null;
    if (CollectionUtils.isNotEmpty(productData.getDescriptiveAttributes())) {
      for (ProductAttributeDetailDTO descriptiveAttributeData : productData.getDescriptiveAttributes()) {
        if (!specialAttributes.contains(descriptiveAttributeData.getAttributeCode())) {
          ProductLevel3AttributeResponse productLevel3Attribute = new ProductLevel3AttributeResponse();
          productLevel3Attribute.setValues(new ArrayList<>());
          productLevel3Attribute.setAttributeCode(descriptiveAttributeData.getAttributeCode());
          productLevel3Attribute.setAttributeName(descriptiveAttributeData.getAttributeName());
          masterDataAttributeDTO = attributeDatas.get(descriptiveAttributeData.getAttributeCode());
          if (Objects.nonNull(masterDataAttributeDTO) && Objects.nonNull(masterDataAttributeDTO.getAttributeType())) {
            productLevel3Attribute.setAttributeType(masterDataAttributeDTO.getAttributeType().name());
            productLevel3Attribute.setVariantCreation(masterDataAttributeDTO.isVariantCreation());
            productLevel3Attribute.setBasicView(masterDataAttributeDTO.isBasicView());
            productLevel3Attribute.setMandatory(masterDataAttributeDTO.isMandatory());
            productLevel3Attribute.setSkuValue(masterDataAttributeDTO.isSkuValue());
          }
          productLevel3Attribute.getValues().add(descriptiveAttributeData.getAttributeValue());
          product.getAttributes().add(productLevel3Attribute);
        }
      }
    }
    if (CollectionUtils.isNotEmpty(productData.getDefiningAttributes())) {
      for (ProductAttributeDTO definingAttributeData : productData.getDefiningAttributes()) {
        String itemSku = definingAttributeData.getItemSku();
        for (ProductAttributeDetailDTO definingDetailAttributeData : definingAttributeData
            .getProductAttributeDetails()) {
          ProductLevel3AttributeResponse productLevel3Attribute = new ProductLevel3AttributeResponse();
          productLevel3Attribute.setValues(new ArrayList<>());
          productLevel3Attribute.setAttributeCode(definingDetailAttributeData.getAttributeCode());
          productLevel3Attribute.setAttributeName(definingDetailAttributeData.getAttributeName());
          masterDataAttributeDTO = attributeDatas.get(definingDetailAttributeData.getAttributeCode());
          if (masterDataAttributeDTO != null && masterDataAttributeDTO.getAttributeType() != null) {
            productLevel3Attribute.setAttributeType(masterDataAttributeDTO.getAttributeType().name());
            productLevel3Attribute.setBasicView(masterDataAttributeDTO.isBasicView());
            productLevel3Attribute.setMandatory(masterDataAttributeDTO.isMandatory());
            productLevel3Attribute.setSkuValue(masterDataAttributeDTO.isSkuValue());
            productLevel3Attribute.setVariantCreation(masterDataAttributeDTO.isVariantCreation());
          }
          productLevel3Attribute.getValues().add(definingDetailAttributeData.getAttributeValue());
          productLevel3Attribute.setItemSku(itemSku);
          product.getAttributes().add(productLevel3Attribute);
        }
      }
    }
    if (Objects.nonNull(productData.getMasterDataProduct())) {
      for (MasterDataProductImageDTO imageData : productData.getMasterDataProduct().getMasterDataProductImages()) {
        ProductLevel3ImageResponse image = new ProductLevel3ImageResponse();
        image.setMainImage(imageData.isMainImage());
        image.setSequence(imageData.getSequence());
        image.setLocationPath(imageData.getLocationPath());
        product.getImages().add(image);
      }
    }

    if (CollectionUtils.isNotEmpty(productLevel3Logistics)) {
      for (ProductLevel3Logistics productLevel3LogisticsResponse : productLevel3Logistics) {
        ProductItemLevel3LogisticResponse productItemLevel3LogisticResponse = new ProductItemLevel3LogisticResponse();
        BeanUtils.copyProperties(productLevel3LogisticsResponse, productItemLevel3LogisticResponse);
        product.getProductLevel3Logistics().add(productItemLevel3LogisticResponse);
      }
    }

    if (Objects.nonNull(productData.getPreOrderDTO())) {
      PreOrderDTO preOrderDTO = productData.getPreOrderDTO();
      PreOrderResponse preOrderResponse =
          PreOrderResponse.builder().isPreOrder(preOrderDTO.getIsPreOrder()).preOrderType(preOrderDTO.getPreOrderType())
              .preOrderValue(preOrderDTO.getPreOrderValue()).preOrderDate(preOrderDTO.getPreOrderDate()).build();
      product.setPreOrder(preOrderResponse);
    }

    product.setPickupPointCodes(productData.getPickupPointCodes());
    product.setFbbPickupPointCodes(productData.getFbbPickupPointCodes());
    product.setItemSkus(productData.getItemSkus());
    product.setFreeSample(productData.isFreeSample());
    product.setSizeChartCode(productData.getSizeChartCode());
    return product;
  }

  public ApiErrorCode checkProductStatus(ProductL3Response productL3Response) {
    if (Objects.isNull(productL3Response)) {
      return ApiErrorCode.PRODUCT_NOT_PRESENT;
    } else if (productL3Response.isMarkForDelete()) {
      int rejectedProductCount =
          productBusinessPartnerRepository.countRejectedProductByGdnProductSku(productL3Response.getProductSku());
      if (rejectedProductCount > 0) {
        return ApiErrorCode.ITEM_IS_REJECTED;
      } else if (productL3Response.isSuspended()) {
        return ApiErrorCode.ITEM_IS_SUSPENDED;
      } else if (productL3Response.isTakenDown()) {
        return ApiErrorCode.PRODUCT_IS_TAKEN_DOWN;
      }
    } else if (productL3Response.isArchived()) {
      return ApiErrorCode.ITEM_IS_ARCHIVED;
    }
    return null;
  }

  private Product convertProductLevel3ToProduct(ProductLevel3 request, Product product,
      Map<String, ProductItemAttributeValueRequest> productItemAttributeValueMap, List<String> deletedAttributeCodes)
      throws Exception {
    String id = product.getId();

    //why are we not copying version
    BeanUtils.copyProperties(request, product, "productItems", "productAttributes", "version", "length", "width",
        "height", "weight", "shippingWeight");
    product.setDescription(request.getDescription().getBytes());
    product.setName(request.getProductName());
    product.setId(id);
    product.setUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
    product.setUpdatedDate(new Date());
    Map<String, ProductLevel3Attribute> newProductAttributes = new HashMap<>();
    Map<String, List<String>> attributeCodeToValuesMap = new HashMap<>();
    Map<String, List<String>> existingAttributeCodeToValuesMap = new HashMap<>();

    if (addDeleteVariantSwitch && CommonUtils.isEligibleForDuplicateAttributeValidation(
      validateAttributeAdditionForOnlyNewlyAddedAttributes, request)) {
      for (ProductLevel3Attribute productLevel3Attribute : request.getAttributes()) {
        if (!isBrandAttribute(productLevel3Attribute)) {
          newProductAttributes.put(productLevel3Attribute.getAttributeCode(), productLevel3Attribute);
        }
      }
      for (ProductLevel3Attribute productLevel3Attribute : request.getAttributes()) {
        if (isVariantCreationAttribute(productLevel3Attribute)) {
          List<String> attributeValues =
              attributeCodeToValuesMap.getOrDefault(productLevel3Attribute.getAttributeCode(), new ArrayList<>());
          attributeValues.addAll(ValueTypeUtil.getValueTypeAndValue(productLevel3Attribute.getValueType(),
              productLevel3Attribute.getValues(), sizeChartValueTypeDelimiter, valueTypeAdditionForDefiningAttributes));
          attributeCodeToValuesMap.put(productLevel3Attribute.getAttributeCode(), attributeValues);
        }
      }
      checkIfVariantCreationAttributeIsDeleted(product, newProductAttributes, deletedAttributeCodes, request);
      checkIfValuesAreUpdatedToExistingAttribute(product, productItemAttributeValueMap, newProductAttributes,
          existingAttributeCodeToValuesMap, attributeCodeToValuesMap, deletedAttributeCodes, request);
    } else {
      newProductAttributes =
          request.getAttributes().stream().filter(attribute -> !isVariantCreationOrBrandAttribute(attribute))
              .collect(Collectors.toMap(ProductLevel3Attribute::getAttributeCode, Function.identity()));
    }

    List<ProductAttribute> existingAttributes = product.getProductAttributes();
    List<ProductAttribute> attributes = new ArrayList<>();
    List<String> brandAttributeCodeList =
        Arrays.stream(brandAttributeCodes.split(Constants.COMMA)).map(StringUtils::trimToEmpty)
            .collect(Collectors.toList());
    for (ProductAttribute productAttribute : existingAttributes) {
      if (isVariantCreationOrBrandAttribute(productAttribute, brandAttributeCodeList, validateAttributesByBrandCode)) {
        if (deletedAttributeCodes.contains(productAttribute.getAttribute().getAttributeCode())) {
          productAttribute.setMarkForDelete(true);
        }
        attributes.add(productAttribute);
        LOGGER.info(
                  "Skipping the attribute because its a brand or variant attribute or special attribute for productCode : {}, attributeCode : {}",
                  product.getProductCode(), productAttribute.getAttribute().getAttributeCode());
          continue;
      }
       else {
        ProductLevel3Attribute productLevel3Attribute =
            newProductAttributes.get(productAttribute.getAttribute().getAttributeCode());
        if (Objects.isNull(productLevel3Attribute)) {
          productAttribute.setMarkForDelete(!productAttribute.getAttribute().isHideForSeller());
        } else {
          Attribute attribute = productAttribute.getAttribute();
          List<ProductAttributeValue> productAttributeValues = productAttribute.getProductAttributeValues();
          List<String> requestValues = productLevel3Attribute.getValues();
          String value;
          if (CollectionUtils.isNotEmpty(requestValues) && StringUtils.isNotBlank(requestValues.get(0))) {
            value = requestValues.get(0);
          } else {
            if (productAttribute.getAttribute().isMandatory()) {
              throw new ApplicationException(ErrorCategory.VALIDATION,
                  ApiErrorCode.MANDATORY_FIELD_NOT_FOUND.getDesc());
            }
            value = DELIMITER_DASH;
          }
          if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(attribute.getAttributeType().name())) {
            if (CollectionUtils.isNotEmpty(productAttributeValues)) {
              if (StringUtils.isBlank(productAttributeValues.get(0).getDescriptiveAttributeValue())
                  || !productAttributeValues.get(0).getDescriptiveAttributeValue().equals(value)) {
                productAttribute.getProductAttributeValues().get(0).setDescriptiveAttributeValue(value);
              }
              newProductAttributes.remove(productAttribute.getAttribute().getAttributeCode());
            }
          } else if (AttributeType.PREDEFINED_ATTRIBUTE.name().equals(attribute.getAttributeType().name())) {
            if (CollectionUtils.isNotEmpty(productAttributeValues)) {
              if (Objects.isNull(productAttributeValues.get(0).getPredefinedAllowedAttributeValue())
                  || !value.equals(productAttributeValues.get(0).getPredefinedAllowedAttributeValue().getValue())) {
                PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = productOutbound
                    .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(attribute.getAttributeCode(),
                        value, false);
                if (Objects.nonNull(predefinedAllowedAttributeValueResponse)) {
                  PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
                      new PredefinedAllowedAttributeValue();
                  BeanUtils.copyProperties(predefinedAllowedAttributeValueResponse, predefinedAllowedAttributeValue);
                  predefinedAllowedAttributeValue.setAttribute(attribute);
                  productAttribute.getProductAttributeValues().get(0)
                      .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
                  productAttribute.getProductAttributeValues().get(0)
                      .setPredefinedAllowedAttributeValueId(predefinedAllowedAttributeValue.getAttributeId());
                }
              }
              newProductAttributes.remove(productAttribute.getAttribute().getAttributeCode());
            }
          }
        }
      }
      attributes.add(productAttribute);
    }

    // adding new attributes during edit
    if (MapUtils.isNotEmpty(newProductAttributes)) {
      for (Map.Entry<String, ProductLevel3Attribute> entry : newProductAttributes.entrySet()) {
        String attributeCode = entry.getKey();
        ProductLevel3Attribute productLevel3Attribute = entry.getValue();
        AttributeResponse attributeResponse = productOutbound.getAttributeDetailByAttributeCode(attributeCode);
        if (hideFromSeller(attributeResponse)) {
          continue;
        }
        Attribute attribute = new Attribute();
        BeanUtils.copyProperties(attributeResponse, attribute);
        ProductAttribute productAttribute = new ProductAttribute();
        productAttribute.setAttribute(attribute);
        productAttribute.setAttributeId(attribute.getId());
        productAttribute.setProductAttributeName(attribute.getName());
        productAttribute.setProduct(product);
        productAttribute.setProductId(product.getId());
        List<ProductAttributeValue> values = new ArrayList<>();
        List<String> requestValues = productLevel3Attribute.getValues();
        String value;
        if (CollectionUtils.isNotEmpty(requestValues) && StringUtils.isNotBlank(requestValues.get(0))) {
          value = requestValues.get(0);
        } else {
          if (attribute.isMandatory()) {
            throw new ApplicationException(ErrorCategory.VALIDATION, ApiErrorCode.MANDATORY_FIELD_NOT_FOUND.getDesc());
          }
          value = DELIMITER_DASH;
        }
        if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(attributeResponse.getAttributeType())) {
          if (attributeResponse.isVariantCreation()) {
            AttributeRequest attributeRequest = new AttributeRequest();
            BeanUtils.copyProperties(attribute, attributeRequest);
            for (String descriptiveValue : attributeCodeToValuesMap.getOrDefault(attributeCode, new ArrayList<>())) {
              ProductAttributeValue productAttributeValue = new ProductAttributeValue();
              productAttributeValue.setDescriptiveAttributeValue(descriptiveValue);
              productAttributeValue.setDescriptiveAttributeValueType(
                  com.gdn.x.productcategorybase.DescriptiveAttributeValueType.SINGLE);
              values.add(productAttributeValue);
              ProductItemAttributeValueRequest productItemAttributeValueRequest =
                  new ProductItemAttributeValueRequest();
              productItemAttributeValueRequest.setAttribute(attributeRequest);
              productItemAttributeValueRequest.setValue(descriptiveValue);
              getItemAttributeValueMap(productItemAttributeValueMap, descriptiveValue, attributeCode,
                  productItemAttributeValueRequest);
            }
          }
          else {
            ProductAttributeValue productAttributeValue = new ProductAttributeValue();
            productAttributeValue.setDescriptiveAttributeValue(value);
            productAttributeValue.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.DescriptiveAttributeValueType.SINGLE);
            values.add(productAttributeValue);
          }
          productAttribute.setProductAttributeValues(values);
          attributes.add(productAttribute);
        } else if (AttributeType.DEFINING_ATTRIBUTE.name().equals(attributeResponse.getAttributeType())) {
          AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
          allowedAttributeValueRequest.setAttributeCode(attributeCode);
          allowedAttributeValueRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
          allowedAttributeValueRequest.setValues(
              attributeCodeToValuesMap.getOrDefault(attributeCode, new ArrayList<>()));
          List<AllowedAttributeValueResponse> allowedAttributeValueResponseList =
              productOutbound.findAllowedAttributeValue(Collections.singletonList(allowedAttributeValueRequest));
          AllowedAttributeValueResponse allowedAttributeValueResponse = allowedAttributeValueResponseList.get(0);
          AttributeRequest attributeRequest = new AttributeRequest();
          BeanUtils.copyProperties(attribute, attributeRequest);
          allowedAttributeValueResponse.setAllowedValue(allowedAttributeValueResponse.getAllowedValue());
          for (AllowedValueResponse allowedValueResponse : allowedAttributeValueResponse.getAllowedValue()) {
            ProductAttributeValue productAttributeValue = new ProductAttributeValue();
            ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
            productItemAttributeValueRequest.setAttribute(attributeRequest);
            productItemAttributeValueRequest.setValue(allowedValueResponse.getValue());
            getItemAttributeValueMap(productItemAttributeValueMap,
                ValueTypeUtil.getValue(allowedValueResponse.getValue(), sizeChartValueTypeDelimiter,
                    valueTypeAdditionForDefiningAttributes), attributeCode, productItemAttributeValueRequest);
            AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
            allowedAttributeValue.setAttribute(attribute);
            allowedAttributeValue.setAttributeId(attribute.getId());
            allowedAttributeValue.setAllowedAttributeCode(allowedValueResponse.getAllowedValueCode());
            allowedAttributeValue.setValue(allowedValueResponse.getValue());
            allowedAttributeValue.setId(allowedValueResponse.getAllowedValueId());
            productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
            productAttributeValue.setAllowedAttributeValueId(allowedAttributeValue.getId());
            productAttributeValue.setDescriptiveAttributeValueType(
                com.gdn.x.productcategorybase.DescriptiveAttributeValueType.NONE);
            values.add(productAttributeValue);
          }
          productAttribute.setProductAttributeValues(values);
          attributes.add(productAttribute);
        } else {
          ProductAttributeValue productAttributeValue = new ProductAttributeValue();
          PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
              productOutbound.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(attribute.getAttributeCode(),
                  value, false);
          PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
          BeanUtils.copyProperties(predefinedAllowedAttributeValueResponse, predefinedAllowedAttributeValue);
          productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
          productAttributeValue.setPredefinedAllowedAttributeValueId(predefinedAllowedAttributeValue.getId());
          productAttributeValue.setDescriptiveAttributeValueType(
              com.gdn.x.productcategorybase.DescriptiveAttributeValueType.PREDEFINED);
          values.add(productAttributeValue);
          productAttribute.setProductAttributeValues(values);
          attributes.add(productAttribute);
        }
      }
    }
    product.setProductAttributes(attributes);

    if (brandCategoryEditEnabledForExternal && request.isBrandUpdated()) {
      for (ProductAttribute productAttribute : product.getProductAttributes()) {
        Attribute attribute = productAttribute.getAttribute();

        if (brandAttributeCodeList.contains(attribute.getAttributeCode())) {
          PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
              productOutbound.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
                  attribute.getAttributeCode(), request.getBrand(), false);
          if (Objects.isNull(predefinedAllowedAttributeValueResponse)) {
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                ErrorMessages.BRAND_CODE_NOT_ALLOWED);
          }

          PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
              new PredefinedAllowedAttributeValue();
          BeanUtils.copyProperties(predefinedAllowedAttributeValueResponse,
              predefinedAllowedAttributeValue);

          predefinedAllowedAttributeValue.setAttribute(attribute);
          productAttribute.getProductAttributeValues().get(0)
              .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
          productAttribute.getProductAttributeValues().get(0).setPredefinedAllowedAttributeValueId(
              predefinedAllowedAttributeValue.getAttributeId());
          request.setBrandCode(predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode());
        }
      }
    }
    if(request.isCategoryUpdated()){
      product.getProductCategories().forEach(productCategory -> productCategory.setMarkForDelete(true));
    }
    ConverterUtil.setDimensionDetails(product, request);
    log.info("Item attribute map : {} for productCode : {} ", productItemAttributeValueMap, request.getProductCode());
    return product;
  }

  private boolean hideFromSeller(AttributeResponse attributeResponse) {
    return attributeResponse.isHideForSeller() && productSuitabilityFeatureEnabled;
  }

  private static void getItemAttributeValueMap(Map<String, ProductItemAttributeValueRequest> productItemAttributeValueMap,
      String allowedValueResponse, String attributeCode,
      ProductItemAttributeValueRequest productItemAttributeValueRequest) {
    productItemAttributeValueMap.put(allowedValueResponse + attributeCode, productItemAttributeValueRequest);
  }

  private void checkIfVariantCreationAttributeIsDeleted(Product product,
      Map<String, ProductLevel3Attribute> newProductAttributes, List<String> deletedAttributeCodes,
      ProductLevel3 request) {
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      if (productAttribute.getAttribute().isVariantCreation()) {
        ProductLevel3Attribute productLevel3Attribute =
            newProductAttributes.get(productAttribute.getAttribute().getAttributeCode());
        if (Objects.isNull(productLevel3Attribute) && !productAttribute.isMarkForDelete() && CollectionUtils.isNotEmpty(
            request.getDeletedItems())) {
          log.info("Deleted variant creation attribute : {} for product : {}, ",
              productAttribute.getAttribute().getAttributeCode(), product.getProductCode());
          deletedAttributeCodes.add(productAttribute.getAttribute().getAttributeCode());
        }
      }
    }
  }

  @Override
  public void checkIfValuesAreUpdatedToExistingAttribute(Product product, Map<String, ProductItemAttributeValueRequest> productItemAttributeValueMap,
      Map<String, ProductLevel3Attribute> newProductAttributes,
      Map<String, List<String>> existingAttributeCodeToValuesMap, Map<String, List<String>> attributeCodeToValuesRequestMap,
      List<String> deletedAttributeCodes, ProductLevel3 request) throws Exception {
    if(addDeleteVariantSwitch) {
      for (ProductAttribute productLevel3Attribute : product.getProductAttributes()) {
        if (productLevel3Attribute.getAttribute().isVariantCreation()) {
          List<String> attributeValues = existingAttributeCodeToValuesMap.getOrDefault(productLevel3Attribute.getAttribute().getAttributeCode(),
              new ArrayList<>());
          if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(productLevel3Attribute.getAttribute().getAttributeType().name())) {
            productLevel3Attribute.getProductAttributeValues().stream().map(ProductAttributeValue::getDescriptiveAttributeValue).forEach(attributeValues::add);
            existingAttributeCodeToValuesMap.put(productLevel3Attribute.getAttribute().getAttributeCode(),
                attributeValues);
          } else {
            productLevel3Attribute.getProductAttributeValues().stream().map(productAttributeValue ->
                    ValueTypeUtil.getValueTypeAndValue(productAttributeValue.getAllowedAttributeValue().getValueType(),
                        productAttributeValue.getAllowedAttributeValue().getValue(),
                        sizeChartValueTypeDelimiter, valueTypeAdditionForDefiningAttributes))
                .forEach(attributeValues::add);
            existingAttributeCodeToValuesMap.put(productLevel3Attribute.getAttribute().getAttributeCode(),
                attributeValues);
          }
        }
      }
      for (ProductAttribute productLevel3Attribute : product.getProductAttributes()) {
        if (productLevel3Attribute.getAttribute().isVariantCreation()) {
          newProductAttributes.remove(productLevel3Attribute.getAttribute().getAttributeCode());
          List<String> valuesInExistingProduct =
              deleteExistingVariantCreationAttributes(product, existingAttributeCodeToValuesMap, attributeCodeToValuesRequestMap,
                  productLevel3Attribute, deletedAttributeCodes, request);
          for (String value : attributeCodeToValuesRequestMap.getOrDefault(
              productLevel3Attribute.getAttribute().getAttributeCode(), new ArrayList<>())) {
            if (!valuesInExistingProduct.contains(value)) {
              if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(productLevel3Attribute.getAttribute().getAttributeType().name())) {
                ProductAttributeValue productAttributeValue = new ProductAttributeValue();
                productAttributeValue.setDescriptiveAttributeValue(value);
                productAttributeValue.setDescriptiveAttributeValueType(
                    com.gdn.x.productcategorybase.DescriptiveAttributeValueType.SINGLE);
                productLevel3Attribute.getProductAttributeValues().add(productAttributeValue);
                getProductItemAttributeMap(productItemAttributeValueMap, productLevel3Attribute, value);
              } else {
                ProductAttributeValue productAttributeValue = new ProductAttributeValue();
                AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
                allowedAttributeValueRequest.setAttributeCode(productLevel3Attribute.getAttribute().getAttributeCode());
                allowedAttributeValueRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
                allowedAttributeValueRequest.setValues(Collections.singletonList(value));
                List<AllowedAttributeValueResponse> allowedAttributeValueResponseList =
                    productOutbound.findAllowedAttributeValue(Collections.singletonList(allowedAttributeValueRequest));
                if (CollectionUtils.isEmpty(allowedAttributeValueResponseList)) {
                  throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
                      ErrorMessages.ATTRIBUTE_VALUE_DELETED);
                }
                AllowedAttributeValueResponse allowedAttributeValueResponse = allowedAttributeValueResponseList.get(0);
                allowedAttributeValueResponse.setAllowedValue(allowedAttributeValueResponse.getAllowedValue());
                AttributeRequest attributeRequest = new AttributeRequest();
                BeanUtils.copyProperties(productLevel3Attribute.getAttribute(), attributeRequest);
                for (AllowedValueResponse allowedValueResponse : allowedAttributeValueResponse.getAllowedValue()) {
                  ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
                  productItemAttributeValueRequest.setAttribute(attributeRequest);
                  productItemAttributeValueRequest.setValue(allowedValueResponse.getValue());
                  getItemAttributeValueMap(productItemAttributeValueMap, allowedValueResponse.getValue(),
                      attributeRequest.getAttributeCode(), productItemAttributeValueRequest);
                  AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
                  allowedAttributeValue.setAttribute(productLevel3Attribute.getAttribute());
                  allowedAttributeValue.setAttributeId(productLevel3Attribute.getAttribute().getId());
                  allowedAttributeValue.setAllowedAttributeCode(allowedValueResponse.getAllowedValueCode());
                  allowedAttributeValue.setValue(allowedValueResponse.getValue());
                  allowedAttributeValue.setId(allowedValueResponse.getAllowedValueId());
                  productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
                  productAttributeValue.setAllowedAttributeValueId(allowedAttributeValue.getId());
                  productAttributeValue.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.DescriptiveAttributeValueType.NONE);
                }
                productLevel3Attribute.getProductAttributeValues().add(productAttributeValue);
                getProductItemAttributeMap(productItemAttributeValueMap, productLevel3Attribute, value);
              }
            } else {
              getProductItemAttributeMap(productItemAttributeValueMap, productLevel3Attribute, value);
            }
          }
        }
      }
    }
  }

  private List<String> deleteExistingVariantCreationAttributes(Product product, Map<String, List<String>> existingAttributeCodeToValuesMap,
      Map<String, List<String>> attributeCodeToValuesRequestMap, ProductAttribute productLevel3Attribute,
      List<String> deletedAttributeCodes, ProductLevel3 request) {
    List<String> valuesInExistingProduct =
        existingAttributeCodeToValuesMap.get(productLevel3Attribute.getAttribute().getAttributeCode());
    if (!deletedAttributeCodes.contains(productLevel3Attribute.getAttribute().getAttributeCode())
        && !productLevel3Attribute.isMarkForDelete() && CollectionUtils.isNotEmpty(request.getDeletedItems())) {
      for (String existingAttributeValue : valuesInExistingProduct) {
        if (!attributeCodeToValuesRequestMap.getOrDefault(productLevel3Attribute.getAttribute().getAttributeCode(),
            new ArrayList<>()).contains(existingAttributeValue)) {
          log.info("Existing defining attribute value deleted : {} , for productCode : {} ", existingAttributeValue,
              product.getProductCode());
          if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
              .equals(productLevel3Attribute.getAttribute().getAttributeType().name())) {
            productLevel3Attribute.getProductAttributeValues().removeIf(
                productAttributeValue -> existingAttributeValue.equals(
                    productAttributeValue.getDescriptiveAttributeValue()));
          } else {
            productLevel3Attribute.getProductAttributeValues()
                .removeIf(productAttributeValue -> existingAttributeValue.equals(ValueTypeUtil.getValueTypeAndValue(
                    productAttributeValue.getAllowedAttributeValue().getValueType(),
                    productAttributeValue.getAllowedAttributeValue().getValue(),
                    sizeChartValueTypeDelimiter, valueTypeAdditionForDefiningAttributes)));
          }
        }
      }
    }
    return valuesInExistingProduct;
  }

  private void getProductItemAttributeMap(Map<String, ProductItemAttributeValueRequest> productItemAttributeValueMap,
      ProductAttribute productLevel3Attribute, String value) {
    value = ValueTypeUtil.getValue(value, sizeChartValueTypeDelimiter, valueTypeAdditionForDefiningAttributes);
    AttributeRequest attributeRequest = new AttributeRequest();
    BeanUtils.copyProperties(productLevel3Attribute.getAttribute(), attributeRequest);
    ProductItemAttributeValueRequest productItemAttributeValueRequest =
        new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemAttributeValueRequest.setValue(value);
    getItemAttributeValueMap(productItemAttributeValueMap, value, productLevel3Attribute.getAttribute().getAttributeCode(),
        productItemAttributeValueRequest);
  }

  private boolean isVariantCreationOrBrandAttribute(ProductAttribute productAttribute,
      List<String> brandAttributeCodeList, boolean validateAttributesByBrandCode) {
    boolean isVariantCreatingOrDefiningAttribute =
        productAttribute.getAttribute().isVariantCreation() || AttributeType.DEFINING_ATTRIBUTE.name()
            .equals(productAttribute.getAttribute().getAttributeType().name());
    if (validateAttributesByBrandCode) {
      return isVariantCreatingOrDefiningAttribute || brandAttributeCodeList.contains(
          productAttribute.getAttribute().getAttributeCode());
    } else {
      return isVariantCreatingOrDefiningAttribute || BRAND.equalsIgnoreCase(productAttribute.getProductAttributeName());
    }
  }


  @Override
  @Trace(dispatcher = true)
  public ItemBulkArchiveResponse toggleArchiveProducts(List<String> productSkuList, boolean doArchive) throws Exception {
    List<String> failedProductSkus = new ArrayList<>();
    String errorCode = null;
    Map<String, String> productSkuErrorMessageMap = new HashMap<>();
    boolean checkLimit = false;
    if (productLimitSwitchEnabled && CollectionUtils.isNotEmpty(productSkuList)) {
      ProfileResponse profileResponse = productService
          .getProfileResponse(CommonUtils.extractMerchantCodeFromProductSku(productSkuList.stream().findFirst().get()));
      checkLimit = CommonUtils.getBusinessPartnerFlagValue(profileResponse, Constants.PRODUCT_LIMIT);
    }
    for (String productSku : productSkuList) {
      try {
        String errorCodeMessage = toggleArchiveProduct(productSku, doArchive, productSkuErrorMessageMap, checkLimit);
        if (StringUtils.isEmpty(errorCode)) {
          errorCode = errorCodeMessage;
        }
      } catch (Exception e) {
        LOGGER.error("Error while performing archive operation for product sku : {} , doArchive : {} ", productSku,
            doArchive, e);
        failedProductSkus.add(productSku);
      }
    }
    return new ItemBulkArchiveResponse(failedProductSkus, errorCode, productSkuErrorMessageMap);
  }

  private String toggleArchiveProduct(String productSku, boolean doArchive,
      Map<String, String> productSkuErrorMessageMap, boolean productLimit) throws Exception {
    if (productLimit && !doArchive) {
      String businessPartnerCode = CommonUtils.extractMerchantCodeFromProductSku(productSku);
      ApiErrorCode productLimitApiErrorCode =
          productService.checkProductLimitExceeded(businessPartnerCode, DEFAULT_STORE_ID);
      if (Objects.nonNull(productLimitApiErrorCode)) {
        log.error("Product Limit Exceeded for businessPartnerCode {} ", businessPartnerCode);
        productSkuErrorMessageMap.put(productSku, productLimitApiErrorCode.getDesc());
        return productLimitApiErrorCode.getCode();
      }
    }
    ProductL3Response productL3Response = xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
    ApiErrorCode apiErrorCode = checkProductStatus(productL3Response);
    if (Objects.nonNull(apiErrorCode) && apiErrorCode != ApiErrorCode.ITEM_IS_ARCHIVED) {
      LOGGER.error("Product sku not in correct state : {} , error message : {}", productSku, apiErrorCode.getDesc());
      productSkuErrorMessageMap.put(productSku, apiErrorCode.getDesc());
      return productSku;
    }
    if (doArchive == productL3Response.isArchived()) {
      LOGGER.warn("Product sku already is required state : productSku : {} doArchive : {}", productSku, doArchive);
      productSkuErrorMessageMap.put(productSku, ApiErrorCode.PRODUCT_IS_ARCHIVED.getDesc());
      return productSku;
    }
    if (!doArchive && !productL3Response.isActiveL5Mapped()) {
      LOGGER.error("Product can't be unarchived, as no active L5s will be left : productSku : {} doArchive : {}",
          productSku, doArchive);
      return ApiErrorCode.FAILED_TO_UNARCHIVE_NO_ACTIVE_L5.getCode();
    }
    String merchantCode = productL3Response.getMerchantCode();
    this.xProductOutbound.toggleArchiveProduct(productSku, doArchive);
    archiveProductStockAlert(doArchive, productL3Response, merchantCode);
    this.updatedProductHistoryService.createProductL3AuditLog(merchantCode, Constants.DEFAULT, productL3Response.getProductSku(),
        productL3Response.getMasterDataProduct().getProductName(), UpdateProductActivity.ARCHIVE.getDesc(),
        String.valueOf(!doArchive), String.valueOf(doArchive), false, StringUtils.EMPTY);
    return null;
  }

  private void archiveProductStockAlert(boolean doArchive, ProductL3Response productL3Response,
    String merchantCode) throws Exception {
    for (String itemSku : productL3Response.getItemSkus()) {
      if (doArchive) {
        if (!skipProductLevel3AggregatorCreation) {
          productLevel3AggregatorService.updateState(itemSku, merchantCode, ProductLevel3AggregatorState.ARCHIVED);
        }
        this.archiveProductStockAlert(merchantCode, itemSku, true, 0);
      } else {
        if (!skipProductLevel3AggregatorCreation) {
          productLevel3AggregatorService.updateState(itemSku, merchantCode, ProductLevel3AggregatorState.ACTIVE);
        }
        this.archiveProductStockAlert(merchantCode, itemSku, false, 0);
      }
    }
  }

  @Override
  public List<ProductSuspensionHistoryResponse> getProductSuspensionHistoryByProductSkus(
      String storeId, List<String> productSkuList) {
    List<ProductSuspensionHistoryResponse> productSuspensionHistoryResponses = new ArrayList<>();
    for (String productSku : productSkuList) {
      ProductSuspensionHistory productSuspensionHistory = productSuspensionHistoryRepository
          .findTop1ByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId,
              productSku);
      if (Objects.nonNull(productSuspensionHistory)) {
        productSuspensionHistoryResponses.add(
            ProductSuspensionHistoryResponse.builder().productSku(productSku)
                .reason(productSuspensionHistory.getDescription()).status(
                Objects.nonNull(productSuspensionHistory.getStatus()) ?
                    productSuspensionHistory.getStatus().name() :
                    StringUtils.EMPTY).build());
      }
    }
    return productSuspensionHistoryResponses;
  }

  @Override
  public Map<String, String> getProductIdProductCodeMap(String storeId, List<String> productIds) {
    List<ProductCollection> productCollections =
        productCollectionRepository.findByStoreIdAndProductIds(storeId, productIds);
    return productCollections.stream()
        .collect(Collectors.toMap(ProductCollection::getProductId, ProductCollection::getProductCode));
  }

  @Override
  public CogsValueResponse fetchCogsValueByMaterialCode(String materialCode) {
    return sapOutbound.getCogsValueResponse(materialCode);
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse updateImages(String storeId, ProductImageEditRequest request) throws Exception {
    LOGGER.info("Updating images for product sku : {} : request {} and storeId : {}", request.getProductSku(), request,
        storeId);
    ProductL3Response productL3Response =
        xProductOutbound.getProductDetailsByProductSku(request.getProductSku()).getValue();
    ItemsPriceStockImagesUpdateResponse response = new ItemsPriceStockImagesUpdateResponse();
    ApiErrorCode apiErrorCode = checkProductStatus(productL3Response);
    if (Objects.nonNull(apiErrorCode)) {
      LOGGER.error("Product sku not in correct state : {} , error message : {}", request.getProductSku(),
          apiErrorCode.getDesc());
      response.setApiErrorCode(apiErrorCode);
      return response;
    }
    if (!productL3Response.isProductEditable()) {
      LOGGER.error("Cannot update image of shared product : {} ", request.getProductSku());
      response.setApiErrorCode(ApiErrorCode.CANNOT_UPDATE_IMAGE_FOR_SHARED_PRODUCT);
      return response;
    }
    getProductItemsAndSynchronizeProduct(request, productL3Response);

    //image update in PCB and review check
    com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest productImageEditRequestPCB =
        new com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest();
    ConverterUtil.toProductImageEditRequest(request, productL3Response.getProductCode(),
        productL3Response.getItemSkuItemCodeMap(), productImageEditRequestPCB);
    productImageEditRequestPCB.setActivatedBefore(true);

    LOGGER.info("Updating images in PCB for product sku : {} , request : {}", request.getProductSku(),
        productImageEditRequestPCB);
    Map<String, String> itemImagesUpdateStatus = productOutbound.updateImages(productImageEditRequestPCB);
    //Merchant config and category check for review type
    return productGoingForReviewIsPostLive(storeId, request, productL3Response, itemImagesUpdateStatus);
  }

  public void getProductItemsAndSynchronizeProduct(ProductImageEditRequest request, ProductL3Response productL3Response)
      throws Exception {
    if (!productL3Response.isSynchronized()) {
      ProductAndItemsResponse productAndItemsResponse =
          xProductOutbound.getProductAndItemsWithProductData(true, request.getProductSku(), true, false).getValue();
      com.gdn.x.productcategorybase.dto.request.ProductRequest productRequest =
          ConverterUtil.toProductRequest(productAndItemsResponse);
      productOutbound.migrateProduct(request.getProductSku(), productL3Response.getProductCode(),
          request.getBusinessPartnerCode(), productRequest);
      this.productLevel3Repository.synchronizeProduct(request.getProductSku());
    }
  }

  private ItemsPriceStockImagesUpdateResponse productGoingForReviewIsPostLive(String storeId, ProductImageEditRequest request,
      ProductL3Response productL3Response, Map<String, String> errorsResponse) throws Exception {
    ItemsPriceStockImagesUpdateResponse response = new ItemsPriceStockImagesUpdateResponse();
    Map<String, String> itemSkuItemCodeMap = productL3Response.getItemSkuItemCodeMap();
    ProfileResponse profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(
      productL3Response.getMerchantCode());
    if (MapUtils.isNotEmpty(errorsResponse)) {
      Map<String, String> itemCodeItemSkuMap = new HashMap<>();
      for (Map.Entry<String, String> entry : itemSkuItemCodeMap.entrySet()) {
        itemCodeItemSkuMap.put(entry.getValue(), entry.getKey());
      }
      List<VariantsErrorListResponse> variantsErrorList = new ArrayList<>();
      for (Map.Entry<String, String> entry : errorsResponse.entrySet()) {
        if (Constants.COPY_ALL_STATUS.equals(entry.getKey())) {
          String status = errorsResponse.get(Constants.COPY_ALL_STATUS);
          if (Constants.FAIL.equals(status)) {
            VariantsErrorListResponse variantsErrorListResponse = new VariantsErrorListResponse();
            variantsErrorListResponse.setItemSku(request.getProductSku());
            variantsErrorListResponse.setMessage(ApiErrorCode.ALL_VARIANTS_ERROR.getDesc());
            variantsErrorListResponse.setCode(ApiErrorCode.ALL_VARIANTS_ERROR.getCode());
            variantsErrorList.add(variantsErrorListResponse);
          }
          continue;
        }
        VariantsErrorListResponse variantsErrorListResponse = new VariantsErrorListResponse();
        variantsErrorListResponse.setItemSku(itemCodeItemSkuMap.get(entry.getKey()));
        variantsErrorListResponse.setMessage(entry.getValue());
        variantsErrorListResponse.setCode(ConverterUtil.getErrorCode(entry.getValue()).getCode());
        variantsErrorList.add(variantsErrorListResponse);
      }
      response.setVariantsErrorList(variantsErrorList);
    }

    List<ImageRequest> editedImages = new ArrayList<>();
    if (request.isImageAdded()) {
      String hashCode = request.getHashCode();
      String location = fileStorageService.generateFinalImageFullPath(request.getImagePath());
      ImageRequest imageRequest =  new ImageRequest(location.substring(location.lastIndexOf("/") + 1, location.length()), location, hashCode);
      if (Objects.nonNull(request.getCopyToAllVariantImages())) {
        if (request.getCopyToAllVariantImages().isAdd() || request.getCopyToAllVariantImages()
          .isCopy()) {
          imageRequest.setCommonImage(true);
        }
      }
      editedImages.add(imageRequest);
    }

    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(storeId, request.getProductCode());
    this.xProductOutbound.generateProductScoreByProductSkuOrProductCode(request.getProductSku(), null, false);
    boolean postLive = productCollection.isPostLive();
    if (request.isImageAdded() || request.isImageUpdated()) {
      List<ConfigurationStatusResponse> responseList = new ArrayList<>();
      if (!INTERNAL.equalsIgnoreCase(productL3Response.getMerchantCode())) {
        responseList = this.productRepository.getConfigurationStatus(
            ConverterUtil.toConfigurationStatusRequestList(productL3Response.getMerchantCode(),
                productL3Response.getMasterCatalog().getCategory().getCategoryCode()));
      }
      if (CollectionUtils.isNotEmpty(responseList) && POST_LIVE.equalsIgnoreCase(
          responseList.get(0).getReviewConfig())) {
        postLive = true;
      }
      List<String> reviewTypeList = new ArrayList<>();
      if (Objects.nonNull(productCollection.getReviewType())) {
        reviewTypeList = Stream.of(productCollection.getReviewType().split(",")).collect(Collectors.toList());
      }
      if (request.isImageAdded()) {
        if (!postLive) {
          takeDownOrReactivateProduct(storeId, request.getProductSku(), true, null, null);
        }
        productCollection.setEdited(true);
        productCollection.setPostLive(postLive);
        if (CollectionUtils.isEmpty(reviewTypeList) || !reviewTypeList.contains(IMAGE)) {
          reviewTypeList.add(IMAGE);
          productCollection.setReviewPending(true);
          productCollection.setReviewType(String.join(",", reviewTypeList));
        }
        //updating flags in PBP prd_product_collection and edit reviewType
        productCollectionRepository.saveAndFlush(productCollection);
        response.setProductReview(true);
        //updating solr review_pending flag is getting changes
        updateSolrProductCollectionDocument(productCollection);
      } else if (request.isImageUpdated() && productCollection.isReviewPending()) {
        //publish Image refresh
        if (productCollection.isReviewPending()) {
          productCollection.setEdited(true);
        }
        response.setProductReview(true);
        productCollectionRepository.saveAndFlush(productCollection);
        ProductBusinessPartner productBusinessPartner = productBusinessPartnerService
            .findFirstByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
        AddEditedProductToPDTEvent addEditedProductToPDTEvent =
            ConverterUtil.toAddEditedProductToPDTEvent(storeId, IMAGE_REFRESH, productCollection, null,
              profileResponse, productBusinessPartner, priceInfoVendorEditedEnabled,
                priceInfoMaxVariantLimit);
        LOGGER.info("Publishing ADD_EDITED_PRODUCT_TO_PDT event for product code : {} ",
            productCollection.getProductCode());
        SellerDetailResponse sellerDetailResponse = null;
        try {
          sellerDetailResponse = productAnalyticsOutbound.getSellerDetail(productL3Response.getMerchantCode());
        }catch (Exception e)
        {
          log.error("Exception while getting sellerDetail Response from ProductAnalytics for businessPartnerCode : {} ",
              productCollection.getProductCode(), e);
        }
        if (Objects.nonNull(sellerDetailResponse)) {
          addEditedProductToPDTEvent.setSellerBadge(sellerDetailResponse.getSellerBadge());
        }
        kafkaProducer.send(kafkaTopicProperties.getVendorCombinedEventNoPriority(), productCollection.getProductCode(),
            AddProductToVendorCombinedEventModel.builder().addEditedProductToPDTEvent(addEditedProductToPDTEvent)
                .build());
      }
    }
    if (request.isImageAdded()) {
      productPublisherService.publishEditImageResizeEvent(
          new EditedImageResizeEvent(request.getProductCode(), DEFAULT_STORE_ID, editedImages));
    }
    response.setPostLive(postLive);
    createAuditLogs(request.getBusinessPartnerCode(), request, productL3Response, errorsResponse);
    return response;
  }

  private void createAuditLogs(String businessPartnerCode, ProductImageEditRequest request,
      ProductL3Response productL3Response, Map<String, String> errorsResponse) throws Exception {
    String newImageActivity = UpdateProductActivity.IMAGE_ADDED.getDesc();
    String deleteImageActivity = UpdateProductActivity.IMAGE_DELETED.getDesc();
    String updateImageActivity = UpdateProductActivity.IMAGE_UPDATED.getDesc();
    String productName = productL3Response.getMasterDataProduct().getProductName();

    if (CollectionUtils.isNotEmpty(request.getProductItems())) {
      Map<String, String> itemSkuItemCodeMap = productL3Response.getItemSkuItemCodeMap();
      for (ItemImageEditRequest itemImageEditRequest : request.getProductItems()) {
        String itemSku = itemImageEditRequest.getItemSku();
        String itemCode = itemSkuItemCodeMap.get(itemSku);
        if (errorsResponse.containsKey(itemCode)) {
          continue;
        }
        if (itemImageEditRequest.isAdd() || itemImageEditRequest.isCopy()) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, itemSku, request.getProductSku(),
              productName, newImageActivity, StringUtils.EMPTY, request.getImagePath(), false, StringUtils.EMPTY);
        }
        if (itemImageEditRequest.isMarkForDelete()) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, itemSku, request.getProductSku(),
              productName, deleteImageActivity, StringUtils.EMPTY, request.getImagePath(), false, StringUtils.EMPTY);
        }
        if (itemImageEditRequest.isMainImage()) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, itemSku, request.getProductSku(),
              productName, updateImageActivity, (MAIN_IMAGE + COLONE + false + COLONE + request.getImagePath()),
              (MAIN_IMAGE + COLONE + true + COLONE + request.getImagePath()), false, StringUtils.EMPTY);
        }
      }
    }

    if (Objects.nonNull(request.getCopyToAllVariantImages())) {
      String status = errorsResponse.get(Constants.COPY_ALL_STATUS);
      if (!(StringUtils.isBlank(status) || Constants.FAIL.equals(status))) {
        CopyImageEditRequest copyImageEditRequest = request.getCopyToAllVariantImages();
        if (copyImageEditRequest.isAdd() || copyImageEditRequest.isCopy()) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT,
              request.getProductSku(), productName, newImageActivity, StringUtils.EMPTY, request.getImagePath(), false,
              StringUtils.EMPTY);
        }
        if (copyImageEditRequest.isMarkForDelete()) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT,
              request.getProductSku(), productName, deleteImageActivity, StringUtils.EMPTY, request.getImagePath(),
              false, StringUtils.EMPTY);
        }
        if (copyImageEditRequest.isMainImage()) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, Constants.DEFAULT,
              request.getProductSku(), productName, updateImageActivity,
              (MAIN_IMAGE + COLONE + false + COLONE + request.getImagePath()),
              (MAIN_IMAGE + COLONE + true + COLONE + request.getImagePath()), false, StringUtils.EMPTY);
        }
      }
    }
  }

  private ProductLevel3Service getProductLevel3Service() {
    return applicationContext.getBean(ProductLevel3Service.class);
  }

  @Override
  public void processSellerPenaltyChecks(ProductVariantUpdateRequest productVariantUpdateRequest,
    ProfileResponse profileResponse, boolean sellerPenaltyEnabledPhase2) {
    // Add pickup points
    validateStockIncrementRestrictions(profileResponse, sellerPenaltyEnabledPhase2,
      productVariantUpdateRequest.getAddPickupPoints());

    // Modified pickup points inside product items
    Optional.ofNullable(productVariantUpdateRequest.getProductItems()).orElse(List.of()).stream()
      .filter(Objects::nonNull)
      .map(ProductVariantPriceStockAndImagesRequest::getModifiedItemPickupPoints).forEach(
        pickupPoints -> validateStockIncrementRestrictions(profileResponse,
          sellerPenaltyEnabledPhase2, pickupPoints));
  }

  private void validateStockIncrementRestrictions(ProfileResponse profileResponse,
    boolean sellerPenaltyEnabledPhase2, List<ItemPickupPointRequest> pickupPoints) {
    Optional.ofNullable(pickupPoints).orElse(List.of()).stream().filter(Objects::nonNull)
      .forEach(pickupPoint -> {
        Integer stock = pickupPoint.getStock();
        if (CommonUtils.validateStockIncrementForSellerPenalty(profileResponse,
          sellerPenaltyEnabledPhase2, stock)) {
          log.error(
            "Stock update failed: You are not allowed to increase stock due to seller penalty "
              + "restrictions for stock value: {}", stock);
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ApiErrorCode.SELLER_PENALTY_RESTRICTION.getDesc());
        }
      });
  }
}

