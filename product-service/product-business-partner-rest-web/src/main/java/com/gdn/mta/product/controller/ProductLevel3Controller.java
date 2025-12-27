package com.gdn.mta.product.controller;

import static org.apache.commons.lang3.StringUtils.EMPTY;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.BrandAndCategoryItemSummaryRequest;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.ItemBulkArchiveRequest;
import com.gda.mta.product.dto.ItemImageEditRequest;
import com.gda.mta.product.dto.ItemSkuListRequest;
import com.gda.mta.product.dto.PickupPointRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductItemLevel3LogisticResponse;
import com.gda.mta.product.dto.ProductItemLevel3OrderResponse;
import com.gda.mta.product.dto.ProductItemLevel3Request;
import com.gda.mta.product.dto.ProductItemLevel3Response;
import com.gda.mta.product.dto.ProductL3SummaryRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.ProductLevel3DTO;
import com.gda.mta.product.dto.ProductLevel3DashboardResponse;
import com.gda.mta.product.dto.ProductLevel3ImageRequest;
import com.gda.mta.product.dto.ProductLevel3ImageResponse;
import com.gda.mta.product.dto.ProductLevel3LogisticsRequest;
import com.gda.mta.product.dto.ProductLevel3OrderResponse;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3QuickEditRequest;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3StockRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryCountResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigStockRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductScoreResponse;
import com.gda.mta.product.dto.ProductSkuListRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.SuspensionProductRequestList;
import com.gda.mta.product.dto.UpdateImageRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.UsageResponse;
import com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse;
import com.gda.mta.product.dto.response.InProgressProductsBySizeChartCodeResponse;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.PreOrderResponse;
import com.gda.mta.product.dto.response.ProductItemNameResponse;
import com.gda.mta.product.dto.response.ProductL3SummaryResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.service.ProductLevel3V2Service;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.converter.ProductLevel3RequestConverter;
import com.gdn.mta.product.converter.ProductLevel3ResponseConverter;
import com.gdn.mta.product.entity.BulkDownloadProductLevel3Summary;
import com.gdn.mta.product.entity.PickupPointCodeResponse;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductItemLevel3Order;
import com.gdn.mta.product.entity.ProductItemWholesalePriceVo;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductLevel3Dashboard;
import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.ProductLevel3Order;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetails;
import com.gdn.mta.product.entity.ProductLevel3UpdateSummary;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.entity.ProductSuspensionHistory;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.UniquePickupPointCodeResponse;
import com.gdn.mta.product.entity.UsageStatus;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductLevel3Wrapper;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.exception.ApiIncorrectInputDataException;
import com.gdn.mta.product.service.exception.InvalidDataStateException;
import com.gdn.mta.product.service.exception.ProductInNeedCorrectionException;
import com.gdn.mta.product.service.exception.ProductInReviewException;
import com.gdn.mta.product.service.exception.ProductRejectedException;
import com.gdn.mta.product.util.ControllerUtils;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.mta.product.util.ValidateUrlUtil;
import com.gdn.mta.product.util.validator.ValidationUtil;
import com.gdn.mta.product.valueobject.EstimateItemPriceDTO;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryCount;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilterDetails;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.mta.product.web.model.ProductControllerErrorMessage;
import com.gdn.mta.product.web.model.ProductLevel3ControllerErrorMessage;
import com.gdn.mta.product.web.model.ProductLevel3ControllerPath;
import com.gdn.partner.pbp.annotations.AuditLog;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.EstimateItemPriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.google.api.services.youtube.YouTube;
import com.newrelic.api.agent.Trace;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;

/**
 * Move to class {@link com.gdn.partners.pbp.controller.productlevel3.ProductLevel3Controller}.
 * Please as much as possible add new method in new class.
 */
@Slf4j
@RestController("ProductLevel3ControllerOld")
@RequestMapping(value = ProductLevel3ControllerPath.BASE_PATH)
@Tag(name = "ProductLevel3ControllerOld", description = "Product Level 3 Service API")
public class ProductLevel3Controller {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductLevel3Controller.class);
  private static final String FAILED_GET_SUSPENDED_ITEM_LIST = "Failed to get the suspended item list for request : {}";
  private static final String FAILED_GET_ALL_PRODUCTS = "Failed to get the products for suspension for request : {}";
  private static final String FAILED_SUSPENSION_ACTION = "Failed to do suspension action for productCodes : {}";
  private static final int PRODUCT_NAME_LIMIT = 150;
  private static final String COMMA_SEPARATOR = ",";

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductLevel3Wrapper productLevel3Wrapper;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Autowired
  private ProductLevel3RequestConverter productLevel3RequestConverter;

  @Autowired
  private ProductLevel3ResponseConverter productLevel3ResponseConverter;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  private ProductLevel3V2Service productLevel3V2Service;

  @Autowired
  private YouTube youTube;

  @Value(value = "${youtube.data.api.key}")
  private String youTubeDataApiKey;

  @Value("${max.inventory.request:10}")
  private int maxInventoryRequest;

  @Value("${preOrder.maximum.days}")
  private int preOrderMaximumDays;

  @Value("${preOrder.working.maximum.week}")
  private int preOrderMaximumWeek;

  @Value("${max.characters.in.description}")
  private int maximumCharactersInDescription;

  @Value("${max.characters.without.formatting.description}")
  private int maximumCharactersWithoutFormattingDescription;

  @Value("${migrate.product.in.other.logistic.update.flow}")
  private boolean migrateProductInOtherLogisticUpdateFlow;

  @Value("${youtube.regex}")
  private String youtubeRegex;

  @Value("${convert.pre.order.date.to.jkt}")
  private boolean convertPreOrderDateToJKT;

  private List<ProductLevel3SummaryResponse> generateProductLevel3SummaryResponses(
      Page<ProductLevel3Summary> productLevel3Summary, boolean removeForceReviewItems) throws Exception {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses =
        new ArrayList<ProductLevel3SummaryResponse>();
    for (ProductLevel3Summary productLevel3SummaryItem : productLevel3Summary.getContent()) {
      ProductLevel3SummaryResponse productLevel3SummaryResponse =
          new ProductLevel3SummaryResponse();
      if (removeForceReviewItems && productLevel3SummaryItem.isForceReview()) {
        continue;
      }
      BeanUtils.copyProperties(productLevel3SummaryItem, productLevel3SummaryResponse, "prices", "viewConfigs",
          "images");
      productLevel3SummaryResponse.setEnableEdit(!productLevel3SummaryItem.isForceReview());
      productLevel3SummaryResponse.setArchived(productLevel3SummaryItem.getIsArchived());
      productLevel3SummaryResponse.setPrices(new ArrayList<>());
      productLevel3SummaryResponse.setViewConfigs(new ArrayList<>());
      productLevel3SummaryResponse.setImages(new ArrayList<>());
      for (ProductLevel3Price productLevel3Price : productLevel3SummaryItem.getPrices()) {
        ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
        BeanUtils.copyProperties(productLevel3Price, productLevel3PriceResponse);
        productLevel3SummaryResponse.getPrices().add(productLevel3PriceResponse);
      }
      for (ProductLevel3ViewConfig productLevel3ViewConfig : productLevel3SummaryItem
          .getViewConfigs()) {
        ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
            new ProductLevel3ViewConfigResponse();
        BeanUtils.copyProperties(productLevel3ViewConfig, productLevel3ViewConfigResponse);
        productLevel3SummaryResponse.getViewConfigs().add(productLevel3ViewConfigResponse);
      }
      for (ProductLevel3Image productLevel3Image : productLevel3SummaryItem.getImages()) {
        ProductLevel3ImageResponse productLevel3ImageResponse = new ProductLevel3ImageResponse();
        BeanUtils.copyProperties(productLevel3Image, productLevel3ImageResponse);
        productLevel3SummaryResponse.getImages().add(productLevel3ImageResponse);
      }
      productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    }
    return productLevel3SummaryResponses;
  }

  private ProductLevel3 generateProductLevel3(ProductLevel3Request request) throws Exception {
    ProductLevel3 product = new ProductLevel3();
    BeanUtils.copyProperties(request, product, "items", "attributes", "images");
    product.setItems(new ArrayList<>());
    product.setAttributes(new ArrayList<>());
    product.setImages(new ArrayList<>());
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(request.getProductLevel3LogisticsRequest())) {
      for (ProductLevel3LogisticsRequest productLevel3LogisticsRequest : request
          .getProductLevel3LogisticsRequest()) {
        ProductLevel3Logistics productLevel3Logistics = ProductLevel3Logistics.builder()
            .logisticProductCode(productLevel3LogisticsRequest.getLogisticProductCode())
            .selected(productLevel3LogisticsRequest.isSelected()).build();
        logistics.add(productLevel3Logistics);
      }
    }
    if (Objects.nonNull(request.getItems())) {
      for (ProductItemLevel3Request itemRequest : request.getItems()) {
        ProductItemLevel3 itemLevel3 = new ProductItemLevel3();
        BeanUtils.copyProperties(itemRequest, itemLevel3, "prices", "viewConfigs", "images");
        itemLevel3.setViewConfigs(new ArrayList<>());
        itemLevel3.setImages(new ArrayList<>());
        if (CollectionUtils.isNotEmpty(itemRequest.getPrices())) {
          itemLevel3.setPrices(new ArrayList<>());
          for (ProductLevel3PriceRequest level3PriceRequest : itemRequest.getPrices()) {
            ProductLevel3Price level3Price = new ProductLevel3Price();
            BeanUtils.copyProperties(level3PriceRequest, level3Price);
            itemLevel3.getPrices().add(level3Price);
          }
        }
        for (ProductLevel3ViewConfigRequest configRequest : itemRequest.getViewConfigs()) {
          ProductLevel3ViewConfig viewConfig = new ProductLevel3ViewConfig();
          BeanUtils.copyProperties(configRequest, viewConfig);
          itemLevel3.getViewConfigs().add(viewConfig);
        }
        for (ProductLevel3ImageRequest imageRequest : itemRequest.getImages()) {
          ProductLevel3Image image = new ProductLevel3Image();
          BeanUtils.copyProperties(imageRequest, image);
          itemLevel3.getImages().add(image);
        }
        itemLevel3.setLogistics(logistics);
        product.getItems().add(itemLevel3);
      }
    }
    for (ProductLevel3AttributeRequest attributeRequest : request.getAttributes()) {
      ProductLevel3Attribute attribute = new ProductLevel3Attribute();
      BeanUtils.copyProperties(attributeRequest, attribute);
      product.getAttributes().add(attribute);
    }
    for (ProductLevel3ImageRequest imageRequest : request.getImages()) {
      ProductLevel3Image image = new ProductLevel3Image();
      BeanUtils.copyProperties(imageRequest, image);
      product.getImages().add(image);
    }
    product.setFreeSample(request.isFreeSample());
    return product;
  }

  private ProductLevel3SummaryResponse generateProductLevel3SummaryResponse(
      ProductLevel3Summary productLevel3Summary) throws Exception {
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    BeanUtils.copyProperties(productLevel3Summary, productLevel3SummaryResponse, "prices",
        "viewConfigs", "images");
    productLevel3SummaryResponse.setEnableEdit(!productLevel3Summary.isForceReview());
    productLevel3SummaryResponse.setPrices(new ArrayList<>());
    productLevel3SummaryResponse.setViewConfigs(new ArrayList<>());
    productLevel3SummaryResponse.setImages(new ArrayList<>());
    for (ProductLevel3Price productLevel3Price : productLevel3Summary.getPrices()) {
      ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
      BeanUtils.copyProperties(productLevel3Price, productLevel3PriceResponse);
      productLevel3SummaryResponse.getPrices().add(productLevel3PriceResponse);
    }
    for (ProductLevel3ViewConfig productLevel3ViewConfig : productLevel3Summary.getViewConfigs()) {
      ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
          new ProductLevel3ViewConfigResponse();
      BeanUtils.copyProperties(productLevel3ViewConfig, productLevel3ViewConfigResponse);
      productLevel3SummaryResponse.getViewConfigs().add(productLevel3ViewConfigResponse);
    }
    for (ProductLevel3Image productLevel3Image : productLevel3Summary.getImages()) {
      ProductLevel3ImageResponse productLevel3ImageResponse = new ProductLevel3ImageResponse();
      BeanUtils.copyProperties(productLevel3Image, productLevel3ImageResponse);
      productLevel3SummaryResponse.getImages().add(productLevel3ImageResponse);
    }
    productLevel3SummaryResponse.setArchived(productLevel3Summary.getIsArchived());
    return productLevel3SummaryResponse;
  }

  private ProductLevel3Response generateProductLevel3Response(ProductLevel3 productLevel3) {
    ProductLevel3Response productLevel3Response = new ProductLevel3Response();
    BeanUtils.copyProperties(productLevel3, productLevel3Response, "items", "attributes", "images");
    productLevel3Response.setItems(new ArrayList<>());
    productLevel3Response.setAttributes(new ArrayList<>());
    productLevel3Response.setImages(new ArrayList<>());
    for (ProductItemLevel3 productItemLevel3 : productLevel3.getItems()) {
      ProductItemLevel3Response productItemLevel3Response = new ProductItemLevel3Response();
      BeanUtils.copyProperties(productItemLevel3, productItemLevel3Response, "prices", "viewConfigs", "images");
      productItemLevel3Response.setViewConfigs(new ArrayList<>());
      productItemLevel3Response.setImages(new ArrayList<>());
      productItemLevel3Response.setProductItemWholesalePriceResponses(new ArrayList<>());
      productItemLevel3Response.setDisableUnSync(productItemLevel3.getDisableUnSync());
      productItemLevel3Response.setProductItemLevel3LogisticResponse(new ArrayList<>());
      if (CollectionUtils.isNotEmpty(productItemLevel3.getPrices())) {
        productItemLevel3Response.setPrices(new ArrayList<>());
        for (ProductLevel3Price productLevel3Price : productItemLevel3.getPrices()) {
          ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
          BeanUtils.copyProperties(productLevel3Price, productLevel3PriceResponse);
          productItemLevel3Response.getPrices().add(productLevel3PriceResponse);
        }
      }
      for (ProductLevel3ViewConfig productLevel3ViewConfig : productItemLevel3.getViewConfigs()) {
        ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
            new ProductLevel3ViewConfigResponse();
        BeanUtils.copyProperties(productLevel3ViewConfig, productLevel3ViewConfigResponse);
        productItemLevel3Response.getViewConfigs().add(productLevel3ViewConfigResponse);
      }
      for (ProductLevel3Image productLevel3Image : productItemLevel3.getImages()) {
        ProductLevel3ImageResponse productLevel3ImageResponse = new ProductLevel3ImageResponse();
        BeanUtils.copyProperties(productLevel3Image, productLevel3ImageResponse);
        productItemLevel3Response.getImages().add(productLevel3ImageResponse);
      }

      for (ProductItemWholesalePriceVo productItemWholesalePriceVo : productItemLevel3
          .getProductItemWholesalePrices()) {
        ProductItemWholesalePriceResponse productItemWholesalePriceResponse = new ProductItemWholesalePriceResponse();
        BeanUtils.copyProperties(productItemWholesalePriceVo, productItemWholesalePriceResponse);
        productItemLevel3Response.getProductItemWholesalePriceResponses().add(productItemWholesalePriceResponse);
      }
      if (CollectionUtils.isNotEmpty(productItemLevel3.getLogistics())) {
        for (ProductLevel3Logistics logistics : productItemLevel3.getLogistics()) {
          ProductItemLevel3LogisticResponse productItemLevel3LogisticResponse =
              new ProductItemLevel3LogisticResponse();
          BeanUtils.copyProperties(logistics, productItemLevel3LogisticResponse);
          productItemLevel3Response.getProductItemLevel3LogisticResponse()
              .add(productItemLevel3LogisticResponse);
        }
      }
      productLevel3Response.getItems().add(productItemLevel3Response);
    }
    for (ProductLevel3Attribute productLevel3Attribute : productLevel3.getAttributes()) {
      ProductLevel3AttributeResponse productLevel3AttributeResponse =
          new ProductLevel3AttributeResponse();
      BeanUtils.copyProperties(productLevel3Attribute, productLevel3AttributeResponse);
      productLevel3Response.getAttributes().add(productLevel3AttributeResponse);
    }
    for (ProductLevel3Image productLevel3Image : productLevel3.getImages()) {
      ProductLevel3ImageResponse productLevel3ImageResponse = new ProductLevel3ImageResponse();
      BeanUtils.copyProperties(productLevel3Image, productLevel3ImageResponse);
      productLevel3Response.getImages().add(productLevel3ImageResponse);
    }
    if (Objects.nonNull(productLevel3.getProductScore())) {
      ProductScoreResponse productScoreResponse = new ProductScoreResponse();
      BeanUtils.copyProperties(productLevel3.getProductScore(), productScoreResponse);
      productLevel3Response.setProductScore(productScoreResponse);
    }
    if (Objects.nonNull(productLevel3.getPreOrder())) {
      PreOrderResponse preOrderResponse = new PreOrderResponse();
      BeanUtils.copyProperties(productLevel3.getPreOrder(), preOrderResponse);
      productLevel3Response.setPreOrder(preOrderResponse);
    }
    productLevel3Response.setFreeSample(productLevel3.isFreeSample());
    return productLevel3Response;
  }

  private ProductLevel3OrderResponse generateProductLevel3OrderResponse(
      ProductLevel3Order productLevel3) throws Exception {
    ProductLevel3OrderResponse productLevel3Response = new ProductLevel3OrderResponse();
    BeanUtils.copyProperties(productLevel3, productLevel3Response, "items");
    productLevel3Response.setItems(new ArrayList<>());
    for (ProductItemLevel3Order productItemLevel3 : productLevel3.getItems()) {
      ProductItemLevel3OrderResponse productItemLevel3Response =
          new ProductItemLevel3OrderResponse();
      BeanUtils.copyProperties(productItemLevel3, productItemLevel3Response, "prices");
      for (ProductLevel3Price productLevel3Price : productItemLevel3.getPrices()) {
        ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
        BeanUtils.copyProperties(productLevel3Price, productLevel3PriceResponse);
        productItemLevel3Response.getPrices().add(productLevel3PriceResponse);
      }
      productLevel3Response.getItems().add(productItemLevel3Response);
    }
    return productLevel3Response;
  }

  private ProductLevel3DashboardResponse generateProductLevel3DashboardResponse(
      ProductLevel3Dashboard productLevel3Dashboard) throws Exception {
    ProductLevel3DashboardResponse productLevel3DashboardResponse =
        new ProductLevel3DashboardResponse();
    BeanUtils.copyProperties(productLevel3Dashboard, productLevel3DashboardResponse);
    return productLevel3DashboardResponse;
  }

  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_DETAIL_ACTIVE_ITEM_BY_GDN_SKU,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 detail for active items at controller",
      description = "filter product level 3 detail for active items at controller")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3Response> filterDetailByGdnSkuForActiveItems(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode, @RequestParam String gdnSku, @RequestParam(required = false, defaultValue = "true") boolean concatValueAndValueTypes) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterDetailByGdnSku", businessPartnerCode, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, gdnSku, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    ProductLevel3Response productLevel3Response = null;
    try {
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
          ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(gdnSku),
          ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY);
      ProductLevel3 productLevel3 = this.productLevel3Service.findDetailByGdnSku(businessPartnerCode, gdnSku, concatValueAndValueTypes);
      productLevel3Response = generateProductLevel3Response(productLevel3);
    } catch (ProductRejectedException e) {
      return new GdnRestSingleResponse(ApiErrorCode.ITEM_IS_REJECTED.getDesc(), ApiErrorCode.ITEM_IS_REJECTED.getCode(),
          false, null, requestId);
    } catch (ProductInNeedCorrectionException e) {
      return new GdnRestSingleResponse<>(ApiErrorCode.ITEM_IS_IN_NEED_CORRECTION.getDesc(),
          ApiErrorCode.ITEM_IS_IN_NEED_CORRECTION.getCode(), false, null, requestId);
    } catch (ProductInReviewException e) {
      return new GdnRestSingleResponse<>(ApiErrorCode.ITEM_IS_IN_REVIEW.getDesc(),
          ApiErrorCode.ITEM_IS_IN_REVIEW.getCode(), false, null, requestId);
    } catch (ApplicationRuntimeException e) {
      return getResponseBasedOnErrorMessage(requestId, e);
    } catch (Exception e) {
      log.error("Error occurred while fetching L3 detail. gdnSku : {} , error - ", gdnSku, e);
      return new GdnRestSingleResponse(e.getMessage(), null, false, null, requestId);
    }
    return new GdnRestSingleResponse<>(productLevel3Response, requestId);
  }

  private static GdnRestSingleResponse getResponseBasedOnErrorMessage(String requestId, ApplicationRuntimeException e) {
    if (e.getErrorMessage().contains(com.gdn.x.product.rest.web.model.enums.ApiErrorCode.ITEM_NOT_FOUND.getCode())) {
      return new GdnRestSingleResponse(com.gdn.x.product.rest.web.model.enums.ApiErrorCode.ITEM_NOT_FOUND.getDesc(),
          com.gdn.x.product.rest.web.model.enums.ApiErrorCode.ITEM_NOT_FOUND.getCode(), false, null, requestId);
    } else if (e.getErrorMessage()
        .contains(com.gdn.x.product.rest.web.model.enums.ApiErrorCode.ITEM_IS_SUSPENDED.getCode())) {
      return new GdnRestSingleResponse(com.gdn.x.product.rest.web.model.enums.ApiErrorCode.ITEM_IS_SUSPENDED.getDesc(),
          com.gdn.x.product.rest.web.model.enums.ApiErrorCode.ITEM_IS_SUSPENDED.getCode(), false, null, requestId);
    } else {
      return new GdnRestSingleResponse(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_DETAIL_FOR_ALL_ITEM_GDN_SKU,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 detail for all items at controller",
      description = "filter product level 3 detail for all items at controller")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3Response> filterDetailByGdnSkuForAllItems(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode, @RequestParam String gdnSku) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterDetailByGdnSku", businessPartnerCode,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, gdnSku, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(gdnSku),
        ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY);
    ProductLevel3 productLevel3 =
        this.productLevel3Service.findDetailByGdnSkuForAllItemSkus(businessPartnerCode, gdnSku);
    LOGGER.info("Price reced in response from service level: {}", productLevel3.getItems().get(0).getPrices());
    return new GdnRestSingleResponse<>(generateProductLevel3Response(productLevel3), requestId);
  }

  private void setMandatoryParameters(String storeId, String channelId, String clientId, String requestId,
      String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.IS_PRISTINE_CATEGORY + "/{categoryId}", method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "API to check whether the category is pristine category", description = "API to check whether the category is pristine category")
  @ResponseBody
  public GdnRestSimpleResponse<Boolean> isPristineCategory(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("categoryId") String categoryId) throws Exception {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "isPristineCategory", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.PRODUCT_LV3_PRISTINE_CATEGORY, categoryId, categoryId);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(categoryId),
        ProductLevel3ControllerErrorMessage.CATEGORY_ID_MUST_NOT_BE_NULL);
    return new GdnRestSimpleResponse<>(requestId, productLevel3Service.isPristineCategory(categoryId));
  }

  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_DETAIL_ORDER_GDN_SKU,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 detail at controller",
      description = "filter product level 3 detail at controller")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3OrderResponse> filterDetailOrderByGdnSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode, @RequestParam String gdnSku) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "filterDetailOrderByGdnSku", businessPartnerCode, username, requestId, storeId,
            channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, gdnSku, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(gdnSku),
        ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY);
    ProductLevel3Order productLevel3 =
        this.productLevel3Service.findDetailOrderByGdnSku(businessPartnerCode, gdnSku);
    ProductLevel3OrderResponse productLevel3Response =
        generateProductLevel3OrderResponse(productLevel3);
    return new GdnRestSingleResponse<ProductLevel3OrderResponse>(productLevel3Response, requestId);
  }


  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_SUMMARY_CATEGORY_BRAND, method = RequestMethod.POST
      , consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "filter product level 3 summary by filter at controller",
      description = "filter product level 3 summary by filter at controller")
  @ResponseBody
  public GdnRestListResponse<ProductLevel3SummaryResponse> filterSummaryByCategoryAndBrand(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam(required = false,
      defaultValue = "createdDate") String orderBy, @RequestParam(required = false,
      defaultValue = "desc") String sortBy, @RequestBody BrandAndCategoryItemSummaryRequest request)
      throws Exception {
    SortOrder sort = new SortOrder(orderBy, sortBy);
    PageRequest pageRequest = PageRequest.of(page, size);
    BrandAndCategoryItemSummaryRequest brandAndCategoryItemSummaryRequest = generateCampaignItemRequest(request);
    try {
      Page<ProductLevel3Summary> productLevel3Summary = this.productLevel3Service
          .findSummaryByCategoryAndBrandFilter(brandAndCategoryItemSummaryRequest, pageRequest, sort);
      List<ProductLevel3SummaryResponse> productLevel3SummaryResponses =
          generateProductLevel3SummaryResponses(productLevel3Summary, false);
      productLevel3SummaryResponses.stream().filter(response -> Objects.isNull(response.getWholesalePriceActivated()))
          .forEach(response -> response.setWholesalePriceActivated(false));
      return new GdnRestListResponse<>(productLevel3SummaryResponses,
          new PageMetaData(pageRequest.getPageSize(), pageRequest.getPageNumber(),
              productLevel3Summary.getTotalElements()), requestId);
    } catch (Exception e) {
      LOGGER.error(LoggerStandard.appendLogErrorTemplate(e.getMessage(), e));
      return new GdnRestListResponse<>("Failed to load product level 3 summary", null, false, null,
          null, requestId);
    }
  }

  private BrandAndCategoryItemSummaryRequest generateCampaignItemRequest(
      BrandAndCategoryItemSummaryRequest request) {
    BrandAndCategoryItemSummaryRequest brandAndCategoryItemSummaryRequest =
        new BrandAndCategoryItemSummaryRequest();
    brandAndCategoryItemSummaryRequest.setMerchantCode(request.getMerchantCode());
    brandAndCategoryItemSummaryRequest.setCategories(request.getCategories());
    brandAndCategoryItemSummaryRequest.setBrands(request.getBrands());
    brandAndCategoryItemSummaryRequest.setKeyword(request.getKeyword());
    brandAndCategoryItemSummaryRequest.setItemSku(request.getItemSku());
    return brandAndCategoryItemSummaryRequest;
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_ITEM_PRICE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item price", description = "update item price")
  @ResponseBody
  public GdnBaseRestResponse updateItemPrice(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String clientHost, @RequestParam String itemSku,
      @RequestBody ProductPriceAndWholesaleRequest request) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateItemPrice", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_UPDATE, null, request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions
        .checkArgument(StringUtils.isNotEmpty(itemSku), ProductLevel3ControllerErrorMessage.ITEM_SKU_MUST_NOT_BE_BLANK);
    try {
      ApiErrorCode apiErrorCode = this.productLevel3Service.updateItemPrice(storeId, request, itemSku);
      return new GdnBaseRestResponse(Objects.nonNull(apiErrorCode) ? apiErrorCode.getDesc() : null,
          Objects.nonNull(apiErrorCode) ? apiErrorCode.getCode() : null, true, requestId);
    } catch (ApiIncorrectInputDataException e){
      return new GdnBaseRestResponse(e.getMessage(), e.getErrorCode().getCode(), false, requestId);
    } catch (Exception e) {
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_ITEM_VIEW_CONFIG, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item view configuration", description = "update item view configuration")
  @ResponseBody
  public GdnBaseRestResponse updateItemViewConfig(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String clientHost, @RequestParam String itemSku,
      @RequestBody ItemViewConfigRequest request) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateItemViewConfig", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_UPDATE, null, request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(itemSku)),
        ProductLevel3ControllerErrorMessage.ITEM_SKU_MUST_NOT_BE_BLANK);
    this.productLevel3Service.updateItemViewConfig(request, itemSku, StringUtils.EMPTY);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_ITEM_OFF_2_ON, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item off 2 on", description = "update item off 2 on")
  @ResponseBody
  public GdnBaseRestResponse updateItemOff2On(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String clientHost, @RequestParam String itemSku,
      @RequestParam boolean off2OnActiveFlag) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateItemOff2On", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_UPDATE, 
        itemSku + ":" + String.valueOf(off2OnActiveFlag), null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(itemSku)),
        ProductLevel3ControllerErrorMessage.ITEM_SKU_MUST_NOT_BE_BLANK);
    this.productLevel3Service.updateItemOff2On(off2OnActiveFlag, itemSku);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.SYNCHRONIZE_PRODUCT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "synchronize product", description = "synchronize product")
  @ResponseBody
  public GdnBaseRestResponse synchronizeProduct(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String clientHost, @RequestParam String productSku, @RequestParam(required = false) String itemSku)
      throws Exception {
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(productSku)),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    try {
      ApiErrorCode apiErrorCode = this.productLevel3Service.synchronizeProduct(productSku, itemSku);
      if (Objects.nonNull(apiErrorCode)) {
        LOGGER.error("Error while synchronize the productSku : {} and error code : {}", productSku,
            apiErrorCode.getDesc());
        return new GdnBaseRestResponse(apiErrorCode.getDesc(), apiErrorCode.getCode(), false, requestId);
      } else {
        return new GdnBaseRestResponse(null, null, true, requestId);
      }
    } catch (Exception e) {
      LOGGER.error("Error while synchronize the productSku : {} ", productSku, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }

  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UNSYNCHRONIZE_PRODUCT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "unsynchronize product", description = "unsynchronize product") @ResponseBody
  public GdnBaseRestResponse unsynchronizeProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String clientHost,
      @RequestParam String productSku, @RequestParam(required = false) String itemSku)
      throws Exception {
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(productSku)),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    try {
      ApiErrorCode apiErrorCode = this.productLevel3Service.unsynchronizeProduct(productSku, itemSku);
      if (Objects.nonNull(apiErrorCode)) {
        LOGGER.error("Error while unsynchronize the productSku : {} and error code : {}", productSku,
            apiErrorCode.getDesc());
        return new GdnBaseRestResponse(apiErrorCode.getDesc(), apiErrorCode.getCode(), false, requestId);
      } else {
        return new GdnBaseRestResponse(null, null, true, requestId);
      }
    } catch (Exception e) {
      LOGGER.error("Error while unsynchronize the productSku : {} ", productSku, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_ITEM_STOCK, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item stock at controller",
      description = "update item stock at controller")
  @ResponseBody
  public GdnBaseRestResponse updateItemStock(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String clientHost, @RequestParam String businessPartnerCode,
      @RequestBody ProductLevel3StockRequest request) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateItemStock", businessPartnerCode, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_UPDATE_SYNC, 
        request.getGdnSku(), request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getGdnSku()),
        ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(request.getDeltaStock() != null,
        ProductLevel3ControllerErrorMessage.DELTA_STOCK_MUST_NOT_BE_EMPTY);
    this.productLevel3Service.updateItemStock(businessPartnerCode, request.getGdnSku(),
        request.getDeltaStock(), request.getMinimumStock());
    return new GdnBaseRestResponse(requestId);
  }

  private void validateUpdateProduct(ProductLevel3Request product, String storeId) throws Exception {
    GdnPreconditions.checkArgument(!product.getItems().isEmpty(),
        ProductLevel3ControllerErrorMessage.ITEM_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!product.getItems().get(0).getViewConfigs().isEmpty(),
        ProductLevel3ControllerErrorMessage.VIEW_CONFIG_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(
        !StringUtils.isEmpty(product.getItems().get(0).getPickupPointCode()),
        ProductLevel3ControllerErrorMessage.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(
        !StringUtils.isEmpty(product.getItems().get(0).getPickupPointName()),
        ProductLevel3ControllerErrorMessage.PICKUP_POINT_NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getProductSku()),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    if (CollectionUtils.isNotEmpty(product.getItems().get(0).getPrices())) {
      GdnPreconditions.checkArgument(product.getItems().get(0).getPrices().get(0).getSalePrice() != null,
          ProductLevel3ControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_BLANK);
      Integer minimumPrice = productLevel3Service.getMinimumPrice(storeId);
      GdnPreconditions.checkArgument(product.getItems().get(0).getPrices().get(0).getPrice() >= minimumPrice,
          ProductLevel3ControllerErrorMessage.PRICE_MINIMUM_VALUE_INVALID + minimumPrice);
      GdnPreconditions.checkArgument(product.getItems().get(0).getPrices().get(0).getSalePrice() >= minimumPrice,
          ProductLevel3ControllerErrorMessage.SALE_PRICE_MINIMUM_VALUE_INVALID + minimumPrice);
      GdnPreconditions.checkArgument(
          product.getItems().get(0).getPrices().get(0).getSalePrice() <= product.getItems().get(0).getPrices().get(0)
              .getPrice(), ProductLevel3ControllerErrorMessage.SALE_PRICE_VALUE_INVALID);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getItems().get(0).getPrices().get(0).getChannelId()),
          ProductLevel3ControllerErrorMessage.CHANNELID_MUST_NOT_BE_BLANK);
    }
    if (Objects.nonNull(product.getUniqueSellingPoint())) {
      String uspWithoutTags = ControllerUtils.getFilteredUSPAndDescription(product.getUniqueSellingPoint());
      GdnPreconditions
          .checkArgument(uspWithoutTags.length() <= ProductControllerErrorMessage.MAXIMUM_UNIQUE_SELLING_POINT_LENGTH,
              ProductControllerErrorMessage.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    }
    GdnPreconditions.checkArgument(
        product.getItems().get(0).getViewConfigs().get(0).getBuyable() != null,
        ProductLevel3ControllerErrorMessage.BUYABLE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(
        product.getItems().get(0).getViewConfigs().get(0).getDisplay() != null,
        ProductLevel3ControllerErrorMessage.DISPLAY_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(product.getProductType() != null,
        ProductLevel3ControllerErrorMessage.PRODUCT_TYPE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getBusinessPartnerCode()),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getItems().get(0).getItemSku()),
        ProductLevel3ControllerErrorMessage.ITEM_SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(product.getItems().get(0).getShippingWeight() != null,
        ProductLevel3ControllerErrorMessage.SHIPPING_WEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(
        !StringUtils.isEmpty(product.getItems().get(0).getViewConfigs().get(0).getChannelId()),
        ProductLevel3ControllerErrorMessage.CHANNELID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(product.getItems().get(0).getLateFulfillment() != null,
        ProductLevel3ControllerErrorMessage.LATE_FULFILLMENT_MUST_NOT_BE_BLANK);
    if (!product.getSynchronize()) {
      GdnPreconditions.checkArgument(!product.getImages().isEmpty(),
          ProductLevel3ControllerErrorMessage.IMAGE_MUST_NOT_BE_EMPTY);
      GdnPreconditions.checkArgument(!product.getAttributes().isEmpty(),
          ProductLevel3ControllerErrorMessage.ATTRIBUTE_MUST_NOT_BE_EMPTY);
      GdnPreconditions.checkArgument(!product.getItems().get(0).getImages().isEmpty(),
          ProductLevel3ControllerErrorMessage.ITEM_IMAGE_MUST_NOT_BE_EMPTY);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getSpecificationDetail()),
          ProductLevel3ControllerErrorMessage.SPECIFICATION_DETAIL_MUST_NOT_BE_BLANK);
      for (ProductLevel3ImageRequest image : product.getImages()) {
        GdnPreconditions.checkArgument(!StringUtils.isEmpty(image.getLocationPath()),
            ProductLevel3ControllerErrorMessage.LOCATION_PATH_MUST_NOT_BE_BLANK);
        GdnPreconditions.checkArgument(image.getSequence() != null,
            ProductLevel3ControllerErrorMessage.SEQUENCE_MUST_NOT_BE_BLANK);
        GdnPreconditions.checkArgument(image.getMainImage() != null,
            ProductLevel3ControllerErrorMessage.MAIN_IMAGE_MUST_NOT_BE_BLANK);
      }
      GdnPreconditions.checkArgument(
          !StringUtils.isEmpty(product.getAttributes().get(0).getAttributeCode()),
          ProductLevel3ControllerErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(
          !StringUtils.isEmpty(product.getAttributes().get(0).getAttributeType()),
          ProductLevel3ControllerErrorMessage.ATTRIBUTE_TYPE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(!product.getAttributes().get(0).getValues().isEmpty(),
          ProductLevel3ControllerErrorMessage.ATTRIBUTE_VALUE_MUST_NOT_BE_EMPTY);
      GdnPreconditions.checkArgument(product.getAttributes().get(0).getSkuValue() != null,
          ProductLevel3ControllerErrorMessage.ATTRIBUTE_SKU_VALUE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getItems().get(0).getItemName()),
          ProductLevel3ControllerErrorMessage.ITEM_NAME_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(product.getItems().get(0).getDangerousGoodsLevel() != null,
          ProductLevel3ControllerErrorMessage.DANGEROUS_GOODS_LEVEL_MUST_NOT_BE_BLANK);
      for (ProductLevel3ImageRequest imageItem : product.getItems().get(0).getImages()) {
        GdnPreconditions.checkArgument(!StringUtils.isEmpty(imageItem.getLocationPath()),
            ProductLevel3ControllerErrorMessage.LOCATION_PATH_MUST_NOT_BE_BLANK);
        GdnPreconditions.checkArgument(imageItem.getSequence() != null,
            ProductLevel3ControllerErrorMessage.SEQUENCE_MUST_NOT_BE_BLANK);
        GdnPreconditions.checkArgument(imageItem.getMainImage() != null,
            ProductLevel3ControllerErrorMessage.MAIN_IMAGE_MUST_NOT_BE_BLANK);
      }
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getProductName()),
          ProductLevel3ControllerErrorMessage.PRODUCT_NAME_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(product.getProductName().length() <= PRODUCT_NAME_LIMIT,
          ProductLevel3ControllerErrorMessage.PRODUCT_NAME_LTE_LIMIT);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getBrand()),
          ProductLevel3ControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getDescription()),
          ProductLevel3ControllerErrorMessage.DESCRIPTION_MUST_NOT_BE_BLANK);
      String descriptionWithoutTags = ControllerUtils.getFilteredUSPAndDescription(product.getDescription());
      GdnPreconditions.checkArgument(descriptionWithoutTags.length() <= maximumCharactersInDescription, String
          .format(ProductLevel3ControllerErrorMessage.DESCRIPTION_MUST_NOT_BE_MORE_THAN_MAX_CHARACTERS,
              maximumCharactersInDescription));
      GdnPreconditions.checkArgument(product.getDescription().length() <= maximumCharactersWithoutFormattingDescription, String
          .format(ProductLevel3ControllerErrorMessage.CHARACTER_LIMIT_REACHED_PLEASE_REDUCE_FORMATTING,
              maximumCharactersWithoutFormattingDescription));
      GdnPreconditions.checkArgument(product.getItems().get(0).getLength() != null,
          ProductLevel3ControllerErrorMessage.ITEM_LENGTH_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(product.getItems().get(0).getWidth() != null,
          ProductLevel3ControllerErrorMessage.ITEM_WIDTH_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(product.getItems().get(0).getHeight() != null,
          ProductLevel3ControllerErrorMessage.ITEM_HEIGHT_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(product.getItems().get(0).getWeight() != null,
          ProductLevel3ControllerErrorMessage.ITEM_WEIGHT_MUST_NOT_BE_BLANK);

      ProductSystemParameter productSystemParameter = productSystemParameterService
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
      String[] apiKeys = youTubeDataApiKey.split(",");
      int apiKeyIndex = (int) (Math.random() * apiKeys.length);
      String youTubeApiKey = apiKeys[apiKeyIndex];
      LOGGER.debug("Using youtube api key index: {} for url: {}", apiKeyIndex, product.getUrl());
      boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(product.getUrl(), youTubeApiKey, youTube, Boolean.valueOf(productSystemParameter.getValue()),
          youtubeRegex);
      if (!youTubeUrlResponse) {
        product.setUrl(StringUtils.EMPTY);
      }
    }
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_RETURN, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product content at controller",
      description = "update product content at controller")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3Response> updateAndReturn(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String clientHost,
      @RequestParam(required = false, defaultValue = "true") boolean isOnlyExternal,
      @RequestParam(required = false, defaultValue = "false") boolean hasOrder,
      @RequestParam(required = false, defaultValue = "false") boolean updateLogistics,
      @RequestBody ProductLevel3Request product) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateAndReturn", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_UPDATE, 
        product.getProductCode(), product.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    validateUpdateProduct(product, storeId);
    ProductLevel3 productLevel3 = generateProductLevel3(product);
    try {
      ProductLevel3DTO result = this.productLevel3Service
          .update(productLevel3, product.getItems().get(0).getDeltaStock(), product.getItems().get(0).getMinimumStock(),
              isOnlyExternal, hasOrder, updateLogistics);
      if (Objects.nonNull(result.getApiErrorCode())) {
        return new GdnRestSingleResponse<>(result.getApiErrorCode().getDesc(), result.getApiErrorCode().getCode(),
            false, new ProductLevel3Response(), requestId);
      } else {
        return new GdnRestSingleResponse<>(null, null, true, new ProductLevel3Response(), requestId);
      }
    } catch (ApplicationRuntimeException e) {
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false, null, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.PRODUCT_EDIT_INFO, method = RequestMethod.POST, consumes =
      MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product edit info content at controller",
      description = "update product edit info content at controller")
  @ResponseBody
  public GdnRestSingleResponse<EditProductV2Response> editProductInfo(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productSku") String productSku,
      @RequestParam(required = false, defaultValue = "true") boolean isOnlyExternal,
      @RequestBody ProductLevel3Request product) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "editProductInfo", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.PRODUCT_LV3_UPDATE, product.getProductCode(), product.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);

    validateProductEditInfo(product);
    ProductLevel3 productLevel3 = generateProductLevel3(product);
    ValidationUtil.validateDescriptiveFieldsForProductUpdate(productLevel3);
    try {
      //Have to get updatedBy for auto approval service
      productLevel3.setUpdatedBy(username);
      ProfileResponse profileResponse =
          businessPartnerRepository.filterDetailByBusinessPartnerCode(product.getBusinessPartnerCode());
      ProductEditValidationDTO productEditValidationDTO =
          productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(productLevel3, new EditProductResponse(),
              profileResponse, false, null, new ArrayList<>(), new ProductL3UpdateRequest(), false, false);
      ProductL3Response productL3Response = new ProductL3Response();
      if (Objects.nonNull(productEditValidationDTO)) {
        productL3Response = productEditValidationDTO.getProductL3Response();
        if (Objects.nonNull(productEditValidationDTO.getEditProductResponse().getApiErrorCode())) {
          LOGGER.error("Error while updating the edit info for productSku : {} and error code : {}", productSku,
              productEditValidationDTO.getEditProductResponse());
          return new GdnRestSingleResponse(
              productEditValidationDTO.getEditProductResponse().getApiErrorCode().getDesc(),
              productEditValidationDTO.getEditProductResponse().getApiErrorCode().getCode(), false,
              productEditValidationDTO.getEditProductResponse(), requestId);
        }
      }
      EditProductResponse editProductResponse = this.productLevel3Service.updateEditInfo(productLevel3,
        isOnlyExternal, true, false, profileResponse, isOnlyExternal,productL3Response, false, null, null);
      EditProductV2Response result = ResponseHelper.toEditProductV2Response(editProductResponse);
      if (Objects.nonNull(result.getApiErrorCode())) {
        LOGGER.error("Error while updating the edit info for productSku : {} and error code : {}",
            productSku, result);
        return new GdnRestSingleResponse(result.getApiErrorCode().getDesc(), result.getApiErrorCode().getCode(), false,
            result, requestId);
      } else {
        return new GdnRestSingleResponse<>(null, null, true, result, requestId);
      }
    } catch (ApplicationException e) {
      LOGGER.error("error updating product edit info. productSku: {} ", productSku, e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false,
          null, requestId);
    } catch (Exception e) {
      LOGGER.error("error updating product edit info. productSku: {} ", productSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product content at controller",
      description = "update product content at controller")
  @ResponseBody
  public GdnBaseRestResponse update(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String clientHost, @RequestBody ProductLevel3Request product) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "update", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_UPDATE, 
        product.getProductCode(), product.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    validateUpdateProduct(product, storeId);
    ProductLevel3 productLevel3 = generateProductLevel3(product);
    this.productLevel3Service.update(productLevel3, product.getItems().get(0).getDeltaStock(),
        product.getItems().get(0).getMinimumStock(), false);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  private void validateUpdateSummary(ProductLevel3UpdateSummaryRequest request, String storeId) throws Exception {
    for (ProductLevel3PriceRequest priceRequest : request.getPrices()) {
      GdnPreconditions.checkArgument(priceRequest.getPrice() != null,
          ProductLevel3ControllerErrorMessage.PRICE_MUST_NOT_BE_EMPTY);
      GdnPreconditions.checkArgument(priceRequest.getSalePrice() != null,
          ProductLevel3ControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_EMPTY);
      Integer minimumPrice = productLevel3Service.getMinimumPrice(storeId);
      GdnPreconditions.checkArgument(priceRequest.getPrice() >= minimumPrice,
          ProductLevel3ControllerErrorMessage.PRICE_MINIMUM_VALUE_INVALID + minimumPrice);
      GdnPreconditions.checkArgument(priceRequest.getSalePrice() >= minimumPrice,
          ProductLevel3ControllerErrorMessage.SALE_PRICE_MINIMUM_VALUE_INVALID + minimumPrice);
      GdnPreconditions.checkArgument(priceRequest.getSalePrice() <= priceRequest.getPrice(),
          ProductLevel3ControllerErrorMessage.SALE_PRICE_VALUE_INVALID);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(priceRequest.getChannelId()),
          ProductLevel3ControllerErrorMessage.CHANNEL_ID_MUST_NOT_BE_EMPTY);
    }
    for (ProductLevel3ViewConfigRequest viewConfigRequest : request.getViewConfigs()) {
      GdnPreconditions.checkArgument(viewConfigRequest.getDisplay() != null,
          ProductLevel3ControllerErrorMessage.DISPLAY_MUST_NOT_BE_EMPTY);
      GdnPreconditions.checkArgument(viewConfigRequest.getBuyable() != null,
          ProductLevel3ControllerErrorMessage.BUYABLE_MUST_NOT_BE_EMPTY);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(viewConfigRequest.getChannelId()),
          ProductLevel3ControllerErrorMessage.CHANNEL_ID_MUST_NOT_BE_EMPTY);
    }
  }

  private ProductLevel3UpdateSummary generateProductLevel3UpdateSummary(
      ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest) throws Exception {
    ProductLevel3UpdateSummary productLevel3UpdateSummary = new ProductLevel3UpdateSummary();
    BeanUtils.copyProperties(productLevel3UpdateSummaryRequest, productLevel3UpdateSummary, "prices",
        "viewConfigs");
    productLevel3UpdateSummary.setPrices(new ArrayList<>());
    productLevel3UpdateSummary.setViewConfigs(new ArrayList<>());
    for (ProductLevel3PriceRequest priceRequest : productLevel3UpdateSummaryRequest.getPrices()) {
      ProductLevel3Price price = new ProductLevel3Price();
      BeanUtils.copyProperties(priceRequest, price);
      productLevel3UpdateSummary.getPrices().add(price);
    }
    for (ProductLevel3ViewConfigRequest viewConfigRequest : productLevel3UpdateSummaryRequest
        .getViewConfigs()) {
      ProductLevel3ViewConfig viewConfig = new ProductLevel3ViewConfig();
      BeanUtils.copyProperties(viewConfigRequest, viewConfig);
      productLevel3UpdateSummary.getViewConfigs().add(viewConfig);
    }
    return productLevel3UpdateSummary;
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.  UPDATE_SUMMARY, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update summary at controller", description = "update summary at controller")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3SummaryResponse> updateSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode, @RequestParam String gdnSku,
      @RequestParam String clientHost, @RequestBody ProductLevel3UpdateSummaryRequest request)
      throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateSummary", businessPartnerCode, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_UPDATE, 
        null, request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(gdnSku),
        ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY);
    validateUpdateSummary(request, storeId);
    ProductLevel3UpdateSummary productLevel3UpdateSummary =
        generateProductLevel3UpdateSummary(request);
    ProductLevel3Summary productLevel3Summary = new ProductLevel3Summary();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = null;
    try {
      productLevel3Summary =
          this.productLevel3Service.updateSummary(businessPartnerCode, gdnSku,
              productLevel3UpdateSummary);
      productLevel3SummaryResponse = generateProductLevel3SummaryResponse(productLevel3Summary);
    } catch (InvalidDataStateException ex) {
      LOGGER.error("Can't update archived product :Tidak berhasil update  product yang telah diarsipkan , request : {}",
          request);
      return new GdnRestSingleResponse<ProductLevel3SummaryResponse>(ex.getMessage(),
          ErrorCategory.INVALID_STATE.getCode(), false, null, requestId);
    } catch (ApplicationRuntimeException ex) {
      LOGGER.error("error updating active product info. businessPartnerCode: {}, gdnSku: {}",
          businessPartnerCode, gdnSku, ex);
      return new GdnRestSingleResponse<>(ex.getMessage(),
          ErrorCategory.VALIDATION.getCode(), false, null, requestId);
    } catch (Exception e) {
      LOGGER.error("error updating active product info. businessPartnerCode: {}, gdnSku: {}",
          businessPartnerCode, gdnSku, e);
      return new GdnRestSingleResponse<ProductLevel3SummaryResponse>(e.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse<ProductLevel3SummaryResponse>(productLevel3SummaryResponse,
        requestId);
    if (Objects.nonNull(productLevel3Summary.getWholesalePriceActivated()) && Boolean.TRUE.equals(request.getWholesalePriceActivated())) {
      gdnRestSingleResponse.setErrorCode(Boolean.FALSE.equals(productLevel3Summary.getWholesalePriceActivated()) ?
          ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE.getCode() :
          ApiErrorCode.SAME_THRESHOLD_ERROR_CODE.getCode());
      gdnRestSingleResponse.setErrorMessage(Boolean.FALSE.equals(productLevel3Summary.getWholesalePriceActivated()) ?
          ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE.getDesc() :
          ApiErrorCode.SAME_THRESHOLD_ERROR_CODE.getDesc());
    } else {
      gdnRestSingleResponse.setErrorCode(null);
      gdnRestSingleResponse.setErrorMessage(null);
    }
    return gdnRestSingleResponse;
  }

  @RequestMapping(value = ProductLevel3ControllerPath.ITEM_LISTING_UPDATE, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update l4 listing at controller", description = "update l4 listing")
  @ResponseBody
  public GdnBaseRestResponse itemListingUpdate(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username, @PathVariable("productSku") String productSku,
      @RequestBody ProductLevel3QuickEditRequest request) throws Exception {
    LOGGER.info("l4 listing update request: {} ", request);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      List<String> stockUpdateFailedItemSkus = new ArrayList<>();
      ApiErrorCode apiErrorCode = this.productLevel3Service.productQuickEdit(storeId, productSku, request, stockUpdateFailedItemSkus);
      if (!stockUpdateFailedItemSkus.isEmpty()) {
        return new GdnBaseRestResponse(String.format(ErrorMessages.ERROR_STOCK_UPDATE_FAILED,
            StringUtils.join(stockUpdateFailedItemSkus, Constants.COMMA)),
            ApiErrorCode.MAXIMUM_STOCK_LIMIT_EXCEEDED.getCode(), false, requestId);
      }
      if (Objects.nonNull(apiErrorCode)) {
        return new GdnBaseRestResponse(apiErrorCode.getDesc(), apiErrorCode.getCode(), false, requestId);
      } else {
        return new GdnBaseRestResponse(null, null, true, requestId);
      }
    } catch (Exception e) {
      LOGGER.error("error updating active product info. {} ", e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_SUMMARY_SINGLE,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 summary single at controller",
      description = "filter product level 3 summary single at controller")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3SummaryResponse> filterSummaryByGdnSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode, @RequestParam String gdnSku) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterSummaryByGdnSku", businessPartnerCode, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, 
        gdnSku, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(gdnSku),
        ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY);
    ProductLevel3Summary productLevel3Summary =
        this.productLevel3Service.findSummaryByGdnSku(businessPartnerCode, gdnSku);
    ProductLevel3SummaryResponse productLevel3SummaryResponse =
        generateProductLevel3SummaryResponse(productLevel3Summary);
    return new GdnRestSingleResponse<ProductLevel3SummaryResponse>(productLevel3SummaryResponse,
        requestId);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_SUMMARY_GDN_SKU_LIST, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 summary  at controller by gdnsku list",
      description = "filter product level 3 summary  at controller by gdnsku list")
  @ResponseBody
  public GdnRestListResponse<ProductLevel3SummaryResponse> filterSummaryByGdnSkuList(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false) String businessPartnerCode, @RequestBody ItemSkuListRequest request,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterSummaryByGdnSkuList", businessPartnerCode, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, 
        page + ":" + size, request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(Objects.nonNull(request) && CollectionUtils.isNotEmpty(request.getItemSkuList()),
        ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_EMPTY);
    PageRequest pageable = PageRequest.of( page, size);
    Page<ProductLevel3Summary> productLevel3SummaryPage =
        this.productLevel3Service.findSummaryByGdnSkuList(businessPartnerCode,
            request.getItemSkuList(), pageable);
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses =
        generateProductLevel3SummaryResponses(productLevel3SummaryPage, false);
    return new GdnRestListResponse<>(productLevel3SummaryResponses, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(),
        productLevel3SummaryPage.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_DASHBOARD, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 dashboard at controller",
      description = "filter product level 3 dashboard at controller")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3DashboardResponse> filterDashboard(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterDashboard", businessPartnerCode, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, 
        null, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    ProductLevel3Dashboard productLevel3Dashboard =
        this.productLevel3Service.findDashboard(businessPartnerCode);
    ProductLevel3DashboardResponse productLevel3DashboardResponse =
        generateProductLevel3DashboardResponse(productLevel3Dashboard);
    return new GdnRestSingleResponse<ProductLevel3DashboardResponse>(
        productLevel3DashboardResponse, requestId);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.CHECK_PICKUP_POINT_USED,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "check if pickup point code is used on item",
      description = "check if pickup point code is used on item")
  @ResponseBody
  public GdnRestSingleResponse<UsageResponse> checkPickupPointCodeUsed(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String pickupPointCode) throws Exception {
    UsageResponse usageResponse = new UsageResponse();
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "checkPickupPointCodeUsed", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, 
        pickupPointCode, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(pickupPointCode),
        ProductLevel3ControllerErrorMessage.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    UsageStatus usageStatus = this.productLevel3Service.checkPickupPointCodeUsed(pickupPointCode);
    usageResponse.setUsed(usageStatus.isUsed());
    return new GdnRestSingleResponse<UsageResponse>(usageResponse, requestId);
  }


  @Deprecated
  @RequestMapping(value = ProductLevel3ControllerPath.BULK_DOWNLOAD_SUMMARY, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 summary for bulk download by filter at "
      + "controller", description = "filter product level 3 summary by filter for bulk download "
      + "products at controller")
  @ResponseBody
  public GdnRestSingleResponse<BulkDownloadProductLevel3Response> bulkDownloadSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestBody ProductLevel3SummaryRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "bulkDownloadSummary", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.PRODUCT_LV3_BULK_DOWNLOAD, null, request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    PageRequest pageable = PageRequest.of(page, size);
    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response = new BulkDownloadProductLevel3Response();
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response =
        new GdnRestSingleResponse<>(bulkDownloadProductLevel3Response, requestId);
    try {
      ProductLevel3SummaryFilter filter =
          productLevel3RequestConverter
              .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(request);
      filter.setBusinessPartnerCode(businessPartnerCode);

      BulkDownloadProductLevel3Summary bulkDownloadProductLevel3Summary = this.productLevel3Service
          .findProductSummaryForBulkDownload(businessPartnerCode, pageable, requestId, filter);
      List<ProductLevel3SummaryResponse> productLevel3SummaryResponses =
          generateProductLevel3SummaryResponses(bulkDownloadProductLevel3Summary.getProductLevel3Summary(), true);
      bulkDownloadProductLevel3Response.setExceptionMap(bulkDownloadProductLevel3Summary.getExceptionMap());
      bulkDownloadProductLevel3Response.setProductLevel3SummaryResponses(productLevel3SummaryResponses);
      bulkDownloadProductLevel3Response.setPageMetaData(new PageMetaData(size, page,
          bulkDownloadProductLevel3Summary.getProductLevel3Summary().getTotalElements()));
      response.setValue(bulkDownloadProductLevel3Response);
    } catch (Exception e) {
      LOGGER.error(LoggerStandard.appendLogErrorTemplate(e.getMessage(), e));
      response.setSuccess(false);
    }
    return response;
  }

  @RequestMapping(value = ProductLevel3ControllerPath.BULK_DOWNLOAD_SUMMARY_FROM_DB, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 summary for bulk download by filter at " + "controller", description =
      "filter product level 3 summary by filter for bulk download " + "products at controller")
  @ResponseBody
  public GdnRestSingleResponse<BulkDownloadProductLevel3Response> bulkDownloadSummaryFromDb(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode,
      @RequestParam(required = false, defaultValue = "false") boolean fetchB2bData,
      @RequestParam(required = false) String fetchViewConfigByChannel,
      @RequestBody ProductLevel3SummaryRequest request) throws Exception {

    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response = new BulkDownloadProductLevel3Response();
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response =
        new GdnRestSingleResponse<>(bulkDownloadProductLevel3Response, requestId);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      ProductLevel3SummaryFilter filter =
          productLevel3RequestConverter.convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(request);
      filter.setBusinessPartnerCode(businessPartnerCode);
      bulkDownloadProductLevel3Response =
          this.productLevel3Service.findProductSummaryForBulkDownloadByDb(filter, fetchB2bData,
              fetchViewConfigByChannel);
      response.setValue(bulkDownloadProductLevel3Response);
    } catch (Exception e) {
      LOGGER.error(LoggerStandard.appendLogErrorTemplate(e.getMessage(), e));
      response.setSuccess(false);
    }
    return response;
  }

  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_SUMMARY_BULK_DOWNLOAD, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 summary for bulk download by filter at controller",
      description = "filter product level 3 summary by filter for bulk download products at controller")
  @ResponseBody
  public GdnRestListResponse<ProductLevel3SummaryResponse> filterSummaryForBulkDownload(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterSummaryForBulkDownload", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, 
        page + ":" + size, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    PageRequest pageable = PageRequest.of(page, size);
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    try {
      BulkDownloadProductLevel3Summary bulkDownloadProductLevel3Summary =
          this.productLevel3Service
              .findProductSummaryForBulkDownload(businessPartnerCode, pageable, requestId, null,
                  null, null, null, null, null, false);
      productLevel3SummaryResponses = generateProductLevel3SummaryResponses(
          bulkDownloadProductLevel3Summary.getProductLevel3Summary(), true);
    } catch (Exception e) {
      LOGGER.error(LoggerStandard.appendLogErrorTemplate(e.getMessage(), e));
      throw e;
    }
    return new GdnRestListResponse<ProductLevel3SummaryResponse>(productLevel3SummaryResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productLevel3SummaryResponses.size()), requestId);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.COUNTS_SUMMARY + "/{businessPartnerCode}",
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(
      summary = "filter product level 3 stock condition count at controller",
      description = "get product count for every stock condition : AVAILABLE, OOS, STOCK_ALERT")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3SummaryCountResponse> filterProductStockConditionCount(
      @PathVariable("businessPartnerCode") String businessPartnerCode,
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductStockConditionCount", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, 
        null, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    ProductLevel3SummaryCount productStockConditionCount  = this.productLevel3AggregatorService.countSummary(businessPartnerCode);
    
    ProductLevel3SummaryCountResponse responseContent = new ProductLevel3SummaryCountResponse();
    responseContent.setStoreId(storeId);
    responseContent.setBusinessPartnerCode(productStockConditionCount.getBusinessPartnerCode());
    responseContent.setStockConditionCounts(productStockConditionCount.getStockConditionCounts());
    responseContent.setTotalCounts(productStockConditionCount.getTotalCounts());
    return new GdnRestSingleResponse<>(responseContent, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.COUNT_SUMMARY_FOR_INACTIVE_PRODUCTS, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "count summary state", description = "count summary with state")
  @ResponseBody
  public GdnRestSingleResponse<CountProductLevel3InactiveResponse> countSummaryForInactiveProducts(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String businessPartnerCode)
      throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    LOGGER.info("Fetching count summary for inactive products for business partner {}", businessPartnerCode);
    CountProductLevel3InactiveResponse countProductLevel3InactiveResponse =
        this.productLevel3Service.countSummaryForInactiveProduct(storeId, requestId, username, businessPartnerCode);
    return new GdnRestSingleResponse<>(countProductLevel3InactiveResponse, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.ARCHIVE_ITEM,
                  method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "toggle archive item", description = "toggle archive item")
  @ResponseBody
  @Trace(dispatcher = true)
  public GdnBaseRestResponse toggleArchiveItem(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String clientHost, @RequestParam String itemSku, @RequestParam boolean doArchive) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "toggleArchiveItem", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.PRODUCT_LV3_ARCHIVE, String.valueOf(doArchive), null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    try {
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(itemSku)), ProductLevel3ControllerErrorMessage.ITEM_SKU_MUST_NOT_BE_BLANK);
    this.productLevel3Service.toggleArchiveItem(itemSku, doArchive);
    return new GdnBaseRestResponse(null, null, true, requestId);
  } catch (Exception e) {
      LOGGER.error("error while archive itemSku: {} ",
          itemSku, e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.BULK_ARCHIVE_OOS_ITEMS, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "toggle archive item", description = "toggle archive item")
  @ResponseBody
  @Trace(dispatcher = true)
  public GdnBaseRestResponse bulkArchiveOosItems(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "100") long bulkArchiveSize,
      @RequestParam(defaultValue = "6") int monthsToArchiveFor,
      @RequestParam(defaultValue = "false") Boolean archiveWithoutSendingMail) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "bulkArchiveOosItems", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.PRODUCT_LV3_ARCHIVE, null, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    this.productLevel3Wrapper.bulkArchiveOldOosProducts(storeId, bulkArchiveSize, monthsToArchiveFor,
        archiveWithoutSendingMail);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_IMAGE, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product image at controller",
      description = "update product image at controller")
  @ResponseBody
  public GdnBaseRestResponse updateImage(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(required = false, defaultValue = "true") boolean isOnlyExternal,
      @RequestBody UpdateImageRequest request) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateImage", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_UPDATE, 
        request.getProductCode(), request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    try {
      // updating Item first
      ItemResponse savedItem = this.productLevel3Service.getItem(request.getItemSku());
      ItemRequest newItem = ControllerUtils.convertItemResponseToItemRequest(savedItem);
      List<MasterDataItemImageDTO> masterDataItemImageDTOs = new ArrayList<>();
      for (ProductLevel3ImageRequest productLevel3ImageRequest : request
          .getMasterDataItemImages()) {
        MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO(
            productLevel3ImageRequest.getMainImage(), productLevel3ImageRequest.getLocationPath(),
            productLevel3ImageRequest.getSequence());
        masterDataItemImageDTOs.add(masterDataItemImageDTO);
      }
      newItem.getMasterDataItem().setMasterDataItemImages(masterDataItemImageDTOs);
      this.productLevel3Service.updateItem(newItem, isOnlyExternal);

      // updating Product now
      ProductAndItemsResponse savedProduct =
          this.productLevel3Service.getProduct(request.getProductSku());
      ProductRequest newProduct = new ProductRequest();
      BeanUtils.copyProperties(savedProduct.getProduct(), newProduct, "isSynchronized",
          "productCatentryId", "masterCatalog", "salesCatalogs", "definingAttributes",
          "descriptiveAttributes", "itemCatalogs", "salesCategorySequences", "off2OnChannelActive");
      List<MasterDataProductImageDTO> masterDataProductImageDTOs = new ArrayList<>();
      for (ProductLevel3ImageRequest productLevel3ImageRequest : request
          .getMasterDataItemImages()) {
        MasterDataProductImageDTO masterDataProductImageDTO = new MasterDataProductImageDTO(
            productLevel3ImageRequest.getMainImage(), productLevel3ImageRequest.getLocationPath(),
            request.getProductCode(), productLevel3ImageRequest.getSequence());
        masterDataProductImageDTOs.add(masterDataProductImageDTO);
      }
      newProduct.getMasterDataProduct().setMasterDataProductImages(masterDataProductImageDTOs);
      this.productLevel3Service.update(newProduct, isOnlyExternal);

    } catch (Exception e) {
      LOGGER.error("error updating product's image. productSku:{}, itemSku:{}",
          request.getProductSku(), request.getItemSku(), e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.COUNT_PRODUCT_BY_BRAND, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "count product by brand", description = "count product by brand")
  @ResponseBody
  public GdnRestSimpleResponse<Long> getProductsCountByBrand(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String brand) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "getProductsCountByBrand", null, username, requestId,
            storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_COUNT_BRAND, brand, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
        LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.info(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(brand),
        ProductLevel3ControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK);

    Long productCount = this.productLevel3Service.getProductsCountByBrand(brand);

    return new GdnRestSimpleResponse<>(null, null, true, requestId, productCount);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.GET_ESTIMATE_PRICE_FOR_ITEM, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "count product by brand", description = "count product by brand")
  @ResponseBody
  public GdnRestSimpleResponse<EstimateItemPriceResponse> getEstimatePriceDetails(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String itemCode,
      @RequestParam(defaultValue = "1.5") double lowestPriceCoefficient) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "getEstimatePriceForItem", null, username, requestId, storeId, channelId,
            clientId, LoggerAspect.PRODUCT_LV3_ESTIMATE_PRICE, itemCode, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.info(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    GdnPreconditions
        .checkArgument(!StringUtils.isEmpty(itemCode), ProductLevel3ControllerErrorMessage.ITEM_CODE_MUST_NOT_BE_NULL);
    EstimateItemPriceDTO estimateItemPriceDTO = productLevel3Service
        .estimatePriceForFlow2ProductCreation(itemCode, lowestPriceCoefficient, maxInventoryRequest);
    EstimateItemPriceResponse estimateItemPriceResponse = EstimateItemPriceResponse.builder()
        .normalPrice(estimateItemPriceDTO.getNormalPrice())
        .offerPrice(estimateItemPriceDTO.getOfferPrice()).build();
    return new GdnRestSimpleResponse<>(EMPTY, EMPTY, Boolean.TRUE, requestId, estimateItemPriceResponse);
  }

  @Operation(summary = "Bulk Archive item Skus", description = "bulk archive item skus")
  @RequestMapping(value = ProductLevel3ControllerPath.ITEM_BULK_ARCHIVE, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestSingleResponse<ItemBulkArchiveResponse> bulkArchiveItems(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ItemBulkArchiveRequest request) {
    try {
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getItemSkus()),
          "itemSkus list should not be empty");
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getBusinessPartnerCode()),
          "business partner code should not be empty");
       List<String> failedItemSkus = productLevel3Service.bulkArchiveItems(request.getItemSkus(),
           request.getBusinessPartnerCode());
      ItemBulkArchiveResponse response = new ItemBulkArchiveResponse(failedItemSkus);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (Exception ex) {
      return new GdnRestSingleResponse(ex.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
  }

  @Operation(summary = "Get active brands by category id", description = "Get active brands by category id")
  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_ACTIVE_BRANDS_BY_CATEGORY_ID, method = RequestMethod.GET,
                  consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> activeBrandByCategoryId(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String categoryId,
      @RequestParam(defaultValue = "false", required = false) boolean clearCache) {
    try {
      GdnPreconditions
          .checkArgument(!StringUtils.isEmpty(categoryId), ProductLevel3ControllerErrorMessage.CATEGORY_ID_MUST_NOT_BE_NULL);
      List<PredefinedAllowedAttributeValueResponse> brandsResponse =
          productLevel3Service.getAllActiveBrandsByCNCategoryId(requestId, username, categoryId, clearCache);
      return new GdnRestListResponse<>(null, null, true, brandsResponse, new PageMetaData(), requestId);
    } catch (Exception ex) {
      return new GdnRestListResponse(null, null, true, new ArrayList(), new PageMetaData(),  requestId);
    }
  }

  @Operation(summary = "get products for suspension", description = "get products for suspension")
  @RequestMapping(value = ProductLevel3ControllerPath.GET_ALL_PRODUCTS, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestListResponse<SuspensionProductResponse> getAllProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody SummaryFilterRequest request,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "25") Integer size) {
    try {
      LoggerAttributeModel loggerAttribute =
          new LoggerAttributeModel(this, "getAllProducts", null, username, requestId, storeId, channelId, clientId,
              LoggerAspect.GET_ALL_PRODUCTS, request.toString(), request.toString());
      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      LOGGER.info("GetAllProducts : {}", MDC.get(LoggerParam.GENERIC_LOGGER.getParam()));
      PageRequest pageRequest = PageRequest.of(page, size);
      Page<SuspensionProductResponse> response =
          productLevel3Service.getAllProducts(request, requestId, username, storeId, pageRequest);
      return new GdnRestListResponse<SuspensionProductResponse>(response.getContent(),
          new PageMetaData(pageRequest.getPageSize(), pageRequest.getPageNumber(), response.getTotalElements()), requestId);
    } catch (Exception ex) {
      LOGGER.error(FAILED_GET_ALL_PRODUCTS, request, ex);
      return new GdnRestListResponse(null, null, false, new ArrayList(), new PageMetaData(), requestId);
    }
  }

  @Operation(summary = "get suspended items", description = "get suspended items")
  @RequestMapping(value = ProductLevel3ControllerPath.GET_SUSPENDED_ITEMS, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestListResponse<SuspensionItemResponse> getSuspendedItems(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody SummaryFilterRequest request,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "25") Integer size) {
    try {
      LoggerAttributeModel loggerAttribute =
          new LoggerAttributeModel(this, "getSuspendedItems", null, username, requestId, storeId, channelId, clientId,
              LoggerAspect.GET_SUSPENDED_ITEMS, request.toString(), request.toString());
      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      LOGGER.info("GetSuspendedItems : {}", MDC.get(LoggerParam.GENERIC_LOGGER.getParam()));
      LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
      PageRequest pageRequest = PageRequest.of(page, size);
      Page<SuspensionItemResponse> response =
          productLevel3Service.getSuspendedItems(request, requestId, username, storeId, pageRequest);
      return new GdnRestListResponse<SuspensionItemResponse>(response.getContent(),
          new PageMetaData(pageRequest.getPageSize(), pageRequest.getPageNumber(), response.getTotalElements()),
          requestId);
    } catch (Exception ex) {
      LOGGER.error(FAILED_GET_SUSPENDED_ITEM_LIST, request, ex);
      return new GdnRestListResponse(null, null, false, new ArrayList(), new PageMetaData(), requestId);
    }
  }

  @AuditLog
  @PostMapping(value = ProductLevel3ControllerPath.PRODUCT_SUSPENSION, produces = MediaType
      .APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Product suspension", description = "Product suspension for products")
  @ResponseBody
  public GdnBaseRestResponse doSuspensionProductsActions(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody SuspensionProductRequest request) {
    try {
      LoggerAttributeModel loggerAttribute =
          new LoggerAttributeModel(this, "doSuspensionProductsActions", null, username, requestId, storeId, channelId,
              clientId, LoggerAspect.PRODUCT_SUSPENSION, request.getAction(), request.toString());
      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      LOGGER.info("ProductSuspension : {}", MDC.get(LoggerParam.GENERIC_LOGGER.getParam()));
      LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
      productLevel3Service.doSuspensionProductsActions(storeId, username, request);
    } catch (Exception e) {
      LOGGER.error(FAILED_SUSPENSION_ACTION, request, e);
      return new GdnBaseRestResponse(e.getMessage(), StringUtils.EMPTY, Boolean.FALSE, requestId);
    }
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.SUSPENSION_HISTORY, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get the suspension history of productSku")
  @ResponseBody
  public GdnRestListResponse<ProductSuspensionHistoryResponse> getSuspensionHistory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String productSku) throws Exception {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "getSuspensionHistory", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.SUSPENSION_HISTORY, productSku, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productSku),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    PageRequest pageRequest = PageRequest.of(page, size);
    try {
      Page<ProductSuspensionHistory> productSuspensionHistories =
          this.productLevel3Service.getSuspensionHistory(storeId, productSku, pageRequest);
      List<ProductSuspensionHistoryResponse> productSuspensionHistoryResponseList = ControllerUtils
          .convertProductSuspensionHistoryToProductSuspensionHistoryResponse(productSuspensionHistories.getContent());
      return new GdnRestListResponse<>(productSuspensionHistoryResponseList,
          new PageMetaData(pageRequest.getPageSize(), pageRequest.getPageNumber(),
              productSuspensionHistories.getTotalElements()), requestId);
    } catch (Exception e) {
      LOGGER.error(LoggerStandard.appendLogErrorTemplate(e.getMessage(), e));
      return new GdnRestListResponse<>("Failed to load productSku suspension history",
          ErrorCategory.DATA_NOT_FOUND.getCode(), false, null, null, requestId);
    }
  }

  @AuditLog
  @PostMapping(value = ProductLevel3ControllerPath.BULK_PRODUCT_SUSPENSION, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Bulk Product suspension", description = "Bulk Product suspension for products")
  @ResponseBody
  public GdnRestListResponse<SuspensionProductResponse> doBulkSuspensionProductsActions(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody SuspensionProductRequestList request) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "doBulkSuspensionProductsActions", null, username, requestId, storeId, channelId,
            clientId, LoggerAspect.BULK_PRODUCT_SUSPENSION, request.toString(), request.toString());
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.info("BulkProductSuspension for request : {}", request);
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    List<SuspensionProductResponse> failedResponse =
        productLevel3Wrapper.doBulkProductSuspension(storeId, username, request.getSuspensionProductRequestList());
    return new GdnRestListResponse<>(failedResponse, new PageMetaData(), requestId);
  }

   @RequestMapping(value = ProductLevel3ControllerPath.COMPARE_PRODUCT_CATEGORY_WHOLEALE, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "compare Product And Category Wholesale Rules", description = "compare Product And Category Wholesale Rules")
  @ResponseBody
  public GdnBaseRestResponse compareProductAndCategoryWholesale(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam(required = false) String username, @RequestParam String productCode,
     @RequestParam String categoryCode) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode),
        ProductLevel3ControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(categoryCode),
        ProductLevel3ControllerErrorMessage.CATEGORY_CODE_MUST_NOT_BE_BLANK);
    try {
      return new GdnBaseRestResponse(null, null, this.
          productLevel3Service.checkCategoryProductWholesaleRules(storeId, productCode, categoryCode), requestId);
    } catch (Exception e) {
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  private void validateProductEditInfo(ProductLevel3Request product) throws Exception {
    if (Objects.nonNull(product.getUniqueSellingPoint())) {
      String uspWithoutTags = ControllerUtils.getFilteredUSPAndDescription(product.getUniqueSellingPoint());
      GdnPreconditions
          .checkArgument(uspWithoutTags.length() <= ProductControllerErrorMessage.MAXIMUM_UNIQUE_SELLING_POINT_LENGTH,
              ProductControllerErrorMessage.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    }
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getBusinessPartnerCode()),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!product.getAttributes().isEmpty(),
        ProductLevel3ControllerErrorMessage.ATTRIBUTE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getSpecificationDetail()),
        ProductLevel3ControllerErrorMessage.SPECIFICATION_DETAIL_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getAttributes().get(0).getAttributeCode()),
        ProductLevel3ControllerErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getAttributes().get(0).getAttributeType()),
        ProductLevel3ControllerErrorMessage.ATTRIBUTE_TYPE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!product.getAttributes().get(0).getValues().isEmpty(),
        ProductLevel3ControllerErrorMessage.ATTRIBUTE_VALUE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(product.getAttributes().get(0).getSkuValue() != null,
        ProductLevel3ControllerErrorMessage.ATTRIBUTE_SKU_VALUE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getProductName()),
        ProductLevel3ControllerErrorMessage.PRODUCT_NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(product.getProductName().length() <= PRODUCT_NAME_LIMIT,
        ProductLevel3ControllerErrorMessage.PRODUCT_NAME_LTE_LIMIT);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getBrand()),
        ProductLevel3ControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getDescription()),
        ProductLevel3ControllerErrorMessage.DESCRIPTION_MUST_NOT_BE_BLANK);
    String descriptionWithoutTags = ControllerUtils.getFilteredUSPAndDescription(product.getDescription());
    GdnPreconditions.checkArgument(descriptionWithoutTags.length() <= maximumCharactersInDescription, String
        .format(ProductLevel3ControllerErrorMessage.DESCRIPTION_MUST_NOT_BE_MORE_THAN_MAX_CHARACTERS,
            maximumCharactersInDescription));
    GdnPreconditions.checkArgument(product.getDescription().length() <= maximumCharactersWithoutFormattingDescription, String
        .format(ProductLevel3ControllerErrorMessage.CHARACTER_LIMIT_REACHED_PLEASE_REDUCE_FORMATTING,
            maximumCharactersWithoutFormattingDescription));
    ProductSystemParameter productSystemParameter = productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    String[] apiKeys = youTubeDataApiKey.split(COMMA_SEPARATOR);
    int apiKeyIndex = (int) (Math.random() * apiKeys.length);
    String youTubeApiKey = apiKeys[apiKeyIndex];
    LOGGER.debug("Using youtube api key index: {} for url: {}", apiKeyIndex, product.getUrl());
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(product.getUrl(), youTubeApiKey, youTube, Boolean.valueOf(productSystemParameter.getValue()),
        youtubeRegex);
    if (!youTubeUrlResponse) {
      product.setUrl(StringUtils.EMPTY);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_ITEMS_PRICE_STOCK, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item price", description = "update item price")
  @ResponseBody
  public GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> updateItemsPriceStockImages(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String businessPartnerCode,
      @RequestBody UpdateItemsPriceStockImagesRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "updateItemsPriceStockImages", null, username, requestId, storeId, channelId,
            clientId, LoggerAspect.PRODUCT_LV3_UPDATE, null, request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getProductItems()),
        ProductLevel3ControllerErrorMessage.PRICE_STOCK_IMAGE_REQUEST_MUST_NOT_BE_EMPTY);
    try {
      ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
          this.productLevel3Service.editProductItemsPriceStockImages(storeId, request, businessPartnerCode);
      return new GdnRestSingleResponse<>(itemsPriceStockImagesUpdateResponse, requestId);
    } catch (Exception e) {
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_SUMMARY_DETAILS, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 summary details by filter at controller", description = "filter product level 3 summary details by filter at controller")
  @ResponseBody
  public GdnRestListResponse<ProductLevel3SummaryDetailsResponse> filterSummaryDetails(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestBody ProductLevel3SummaryDetailsRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "filterSummaryDetails", businessPartnerCode, username, requestId, storeId,
            channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, page + ":" + size, request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductSku()),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    PageRequest pageRequest = PageRequest.of(page, size);
    ProductLevel3SummaryFilterDetails filter =
        productLevel3RequestConverter.convertProductLevel3SummaryRequestToProductLevel3SummaryDetailsFilter(request);
    filter.setBusinessPartnerCode(businessPartnerCode);
    Page<ProductLevel3SummaryDetails> pageOfProductLevel3SummaryDetails =
        productLevel3Service.findSummaryDetailsByFilter(storeId, filter, pageRequest);
    List<ProductLevel3SummaryDetailsResponse> productLevel3SummaryResponses = new ArrayList<>();
    for (ProductLevel3SummaryDetails productLevel3SummaryDetails : pageOfProductLevel3SummaryDetails.getContent()) {
      ProductLevel3SummaryDetailsResponse response = productLevel3ResponseConverter
          .convertProductLevel3SummaryDetailsToProductLevel3SummaryDetailsResponse(productLevel3SummaryDetails);
      productLevel3SummaryResponses.add(response);
    }
    return new GdnRestListResponse<>(productLevel3SummaryResponses,
        new PageMetaData(pageRequest.getPageSize(), pageRequest.getPageNumber(),
            pageOfProductLevel3SummaryDetails.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.GET_PICKUP_POINT_CODES, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get pickup-points by l3", description = "get pickup-points by l3")
  @ResponseBody
  public GdnRestListResponse<PickupPointCodeResponse> getPickupPointCodes(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String username, @RequestParam int page, @RequestParam(defaultValue = "20") int size,
      @RequestParam String productSku, @RequestParam boolean needCorrection, @RequestParam String businessPartnerCode,
      @RequestParam boolean fbbActivated) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
      ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    try {
      LOGGER.info("getPickupPointCodes productSku : {}, needCorrection : {}", productSku, needCorrection);
      Page<PickupPointCodeResponse> response =
          productLevel3Service.getPickupPointCodes(productSku, page, size, needCorrection, storeId, businessPartnerCode,
              fbbActivated);
      return new GdnRestListResponse(response.getContent(), new PageMetaData(size, page, response.getTotalElements()),
          requestId);
    } catch (ApplicationException ae) {
      LOGGER.error("MPP Error while fetching pickup-point codes for productSku : {}, businessPartnerCode : {} with error : {}",
        productSku, businessPartnerCode, ae.getStackTrace());
      return new GdnRestListResponse<>(ae.getErrorMessage(), ae.getErrorCodes().getCode(), false, requestId);
    }
    catch (Exception e) {
      LOGGER.error("Error while fetching pickup-point codes for productSku : {}, page : {}, size : {}, requestId : {}",
          productSku, page, size, requestId, e);
      return new GdnRestListResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.GET_UNIQUE_PICKUP_POINT_CODES, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get pickup-points by l3", description = "get pickup-points by l3")
  @ResponseBody
  public GdnRestSingleResponse<UniquePickupPointCodeResponse> getUniquePickupPointCodes(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String username, @RequestParam String productSku) throws Exception {
    try {
      UniquePickupPointCodeResponse response = productLevel3Service.getUniquePickupPointCodes(productSku);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (Exception e) {
      LOGGER.error("Error while fetching unique-pickup-point codes for productSku : {}, requestId : {}", productSku,
          requestId, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_PICKUP_POINT_CODES, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update pickup point codes", description = "update pickup point codes")
  @ResponseBody
  public GdnRestSingleResponse<PickupPointUpdateResponse> updatePickupPointCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody PickupPointUpdateRequest pickupPointUpdateRequest) throws Exception {
    validatePickupRequest(pickupPointUpdateRequest);
    try {
      PickupPointUpdateResponse result = this.productLevel3Service.updatePickupPointCodes(pickupPointUpdateRequest);
      if (Objects.nonNull(result.getApiErrorCode())) {
        LOGGER.error("Error while updating the pickup points for productSku : {} and error code : {}",
            pickupPointUpdateRequest.getProductSku(), result);
        return new GdnRestSingleResponse(result.getApiErrorCode().getDesc(), result.getApiErrorCode().getCode(), false,
            result, requestId);
      } else {
        return new GdnRestSingleResponse<>(null, null, true, result, requestId);
      }
    } catch (Exception e) {
      LOGGER.error("Error while updating the pickup points for productSku : {} ",
          pickupPointUpdateRequest.getProductSku(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false, null, requestId);
    }
  }

  private void validatePickupRequest(PickupPointUpdateRequest product) throws Exception {
    GdnPreconditions.checkArgument(!product.getItemsPickupPoint().isEmpty(),
        ProductLevel3ControllerErrorMessage.ITEM_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getBusinessPartnerCode()),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getProductSku()),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    for (PickupPointRequest pickupPointRequest : product.getItemsPickupPoint()) {
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(pickupPointRequest.getPickupPointCode()),
          ProductLevel3ControllerErrorMessage.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(pickupPointRequest.getItemSku()),
          ProductLevel3ControllerErrorMessage.ITEM_SKU_MUST_NOT_BE_BLANK);
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_LOGISTICS, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update logistic content at controller", description = "update logistic content at controller")
  @ResponseBody
  public GdnBaseRestResponse updateLogistics(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false, defaultValue = "true") boolean isOnlyExternal,
      @RequestParam(required = false, defaultValue = "false") boolean isNeedCorrection,
      @RequestBody ProductLevel3UpdateRequest product) throws Exception {
    LOGGER.info("Updating the logistic info for request : {}", product);
    validateUpdateProduct(product);
    try {
      if (Objects.nonNull(product.getPreOrder()) && Boolean.TRUE.equals(product.getPreOrder().getIsPreOrder())) {
        ValidationUtil.validatePreOrder(product.getPreOrder(), preOrderMaximumDays, preOrderMaximumWeek,
            convertPreOrderDateToJKT);
      }
      ApiErrorCode result = null;
      if (!isNeedCorrection) {
        result = this.productLevel3Service.updateLogistics(product, isOnlyExternal, null, false, false, migrateProductInOtherLogisticUpdateFlow);
      } else {
        result = this.productLevel3Service.updateLogisticsForNeedRevision(product);
      }
      if (Objects.nonNull(result)) {
        LOGGER.error("Error while updating the logistic detail for productSku : {} and error code : {}",
            product.getProductSku(), result);
        return new GdnBaseRestResponse(result.getDesc(), result.getCode(), false, requestId);
      } else {
        return new GdnBaseRestResponse(null, null, true, requestId);
      }
    } catch (Exception e) {
      LOGGER.error("Error while updating the logistic detail for productSku : {} , Error - ", product.getProductSku(), e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.GET_PRODUCT_VARIANT_NAME, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get variant name for l3", description = "get variant name for l3")
  @ResponseBody
  public GdnRestListResponse<ProductItemNameResponse> getProductVariantsName(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String username, @RequestParam int page, @RequestParam(defaultValue = "20") int size,
      @RequestParam String productSku) {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productSku),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    try {
      Page<ProductItemNameResponse> response =
          productLevel3Service.getItemSummaryResponseByProductSku(productSku, page, size);
      return new GdnRestListResponse(response.getContent(), new PageMetaData(size, page, response.getTotalElements()),
          requestId);
    } catch (Exception e) {
      LOGGER.error("Error while fetching variant name for productSku : {}, page : {}, size : {}, requestId : {}",
          productSku, page, size, requestId, e);
      return new GdnRestListResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  private void validateUpdateProduct(ProductLevel3UpdateRequest product) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getProductSku()),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getProductCode()),
        ProductLevel3ControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(product.getProductType()),
        ProductLevel3ControllerErrorMessage.PRODUCT_TYPE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getBusinessPartnerCode()),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(product.getShippingWeight()),
        ProductLevel3ControllerErrorMessage.SHIPPING_WEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(product.getLength()),
        ProductLevel3ControllerErrorMessage.ITEM_LENGTH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(product.getWidth()),
        ProductLevel3ControllerErrorMessage.ITEM_WIDTH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(product.getHeight()),
        ProductLevel3ControllerErrorMessage.ITEM_HEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(product.getWeight()),
        ProductLevel3ControllerErrorMessage.ITEM_WEIGHT_MUST_NOT_BE_BLANK);
  }

  @AuditLog
  @PostMapping(value = ProductLevel3ControllerPath.GET_PRODUCT_SKU_LIST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Product L3 list filter", description = "Product L3 list filter")
  @ResponseBody
  public GdnRestListResponse<ProductL3SummaryResponse> getProductSkuSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String merchantCode,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "25") int size,
      @RequestBody ProductL3SummaryRequest productL3SummaryRequest) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "getProductSkuSummary", merchantCode, username,
            requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_SKU_FILTER,
            productL3SummaryRequest.toString(), productL3SummaryRequest.toString());
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.info("Product Sku filter for request : {}", productL3SummaryRequest);
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    try {
      Page<ProductL3SummaryResponse> response = productLevel3Service
          .getProductL3Summary(storeId, requestId, merchantCode, page, size,
              productL3SummaryRequest);
      return new GdnRestListResponse<>(null, null, true, response.getContent(),
          new PageMetaData(response.getSize(), response.getNumber(), response.getTotalElements()),
          requestId);
    } catch (Exception e) {
      LOGGER.error("Error fetching product skus for request : {} ", productL3SummaryRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }


  @RequestMapping(value = ProductLevel3ControllerPath.GET_PRODUCT_DETAIL, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get L3 details by product sku", description = "get L3 details by product sku")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3DetailResponse> getL3DetailByProductSku(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String username, @RequestParam String productSku,
      @RequestParam(required = false, defaultValue = "false") boolean isNeedCorrection) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productSku),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    try {
      ProductLevel3DetailResponse response = productLevel3Service.getL3DetailByProductSku(productSku, isNeedCorrection,
          true, new ArrayList<>(), false);
      return new GdnRestSingleResponse<>(response, requestId);
    } catch (Exception e) {
      LOGGER.error("Error while fetching the L3 detail for productSku : {}, requestId : {}", productSku, requestId, e);
      return new GdnRestSingleResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @Operation(summary = "Archive product Skus", description = "Archive product skus")
  @RequestMapping(value = ProductLevel3ControllerPath.PRODUCT_ARCHIVE, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestSingleResponse<ItemBulkArchiveResponse> archiveProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam boolean doArchive, @RequestBody SimpleListStringRequest request) {
    GdnPreconditions
        .checkArgument(CollectionUtils.isNotEmpty(request.getValue()), "ProductSku list should not be empty");
    try {
      ItemBulkArchiveResponse itemBulkArchiveResponse =
          productLevel3Service.toggleArchiveProducts(request.getValue(), doArchive);
      return new GdnRestSingleResponse<>(null, itemBulkArchiveResponse.getErrorCode(),
          CollectionUtils.isEmpty(itemBulkArchiveResponse.getFailedItemSkus()), itemBulkArchiveResponse, requestId);
    } catch (Exception ex) {
      return new GdnRestSingleResponse(ex.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @Operation(summary = "Fetch suspension notes by product sku list", description = "Fetch suspension notes by product sku list")
  @RequestMapping(value = ProductLevel3ControllerPath.GET_SUSPENSION_NOTES_BY_PRODUCT_SKUS,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestListResponse<ProductSuspensionHistoryResponse> fetchProductSuspensionHistory(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductSkuListRequest productSkuListRequest) {
    LOGGER.info("Fetch suspension notes by list of productSku : {}", productSkuListRequest);
    try {
      GdnPreconditions
          .checkArgument(CollectionUtils.isNotEmpty(productSkuListRequest.getProductSkuList()),
              ErrorMessages.PRODUCT_LIST_EMPTY);
      List<ProductSuspensionHistoryResponse> productSuspensionHistoryResponses = productLevel3Service
          .getProductSuspensionHistoryByProductSkus(storeId,
              productSkuListRequest.getProductSkuList());
      return new GdnRestListResponse<>(null, null, true, productSuspensionHistoryResponses,
          new PageMetaData(0, 0, productSuspensionHistoryResponses.size()), requestId);
    } catch (Exception e) {
      LOGGER.error("Error while fetching product suspension history for : {}, error - ",
          productSkuListRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          null, null, requestId);
    }
  }

  @Operation(summary = "Retry flow 2 activation for skip review true products", description = "Retry flow 2 activation for skip review true products")
  @RequestMapping(value = ProductLevel3ControllerPath.RETRY_FLOW2_ACTIVATION, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnBaseRestResponse retryActivation(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductSkuListRequest productSkuListRequest) {
    try {
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productSkuListRequest.getProductSkuList()),
          ErrorMessages.PRODUCT_LIST_EMPTY);
      productLevel3Service.retrySkipReviewProductActivation(storeId, productSkuListRequest.getProductSkuList());
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Error while retry product activation for : {}, error - ", productSkuListRequest, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_PRODUCT_ITEM_VIEW_CONFIG, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product item view configuration in PBP for product in Draft, IN_Progress, Need Revision"
      , description = "update product item view configuration")
  @ResponseBody
  public GdnBaseRestResponse updateProductItemViewConfig(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productSku") String productSku, @RequestBody ProductLevel3ViewConfigStockRequest request) throws Exception {
    LOGGER.info("update product item view configuration productSku : {} : request : {}",productSku, request);
    this.productLevel3Service.updateProductItemViewConfig(request, productSku);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_PRODUCT_LEVEL3_INFO, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product level3 information, logistics, dimension"
      , description = "update product level3 information")
  @ResponseBody
  public GdnRestSingleResponse<EditProductV2Response> updateProductLevel3Info(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productSku") String productSku,
      @RequestBody UpdateProductLevel3InfoRequest request) throws Exception {
    LOGGER.info("update product level3 information productSku : {} : request : {}", productSku, request);
    try {
      if (Objects.nonNull(request.getPreOrder()) && Boolean.TRUE.equals(request.getPreOrder().getIsPreOrder())) {
        ValidationUtil.validatePreOrder(request.getPreOrder(), preOrderMaximumDays, preOrderMaximumWeek,
            convertPreOrderDateToJKT);
      }
      EditProductResponse response = this.productLevel3Service.updateProductLevel3Info(request, productSku);
      EditProductV2Response editProductResponse = ResponseHelper.toEditProductV2Response(response);
      if (Objects.isNull(editProductResponse.getApiErrorCode())) {
        return new GdnRestSingleResponse<>(null, null, true, editProductResponse, requestId);
      } else {
        LOGGER.error("Error while updating the product level3 info for productSku : {} and error code : {}", productSku,
            editProductResponse);
        return new GdnRestSingleResponse(editProductResponse.getApiErrorCode().getDesc(),
            editProductResponse.getApiErrorCode().getCode(), false, editProductResponse, requestId);
      }
    } catch (ApplicationRuntimeException e) {
      LOGGER.error("error updating  the product level3 info productSku: {} ", productSku, e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false,
          EditProductV2Response.builder().apiErrorCode(ApiErrorCode.INVALID_DATA_INPUT).build(), requestId);
    } catch (ApplicationException e) {
      LOGGER.error("error updating the product level3 info productSku: {} ", productSku, e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false,
          EditProductV2Response.builder().apiErrorCode(ApiErrorCode.INVALID_DATA_INPUT).build(), requestId);
    } catch (Exception e) {
      LOGGER.error("error updating the product level3 info productSku: {} ", productSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          null, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_IMAGES, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product item images", description = "update product item images")
  @ResponseBody
  public GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> updateImages(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductImageEditRequest productImageEditRequest) throws Exception {
    validateProductImageEditRequest(productImageEditRequest);
    try {
      ItemsPriceStockImagesUpdateResponse result =
          this.productLevel3Service.updateImages(storeId, productImageEditRequest);
      if (Objects.nonNull(result.getApiErrorCode())) {
        LOGGER.error("Error while updating the images for productSku : {} and error code : {}",
            productImageEditRequest.getProductSku(), result);
        return new GdnRestSingleResponse(result.getApiErrorCode().getDesc(), result.getApiErrorCode().getCode(), true,
            result, requestId);
      } else {
        return new GdnRestSingleResponse<>(null, null, true, result, requestId);
      }
    } catch (Exception e) {
      LOGGER.error("Error while updating the images for productSku : {} ", productImageEditRequest.getProductSku(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false, null, requestId);
    }
  }

  private void validateProductImageEditRequest(ProductImageEditRequest product) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getBusinessPartnerCode()),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getImagePath()),
        ProductLevel3ControllerErrorMessage.IMAGE_PATH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(product.getProductSku()),
        ProductLevel3ControllerErrorMessage.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    if (CollectionUtils.isNotEmpty(product.getProductItems())) {
      for (ItemImageEditRequest itemImageEditRequest : product.getProductItems()) {
        GdnPreconditions.checkArgument(!StringUtils.isEmpty(itemImageEditRequest.getItemSku()),
            ProductLevel3ControllerErrorMessage.ITEM_SKU_MUST_NOT_BE_BLANK);
      }
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.FETCH_IN_PROGRESS_L4_BY_PICKUP_POINT_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "check for inProgress products by business partner code and pickuppoint", description = "check for inProgress products by business partner code and pickuppoint")
  @ResponseBody
  public GdnRestListResponse<InProgressProductsByPickupPointCodeResponse> inProgressProductsByPickupPointCodeAndBusinessPartnerCode(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String businessPartnerCode, @RequestParam String pickupPointCode,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size) {
    LOGGER.info("inProgress product by bp code : {} : ppCode : {}", businessPartnerCode, pickupPointCode);
    try {
      Page<InProgressProductsByPickupPointCodeResponse> inProgressProductsByPPCode =
          productLevel3Service.getInProgressProductsByBusinessPartnerAndPickupPointCode(storeId, businessPartnerCode,
              pickupPointCode, page, size);
      return new GdnRestListResponse<>(inProgressProductsByPPCode.getContent(),
          new PageMetaData(size, page, inProgressProductsByPPCode.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error inProgressProductsBy bp code: {}, ppCode : {}, error ", businessPartnerCode, pickupPointCode, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, Collections.emptyList(), null, requestId);
    }

  }

  @GetMapping(value = ProductLevel3ControllerPath.FETCH_IN_PROGRESS_L3_BY_SIZE_CHART_CODE,
              produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(description = "API to fetch InProgress products based on sizeChartCode", summary =
      "API to fetch InProgress products based on sizeChartCode")
  public GdnRestListResponse<InProgressProductsBySizeChartCodeResponse> fetchInProgressL3BySizeChartCode(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String sizeChartCode,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size) {
    try {
      Page<InProgressProductsBySizeChartCodeResponse> inProgressProductsBySizeChartCode =
          productLevel3Service.getInProgressProductsBySizeChartCode(storeId, sizeChartCode, page,
              size);
      return new GdnRestListResponse<>(inProgressProductsBySizeChartCode.getContent(),
          new PageMetaData(size, page, inProgressProductsBySizeChartCode.getTotalElements()),
          requestId);
    } catch (Exception exception) {
      log.error("Error inProgressProductsBySizeChartCode : {}, error ", sizeChartCode, exception);
      return new GdnRestListResponse<>(exception.getMessage(), exception.getMessage(), false,
          Collections.emptyList(), null, requestId);
    }
  }

}
