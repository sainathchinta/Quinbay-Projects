package com.gdn.x.productcategorybase.service.impl;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.BeanUtils;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.inventory.dto.WarehouseMasterSKUEvent;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.EditedReviewTypeConstants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.ProductMasterEvent;
import com.gdn.x.productcategorybase.ProductMigrationStatus;
import com.gdn.x.productcategorybase.ProductMigrationType;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CompressedVideoUpdateEventModel;
import com.gdn.x.productcategorybase.domain.event.model.InternalProductHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityAttributeModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VendorPublishEventModel;
import com.gdn.x.productcategorybase.dto.CategoryChangeDTO;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.MasterProductDataUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.ProductDTO;
import com.gdn.x.productcategorybase.dto.ProductDetailEditDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.UpdateNeedRevisionDTO;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import com.gdn.x.productcategorybase.dto.request.BatchVatUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMasterDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.AuditTrailDto;
import com.gdn.x.productcategorybase.dto.response.AuditTrailListResponse;
import com.gdn.x.productcategorybase.dto.response.BatchVatUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemCompleteResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleItemDetailResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.ProductMigration;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;
import com.gdn.x.productcategorybase.outbound.matrix.MatrixOutbound;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionResponse;
import com.gdn.x.productcategorybase.outbound.model.SingleBaseResponse;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CategoryReferenceService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryShippingService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.FileStorageService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductAttributeExtractionService;
import com.gdn.x.productcategorybase.service.ProductAttributeService;
import com.gdn.x.productcategorybase.service.ProductAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductItemServiceWrapper;
import com.gdn.x.productcategorybase.service.ProductMigrationService;
import com.gdn.x.productcategorybase.service.ProductScoreService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.SchedulerService;
import com.gdn.x.productcategorybase.service.SizeChartService;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.util.AggregateUtil;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.util.ProductImageUtil;
import com.gdn.x.productcategorybase.util.ProductUtil;
import com.google.common.collect.ImmutableList;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import com.gdn.x.productcategorybase.repository.ProductImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemImageRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.ProductItemAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductItemRepository;
import com.gdn.x.productcategorybase.repository.ProductCategoryRepository;

@Slf4j
@Service
public class ProductServiceWrapperImpl implements ProductServiceWrapper{

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private SchedulerService schedulerService;

  @Autowired
  private ProductItemService productItemService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private CategoryReferenceService categoryReferenceService;

  @Autowired
  private CategoryShippingService categoryShippingService;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private ProductScoreService productScoreService;

  @Autowired
  private ProductMigrationService productMigrationService;

  @Autowired
  private ProductAttributeExtractionService productAttributeExtractionService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private ImageService imageService;

  @Autowired
  private MatrixOutbound matrixOutbound;

  @Autowired
  private ProductItemServiceWrapper productItemServiceWrapper;

  @Autowired
  private ProductCategoryService productCategoryService;

  @Autowired
  private ProductAttributeService productAttributeService;

  @Autowired
  private AttributeService attributeService;

  @Autowired
  private AllowedAttributeValueService allowedAttributeValueService;

  @Autowired
  private ProductAttributeValueService productAttributeValueService;

  @Autowired
  private BrandService brandService;

  @Value("${validate.dimension.warehouse.event.switch.enabled}")
  private boolean validateDimensionSwitch;

  @Value("${publish.vendor.event.for.ds.attribute.mapping.enabled}")
  private boolean publishVendorEventForDsAttributeMappingEnabled;

  @Value("${client-id.video.compression.event}")
  private String clientIdForVideoCompression;

  @Value("${skip.mfd.true.products.for.deletion}")
  private boolean skipMfdTrueProductsForDeletion;

  @Value("${ds.extraction.listener.remove.empty.predefined.value}")
  private boolean dsExtractionListenerRemoveEmptyPredefinedValue;

  @Value("${product.attribute.migration.predefined.attribute.value.filtering.enabled}")
  private boolean productAttributeMigrationPredefinedAttributeValueFilteringEnabled;

  @Autowired
  private ProductImageRepository productImageRepository;

  @Autowired
  private ProductItemImageRepository productItemImageRepository;

  @Autowired
  private ProductAttributeRepository productAttributeRepository;

  @Autowired
  private ProductAttributeValueRepository productAttributeValueRepository;

  @Autowired
  private ProductItemAttributeRepository productItemAttributeRepository;

  @Autowired
  private ProductItemRepository productItemRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private SizeChartService sizeChartService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductServiceWrapperImpl.class);
  private static final String PRODUCT_UPDATE_FAILED = "Product update failed for productCode : {}";
  private static final int LIST_SIZE_FOR_BATCH = 30;
  private static final ImmutableList<String> VAT_ALLOWED_MERCHANT_TYPES = ImmutableList.of("TD", "TC");
  private static final String NULL_VALUE_ON_REQUEST_MAP = "null";
  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @PostConstruct
  public void init() {
    ProductImageUtil.setFileStorageService(fileStorageService);
    ConverterUtil.setFileStorageService(fileStorageService);
  }

  @Override
  public SimpleMasterProductUpdateResponseDTO updateMasterProductData(
      String storeId, SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO) {
    SimpleMasterProductUpdateResponseDTO simpleMasterProductUpdateResponseDTO =
        new SimpleMasterProductUpdateResponseDTO(simpleMasterProductUpdateDTO.getProductCode());
    try {
      MasterProductDataUpdateDTO masterProductDataUpdateDTO =
        productService.updateProductAndGetDimensionChanged(storeId, simpleMasterProductUpdateDTO);
      productService.evictCacheForSimpleMasterProductUpdate(storeId,
          masterProductDataUpdateDTO.getProduct().getId(), masterProductDataUpdateDTO.getProduct().getProductCode());
      Product product = getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(
          storeId, simpleMasterProductUpdateDTO.getProductCode());
      domainEventPublisherService.publishProductChangeCategory(product, null,
          masterProductDataUpdateDTO.isBrandUpdated(), true, true, false, new HashSet<>());
      simpleMasterProductUpdateResponseDTO.setUpdateSuccess(Boolean.TRUE);
      simpleMasterProductUpdateResponseDTO.setDimensionOrDgLevelUpdated(
          masterProductDataUpdateDTO.isDimensionOrDgLevelUpdated());
    } catch (Exception e) {
      LOGGER.error(PRODUCT_UPDATE_FAILED, simpleMasterProductUpdateDTO.getProductCode(), e);
      simpleMasterProductUpdateResponseDTO.setUpdateSuccess(Boolean.FALSE);
      simpleMasterProductUpdateResponseDTO.setReasonOfFailure(e.getMessage());
    }
    return simpleMasterProductUpdateResponseDTO;
  }

  @Override
  public ProductBrandUpdateResponseDTO updateProductBrandData(String storeId,
      ProductBrandUpdateDTO productBrandUpdateDTO) {
    ProductBrandUpdateResponseDTO productBrandUpdateResponseDTO = new ProductBrandUpdateResponseDTO();
    try {
      Pair<Product, String> productWithBrandName = productService.updateProductBrand(storeId, productBrandUpdateDTO);
      productService.evictCacheForSimpleMasterProductUpdate(storeId, productWithBrandName.getKey().getId(),
          productWithBrandName.getKey().getProductCode());
      domainEventPublisherService.publishProductChangeCategory(productWithBrandName.getKey(), null, true, true, true,
          false, new HashSet<>());
      productBrandUpdateResponseDTO.setProductCode(productWithBrandName.getKey().getProductCode());
      productBrandUpdateResponseDTO.setBrandCode(productWithBrandName.getValue());
      productBrandUpdateResponseDTO.setBrandName(productWithBrandName.getKey().getBrand());
    } catch (Exception e) {
      log.error(PRODUCT_UPDATE_FAILED, productBrandUpdateDTO.getProductCode(), e);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "Brand update failed");
    }
    return productBrandUpdateResponseDTO;
  }

  @Override
  public void processProductAttributeDataBackFilling(
      CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel) {
    Pair<Product, List<ProductAttributeValue>> productAndproductAttributeValuesPair =
        productService.fetchProductAndInsertMissingProductAttributes(
            productAttributeDataBackFillingEventModel);
    if (CollectionUtils.isNotEmpty(productAndproductAttributeValuesPair.getRight())
        && Objects.nonNull(productAndproductAttributeValuesPair.getLeft())) {
      productService.clearProductCacheAndProductAttributesCache(productAttributeDataBackFillingEventModel.getStoreId(),
          productAttributeDataBackFillingEventModel.getProductCode(),
          productAndproductAttributeValuesPair.getLeft().getId());
      for (ProductAttributeValue productAttributeValue :
          productAndproductAttributeValuesPair.getRight()) {
        if (!productAttributeMigrationPredefinedAttributeValueFilteringEnabled
            || !productAttributeValue.isMarkForDelete()) {
          domainEventPublisherService.publishPBPAttributeMigrationEvent(productAttributeValue,
              productAttributeDataBackFillingEventModel.getProductCode());
        }
      }
    }
    else {
      schedulerService.updateProductMigrationStatus(
          Optional.ofNullable(productAttributeDataBackFillingEventModel.getStoreId())
              .orElse(Constants.DEFAULT_STORE_ID), Constants.CLIENT_ID,
          CommonUtil.getProductMigrationRequest(productAttributeDataBackFillingEventModel,
              Objects.isNull(productAndproductAttributeValuesPair.getLeft()) ?
                  ErrorMessage.PRODUCT_NOT_FOUND.getMessage() :
                  ErrorMessage.NO_PENDING_ATTRIBUTES_TO_BE_MAPPED.getMessage()));
    }
  }

  @Override
  public boolean updateProductAndItemImagesByProductCode(
      ProductAndItemImageRequest productAndItemImageRequest, String storeId, boolean setDgLevel) {
    try {
      ProductPublishUpdateDTO productPublishUpdateDTO = CommonUtil.updateProductPublishUpdateDTO(productAndItemImageRequest);
      Product updatedProduct =
        productService.updateProductAndItemImagesByProductCode(productAndItemImageRequest, storeId,
          setDgLevel);
      productService.evictCompleteProductAndItemsCache(storeId, updatedProduct);
      productPublishUpdateDTO.setProduct(updatedProduct);
      domainEventPublisherService.publishProductChangeCategory(productPublishUpdateDTO, null, false, false, false,
          false, false, new HashSet<>(), false, false, new HashSet<>());
      return true;
    } catch (Exception e) {
      LOGGER.error("Error while updating location paths in PCB, productCode :{} ",
          productAndItemImageRequest.getProductCode(), e);
      return false;
    }
  }

  private void evictCompleteProductAndItemsCacheAndPublish(String storeId,
    Product updatedProduct) throws Exception {
    productService.evictCompleteProductAndItemsCache(storeId, updatedProduct);
    productService.publishProduct(updatedProduct, false);
  }

  @Async
  @Override
  public void republishProductByProductCodesToAgp(String storeId, List<String> productCodes, boolean split) {
    productCodes.stream()
        .filter(StringUtils::isNotEmpty)
        .map(productCode -> findDetailProduct(storeId,productCode))
        .filter(Objects::nonNull)
        .forEach(product -> republishProductToAgp(product,split));
  }

  @Override
  public Page<SimpleItemDetailResponse> getItemListByProductCode(String storeId, String productCode, int page, int size)
      throws Exception {
    Product product = productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItemPage =
        this.productItemService.findListOfItemsByProduct(storeId, product, pageable);
    return new PageImpl<>(Optional.ofNullable(productItemPage.getContent()).orElse(new ArrayList<>()).stream()
        .map(ConverterUtil::toSimpleItemDetailResponse).collect(Collectors.toList()), pageable,
        productItemPage.getTotalElements());
  }

  @Override
  public void migrateFinalImageFromGfsToGcs(List<String> productCodeList, String storeId) throws Exception {
    Set<String> locationPathSet =
        imageService.findProductAndItemLocationPathsByProductCodeListAndStoreId(productCodeList, storeId);
    if (CollectionUtils.isNotEmpty(locationPathSet)) {
      fileStorageService.migrateFinalImageFromGfsToGcs(locationPathSet);
    }
  }

  @Override
  public ProductItemCompleteResponse getItemResponseByItemCode(String storeId, String itemCode) {
    ProductItem productItem = this.productItemService.getProductItemByItemCode(storeId, itemCode);
    Product product =
        getCompleteProductDetailByProductIdAndMarkForDeleteFalse(storeId, productItem.getProductId());
    return ConverterUtil.toProductItemCompleteResponse(productItem, product);
  }

  private Product findDetailProduct(String storeId, String productCode) {
    try {
      return getCompleteProductDetailByProductCodeInAllProducts(storeId, productCode);
    } catch (Exception e) {
      log.error("Error when publish products by product code to aggregator with productCode: {}", productCode);
      return null;
    }
  }

  private void republishProductToAgp(Product product, boolean split) {
    if (split) {
      republishProductToAgpWithSplit(product);
    } else {
      republishProductToAgpWithoutSplit(product);
    }
  }

  private void republishProductToAgpWithSplit(Product product) {
    try {
      ProductDomainEventModel productDomainEventModel = domainEventPublisherService.toProductDomainEventModel(product, false,
        StringUtils.EMPTY);
      productDomainEventModel.setImages(null);
      productDomainEventModel.setProductItems(null);
      productDomainEventModel.setProductCategories(null);
      productDomainEventModel.setProductAttributes(null);
      domainEventPublisherService.republishProductToAgp(productDomainEventModel);

      publishImagesToAgp(product);
      publishProductItemsToAgp(product);
      publishProductCategoriesToAgp(product);
      publishProductAttributesToAgp(product);
    } catch (Exception e) {
      log.error("Error when republishProductToAgpWithSplit with productCode: {}", product.getProductCode());
    }
  }

  private void republishProductToAgpWithoutSplit(Product product) {
    try {
      ProductDomainEventModel productDomainEventModel = domainEventPublisherService.toProductDomainEventModel(product, false,
        StringUtils.EMPTY);
      domainEventPublisherService.republishProductToAgp(productDomainEventModel);
    } catch (Exception e) {
      log.error("Error when republishProductToAgpWithoutSplit with productCode: {}", product.getProductCode());
    }
  }

  @Override
  public CategorySummaryResponse updateProductCategoryByStoreIdAndProductCode(String storeId, String productCode,
      String categoryCode, boolean updateSalesCategory, boolean b2bSeller) throws Exception {
    Pair<Product, CategorySummaryResponse> categorySummaryResponseAndProductPair =
        productService.updateProductCategoryByStoreIdAndProductCode(storeId, productCode, categoryCode,b2bSeller);
    this.productService.evictProductCache(storeId, categorySummaryResponseAndProductPair.getLeft());
    this.applicationCacheServiceBean.evictProductCategoriesCacheByStoreIdAndProductId(storeId,
        categorySummaryResponseAndProductPair.getLeft().getId());
    ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory =
        categoryReferenceService.getSalesCategoryReferenceByMasterCategory(
            categorySummaryResponseAndProductPair.getRight().getOldCategoryId(),
            categorySummaryResponseAndProductPair.getRight().getNewCategoryId(), false);
    salesCategoryReferenceByMasterCategory.setNewCategoryBopisEligible(
        categorySummaryResponseAndProductPair.getRight().isNewCategoryBopisEligible());
    if (updateSalesCategory) {
      domainEventPublisherService.publishProductChangeCategory(categorySummaryResponseAndProductPair.getLeft(),
          salesCategoryReferenceByMasterCategory, false, false, true, false, new HashSet<>());
    } else {
      domainEventPublisherService.publishProductChangeCategory(categorySummaryResponseAndProductPair.getLeft(), null,
          false, false, true, true, new HashSet<>());
    }
    return categorySummaryResponseAndProductPair.getRight();
  }

  @Override
  public boolean updateProductDimensions(WarehouseMasterSKUEvent request) throws Exception {
    try {
      if (isProductDimensionInvalid(request))
        return false;
      MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
      ProductItem productItem =
          productItemService.findByStoreIdAndSkuCode(Constants.DEFAULT_STORE_ID, request.getItemCode());
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getItemCode()),
          ErrorMessage.ITEM_CODE_MUST_NOT_BE_BLANK.getMessage());
      Product newProduct = productService.updateProductDimensions(request, productItem.getProductId());
      this.productService.evictProductCache(Constants.DEFAULT_STORE_ID, newProduct);
      this.applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(Constants.DEFAULT_STORE_ID,
          newProduct.getId());
      productService.publishProduct(newProduct, false);
      return true;
    } catch (Exception e) {
      LOGGER.error("Error while updating product with itemCode: {}", request.getItemCode(), e);
      return false;
    }
  }

  private boolean isProductDimensionInvalid(WarehouseMasterSKUEvent request) {
    boolean isDimensionInvalid =
        ConverterUtil.validateDimension(request.getLength(), request.getWidth(), request.getWeight(),
            request.getHeight());
    if (isDimensionInvalid && validateDimensionSwitch) {
      log.error("Error processing SaveDGLevelAndDimensionsListener, invalid dimension for itemCode : {}",
          request.getItemCode());
      return true;
    }
    return false;
  }

  private void publishImagesToAgp(Product product) {
    Optional.ofNullable(product)
        .map(Product::getProductImages)
        .map(AggregateUtil::toAggregateImageDomainEventModel)
        .ifPresent(domainEventPublisherService::republishImageToAgp);
  }

  private void publishProductItemsToAgp(Product product) {
    Optional.ofNullable(product)
        .map(Product::getProductItems)
        .map(AggregateUtil::toAggregateProductItemDomainEventModel)
        .ifPresent(domainEventPublisherService::republishProductItemToAgp);
  }

  private void publishProductCategoriesToAgp(Product product) {
    Optional.ofNullable(product)
        .map(Product::getProductCategories)
        .map(AggregateUtil::toAggregateProductCategoryDomainEventModel)
        .ifPresent(domainEventPublisherService::republishProductCategoryToAgp);
  }

  private void publishProductAttributesToAgp(Product product) {
    Optional.ofNullable(product)
        .map(Product::getProductAttributes)
        .map(AggregateUtil::toAggregateProductAttributeDomainEventModel)
        .ifPresent(domainEventPublisherService::republishProductAttributeToAgp);
  }

  @Override
  public Product getCompleteProductDetailByProductCodeInAllProducts(String storeId, String productCode) {
    Product product = productService.getProductByStoreIdAndProductCodeCached(storeId, productCode);
    GdnPreconditions.checkArgument(Objects.nonNull(product),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productCode);
    productService.setCompleteProductDetailsCached(storeId, product, true);
    return product;
  }

  @Override
  public ProductMasterDataResponse getMasterProductDetailsByItemCode(String storeId, String itemCode) throws Exception {
    ProductItem productItem = productItemService.getProductItemBySkuCode(storeId, itemCode);
    GdnPreconditions.checkArgument(Objects.nonNull(productItem),
        ErrorMessage.PRODUCT_ITEMS_MUST_DEFINE.getMessage() + itemCode);
    Product product = productService.getProductByStoreIdAndProductIdCached(storeId, productItem.getProductId());
    GdnPreconditions.checkArgument(Objects.nonNull(product),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productItem.getProductId());
    productService.setProductImagesCached(storeId, product, true);
    productService.setProductCategoriesWithCategoriesCached(storeId, product, true);
    return ConverterUtil.toProductMasterDataResponse(product, productItem);
  }

  @Override
  public Product getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(
      String storeId, String productCode) {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    productService.setCompleteProductDetailsCached(storeId, product, false);
    return product;
  }

  @Override
  public Product getCompleteProductDetailByProductIdAndMarkForDeleteFalse(
      String storeId, String productId) {
    Product product = getProductByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, productId);
    productService.setCompleteProductDetailsCached(storeId, product, false);
    return product;
  }

  @Override
  public Product getCompleteProductDetailByProductId(
      String storeId, String productId) {
    Product product = getProductByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, productId);
    productService.setCompleteProductDetailsCached(storeId, product, true);
    return product;
  }

  @Override
  public Product getProductDetailsWithoutItemsByProductCodeAndMarkForDeleteFalse(String storeId, String productCode) {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    productService.setProductCategoriesWithCategoriesCached(storeId, product, false);
    productService.setProductAttributesWithValuesAndAttributeCached(storeId, product, false);
    productService.setProductImagesCached(storeId, product, false);
    return product;
  }

  private Product getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode) {
    Product product = productService.getProductByStoreIdAndProductCodeCached(storeId, productCode);
    GdnPreconditions.checkArgument(Objects.nonNull(product) && !product.isMarkForDelete(),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productCode);
    return product;
  }

  private Product getProductByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId) {
    Product product = productService.getProductByStoreIdAndProductIdCached(storeId, productId);
    GdnPreconditions.checkArgument(Objects.nonNull(product) && !product.isMarkForDelete(),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productId);
    return product;
  }

  @Override
  public void updateProductActivated(String storeId, String productCode, boolean activated) throws Exception {
    Product product = productService.updateProductActivated(storeId, productCode, activated);
    this.productService.evictProductCache(storeId, product);
    this.applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(storeId, product.getId());
    productService.publishProduct(product, false);
  }

  @Override
  public void activateProduct(String storeId, String id) throws Exception {
    productService.activateProduct(storeId, id);
    Product product = getCompleteProductDetailByProductIdAndMarkForDeleteFalse(storeId, id);
    productService.publishProduct(product, false);
  }

  @Override
  public void deactivateProduct(String storeId, String id) throws Exception {
    productService.deactivateProduct(storeId, id);
    Product product = getCompleteProductDetailByProductIdAndMarkForDeleteFalse(storeId, id);
    productService.publishProduct(product, false);
  }

  @Override
  public void updateProductViewable(String storeId, String productCode, boolean viewable) throws Exception {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    boolean existingActivated = product.isActivated();
    boolean existingViewable = product.isViewable();
    product.setViewable(viewable);
    productService.updateProductViewable(storeId, productCode, viewable);
    productService.evictProductCache(storeId, product);
    applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(storeId, product.getId());
    productService.setCompleteProductDetailsCached(storeId, product, false);
    domainEventPublisherService.publishProductChangeCategory(product, null, false, true, true, false, new HashSet<>());
    if (existingActivated && !existingViewable && viewable) {
      this.domainEventPublisherService.publishEvent(product, ProductMasterEvent.ACTIVATED);
    }
  }

  @Override
  public void updateProductImage(Product product) throws Exception {
    ProductPublishUpdateDTO savedProduct = productService.updateProductImage(product);
    productService.evictProductCache(savedProduct.getProduct().getStoreId(), savedProduct.getProduct());
    applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(
        savedProduct.getProduct().getStoreId(), savedProduct.getProduct().getId());
    productService.evictProductItemsAndProductItemImagesCache(savedProduct.getProduct().getStoreId(),
        savedProduct.getProduct());
    Product updatedProduct =
        getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(product.getStoreId(), product.getProductCode());
    savedProduct.setProduct(updatedProduct);
    domainEventPublisherService.publishProductChangeCategory(savedProduct, null, false, false, false, false, false,
        new HashSet<>(), false, false, new HashSet<>());
  }

  @Override
  @Async
  public void updateProductScore(String storeId, Pageable pageable, boolean productScoreUpdateSwitch,
      int productScoreBatchSize) {
    if (productScoreUpdateSwitch) {
      long startTime = Calendar.getInstance().getTimeInMillis();
      Page<String> productsPage;
      int count = 0;
      try {
        do {
          productsPage = this.productScoreService.findByMarkForDeleteFalseAndUpdatedFalse(pageable);
          if (CollectionUtils.isEmpty(productsPage.getContent())) {
            break;
          }
          List<String> products = productsPage.getContent();
          count = count + products.size();
          this.domainEventPublisherService.publishProductScoreUpdate(products);
          productScoreService.updateProducts(products);
          Thread.sleep(2000);
        } while (count < productScoreBatchSize);
        long endtime = Calendar.getInstance().getTimeInMillis();
        log.info(
            "Update product score successful. Total number of products score updated : {} and time taken in milliseconds: {}",
            count, endtime - startTime);
      } catch (Exception e) {
        log.error(
            ErrorMessage.PRODUCT_SCORE_SCHEDULER_ERROR + "Total number of products with successful score updated : {} ",
            count, e);
      }
    }
  }

  @Override
  public GdnRestListResponse<LocationPathAndCommonImage> updateProductItemImagesByProductCode(ProductItemImageUpdateRequest productItemImageUpdateRequest,
      String storeId, String requestId) {
    try {
      Pair<ProductPublishUpdateDTO, List<LocationPathAndCommonImage>> productAndImageDetailPair =
        productService.updateProductItemImagesByProductCode(productItemImageUpdateRequest, storeId);
      Product product = getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(storeId,
          productItemImageUpdateRequest.getProductCode());
      productService.evictCompleteProductAndItemsCache(storeId, product);
      ProductPublishUpdateDTO productPublishUpdateDTO = productAndImageDetailPair.getKey();
      productPublishUpdateDTO.setProduct(product);
      domainEventPublisherService.publishProductChangeCategory(productPublishUpdateDTO, null, false, false,
          true, false, false, new HashSet<>(), true, true, new HashSet<>());
      return new GdnRestListResponse<>(null, null, true, productAndImageDetailPair.getValue(),
        new PageMetaData(productAndImageDetailPair.getValue().size(), 0,
          productAndImageDetailPair.getValue().size()), requestId);
    } catch (Exception e) {
      LOGGER.error("Error while updating location paths in PCB, productCode :{} ",
          productItemImageUpdateRequest.getProductCode(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, null, null,
          requestId);
    }
  }

  @Override
  public boolean deleteOriginalImagesByProductCode(String storeId, String productCode) {
    try {
      Product product = getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(storeId, productCode);
      product.getProductImages().forEach(this::deleteOriginalProductImage);
      product.getProductItems().forEach(productItem -> {
        productItem.getProductItemImages().forEach(this::deleteOriginalProductItemImage);
      });
      product.setReviewPending(false);
      productService.update(product);
      productService.evictAllProductDetailCache(storeId, product);
      domainEventPublisherService.publishProductChangeCategory(product, null, false, false, false, false, new HashSet<>());
      return true;
    } catch (Exception e) {
      LOGGER.error("Error while deleting original images from PCB, productCode : {} ", productCode, e);
      return false;
    }
  }

  private void deleteOriginalProductImage(ProductImage productImage) {
    if (Objects.nonNull(productImage) && Boolean.TRUE.equals(productImage.getOriginalImage())) {
      productImage.setMarkForDelete(true);
    }
  }

  private void deleteOriginalProductItemImage(ProductItemImage productItemImage) {
    if (Objects.nonNull(productItemImage) && Boolean.TRUE.equals(productItemImage.getOriginalImage())) {
      productItemImage.setMarkForDelete(true);
    }
  }

  @Override
  public List<BatchVatUpdateResponse> updateVatFlagBySkuCodes(String storeId, String requestId, String username,
      BatchVatUpdateRequest batchVatUpdateRequest) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(requestId),
        ErrorMessage.REQUEST_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(batchVatUpdateRequest.getBusinessPartnerCode()),
        ErrorMessage.MERCHANT_CODE_ERROR.getMessage());
    List<BatchVatUpdateResponse> batchVatUpdateResponseList = new ArrayList<>();
    batchVatUpdateResponseList.addAll(batchVatUpdateRequest.getItemCodeToVatMap().entrySet().stream()
        .filter(entry -> Objects.isNull(entry.getValue()))
        .map(entry -> addBatchVatUpdateResponseForNull(entry.getKey())).collect(Collectors.toList()));
    List<String> skuCodesForUpdate = batchVatUpdateRequest.getItemCodeToVatMap().entrySet().stream()
        .filter(entry -> Objects.nonNull(entry.getValue())).map(Map.Entry::getKey).collect(Collectors.toList());
    fetchAndUpdateVatFlagToItem(storeId, requestId, username, batchVatUpdateRequest, skuCodesForUpdate,
        batchVatUpdateResponseList);
    return batchVatUpdateResponseList;
  }

  private void fetchAndUpdateVatFlagToItem(String storeId, String requestId, String username,
      BatchVatUpdateRequest batchVatUpdateRequest, List<String> skuCodesAfterMerchantValidation,
      List<BatchVatUpdateResponse> batchVatUpdateResponseList) {
    List<ProductItem> productItemList =
        this.productItemService.findItemsBySkuCodesAndMarkForDeleteFalse(storeId, skuCodesAfterMerchantValidation);
    for (ProductItem productItem : productItemList) {
      if (batchVatUpdateRequest.getItemCodeToVatMap().containsKey(productItem.getSkuCode())) {
        if (!Objects.equals(productItem.getVatApplicable(),
            batchVatUpdateRequest.getItemCodeToVatMap().get(productItem.getSkuCode()))) {
          productItem.setVatApplicable(batchVatUpdateRequest.getItemCodeToVatMap().get(productItem.getSkuCode()));
          this.productItemService.saveProductItem(productItem);
          this.applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(productItem.getStoreId(),
              productItem.getProductId());
          this.domainEventPublisherService.publishVatApplicableUpdateEvent(productItem.getSkuCode(),
              productItem.getVatApplicable());
          this.domainEventPublisherService.publishVatApplicableExternalHistoryEvent(requestId, storeId,
              productItem.getId(), productItem.getSkuCode(), productItem.getGeneratedItemName(), username,
              String.valueOf(!productItem.getVatApplicable()), String.valueOf(productItem.getVatApplicable()));
        }
      }
    }
  }

  private BatchVatUpdateResponse addBatchVatUpdateResponseForNull(String key) {
    return BatchVatUpdateResponse.builder().errorMessage(ErrorMessage.VAT_FLAG_TO_UPDATE_NULL.getMessage())
        .vatFlagRequest(NULL_VALUE_ON_REQUEST_MAP).skuCode(key).build();
  }

  @Override
  public Map<String, String> updateImages(String storeId, ProductImageEditRequest productImageEditRequest)
      throws Exception {
    Pair<ProductPublishUpdateDTO, Map<String, Map<String, String>>> errorMapAndProductPair =
        productService.updateImages(storeId, true, Arrays.asList(productImageEditRequest), null,
          false);
    log.info("Images updated for request : {}, errors : {}", productImageEditRequest, errorMapAndProductPair);
    this.productService.evictProductCache(storeId, errorMapAndProductPair.getLeft().getProduct());
    this.applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(storeId,
        errorMapAndProductPair.getLeft().getProduct().getId());
    this.productService.evictProductItemsAndProductItemImagesCache(storeId, errorMapAndProductPair.getLeft().getProduct());
    domainEventPublisherService.publishProductChangeCategory(errorMapAndProductPair.getLeft(), null, false,
        false, true, false, false, new HashSet<>(), true, true, new HashSet<>());
    return errorMapAndProductPair.getRight().get(productImageEditRequest.getImagePath());
  }

  @Override
  public void backfillCommonImageFlagInProductAndItemImages(String storeId, String productCode,
    String migrationType) {
    ProductMigration commonImageMigration = null;
    Product product;
    try {
      if(migrationType.equals(ProductMigrationType.UPC_MIGRATION.name())){
        commonImageMigration = productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(productCode,
          ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.UPC_MIGRATION.name());
        commonImageMigration =
          updateProductMigration(commonImageMigration, ProductMigrationStatus.IN_PROGRESS.name());
        product = this.getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
        productItemServiceWrapper.setProductItemsCached(storeId, product, false);
      }
      else {
        commonImageMigration = productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(productCode,
          ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.COMMON_IMAGE_MIGRATION.name());
        commonImageMigration =
          updateProductMigration(commonImageMigration, ProductMigrationStatus.IN_PROGRESS.name());
        product = this.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(storeId, productCode);
        ProductImageUtil.setCommonImageFlagForProductAndItemImages(product, true);
        productService.update(product);
        productService.evictAllProductDetailCacheByProductCode(storeId, productCode);
      }
      commonImageMigration = updateProductMigration(commonImageMigration, ProductMigrationStatus.SUCCESS.name());
      domainEventPublisherService.publishProductForMasterDataMigration(product, migrationType);
    } catch (Exception e) {
      LOGGER.error("Error while back filling product and item common images for productCode : {} ", productCode, e);
      updateProductMigration(commonImageMigration, ProductMigrationStatus.FAILED.name());
    }
  }

  private ProductMigration updateProductMigration(ProductMigration productMigration, String status) {
    productMigration.setStatus(status);
    return productMigrationService.saveProductMigration(productMigration);
  }

  @Override
  public Map<String, Map<String, String>> updateCommonImages(String storeId,
      List<ProductImageEditRequest> productImageEditRequestList) throws Exception {
    Pair<ProductPublishUpdateDTO, Map<String, Map<String, String>>> errorMapAndUpdatedProductPair =
        productService.updateImages(storeId, false, productImageEditRequestList, null, false);
    Product product = getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(storeId,
        errorMapAndUpdatedProductPair.getLeft().getProduct().getProductCode());
    this.productService.evictProductCache(storeId, errorMapAndUpdatedProductPair.getLeft().getProduct());
    this.applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(storeId,
        errorMapAndUpdatedProductPair.getLeft().getProduct().getId());
    this.productService.evictProductItemsAndProductItemImagesCache(storeId, errorMapAndUpdatedProductPair.getLeft().getProduct());
    errorMapAndUpdatedProductPair.getLeft().setProduct(product);
    domainEventPublisherService.publishProductChangeCategory(errorMapAndUpdatedProductPair.getLeft(), null, false,
        false, true, false, false, new HashSet<>(), true, true, new HashSet<>());
    return errorMapAndUpdatedProductPair.getRight();
  }

  @Override
  public Map<String, Map<String, String>> updateCommonImagesAndPublishHistory(String storeId,
      String username, ProductMasterDataUpdateRequest productMasterDataUpdateRequest)
      throws Exception {
    Map<String, Map<String, String>> imageErrorMap =
        updateCommonImages(storeId, productMasterDataUpdateRequest.getCommonImages());
    List<ProductImageEditRequest> successRequest = new ArrayList<>();
    for (ProductImageEditRequest pcbRequest : productMasterDataUpdateRequest.getCommonImages()) {
      String imageResponse =
          imageErrorMap.get(pcbRequest.getImagePath()).get(Constants.COPY_ALL_STATUS);
      if (Constants.SUCCESS.equalsIgnoreCase(imageResponse)) {
        successRequest.add(pcbRequest);
      } else {
        log.error("Error while updating common image : {} ", pcbRequest);
      }
    }
    if (CollectionUtils.isNotEmpty(successRequest)) {
      AuditTrailListResponse auditTrailListResponse =
          createAuditLogs(username, successRequest, productMasterDataUpdateRequest);
      publishExternalHistory(auditTrailListResponse, productMasterDataUpdateRequest);
    }
    return imageErrorMap;
  }

  private AuditTrailListResponse createAuditLogs(String username,
      List<ProductImageEditRequest> productImageEditRequests,
      ProductMasterDataUpdateRequest productMasterDataUpdateRequest) {
    AuditTrailListResponse auditTrailListResponse = null;
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    for (ProductImageEditRequest productImageEditRequest : productImageEditRequests) {
      if (productImageEditRequest.getCopyToAllVariantImages().isAdd()) {
        AuditTrailDto auditTrailDto =
            CommonUtil.getAuditTrailDto(productImageEditRequest.getImagePath(), StringUtils.EMPTY,
                productMasterDataUpdateRequest, Constants.UPDATE_PRODUCT_ACTIVITY_ADD_IMAGE);
        auditTrailDtoList.add(auditTrailDto);
      }
      if (productImageEditRequest.getCopyToAllVariantImages().isMarkForDelete()) {
        AuditTrailDto auditTrailDto =
            CommonUtil.getAuditTrailDto(productImageEditRequest.getImagePath(), StringUtils.EMPTY,
                productMasterDataUpdateRequest, Constants.UPDATE_PRODUCT_ACTIVITY_DELETE_IMAGE);
        auditTrailDtoList.add(auditTrailDto);
      }
      if (productImageEditRequest.getCopyToAllVariantImages().isMainImage()) {
        AuditTrailDto auditTrailDto = CommonUtil.getAuditTrailDto(
            (Constants.MAIN_IMAGE + Constants.COLON + Boolean.TRUE + Constants.COLON
                + productImageEditRequest.getImagePath()),
            (Constants.MAIN_IMAGE + Constants.COLON + Boolean.FALSE + Constants.COLON
                + productImageEditRequest.getImagePath()), productMasterDataUpdateRequest,
            Constants.UPDATE_PRODUCT_ACTIVITY_MAIN_IMAGE_UPDATED);
        auditTrailDtoList.add(auditTrailDto);
      }
    }
    if (CollectionUtils.isNotEmpty(auditTrailDtoList)) {
      auditTrailListResponse =
          getAuditTrailListResponse(username, auditTrailDtoList, auditTrailListResponse);
    }
    return auditTrailListResponse;
  }

  private AuditTrailListResponse getAuditTrailListResponse(String username,
      List<AuditTrailDto> auditTrailDtoList, AuditTrailListResponse auditTrailListResponse) {
    auditTrailListResponse = new AuditTrailListResponse();
    auditTrailListResponse.setAccessChannel(Constants.CLIENT_ID);
    auditTrailListResponse.setClientId(Constants.CLIENT_ID);
    auditTrailListResponse.setRequestId(UUID.randomUUID().toString());
    auditTrailListResponse.setUpdateDirectly(true);
    auditTrailListResponse.setUpdateDirectlyToDB(true);
    auditTrailListResponse.setChangedBy(username);
    auditTrailListResponse.setAuditTrailResponseList(auditTrailDtoList);
    return auditTrailListResponse;
  }

  public void publishExternalHistory(AuditTrailListResponse auditTrailListResponse,
      ProductMasterDataUpdateRequest productMasterDataUpdateRequest) {
    if (Objects.nonNull(auditTrailListResponse)) {
      log.info("Publishing external history event for product code: {} with payload: {}",
          productMasterDataUpdateRequest.getProductCode(), auditTrailListResponse);
      kafkaPublisher.send(kafkaTopicProperties.getProductSkuUpdateExternalHistoryEvent(),
          productMasterDataUpdateRequest.getProductSku(), auditTrailListResponse);
    }
  }

  @Override
  public List<ImageResponse> findProductImagesByProductCode(String storeId, String productCode, boolean removeOriginalImages){
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    productService.setProductImagesCached(storeId, product, false);
    return ConverterUtil.convertProductImageToImageResponse(product.getProductImages(), removeOriginalImages);
  }

  @Override
  public void updateProductContent(Product product, boolean publishProductEvent,
      boolean ignoreSalesCategoryPublish, boolean updateFromVendor) throws Exception {
    Product savedProduct = productService.getProductByStoreIdAndProductCodeCached(
        product.getStoreId(), product.getProductCode());
    log.info("#updateProductContent ProductCode : {}, requestVersion: {}, savedVersion: {}",
        product.getProductCode(), product.getVersion(), savedProduct.getVersion());
    if (product.getVersion() < savedProduct.getVersion()) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "Product Code : " + product.getProductCode()
          + ". Error Code : Product content expired");
    }
    productService.setCompleteProductDetailsCached(savedProduct.getStoreId(), savedProduct, true);
    boolean productDetailChanged = ProductUtil.isProductDetailChanged(savedProduct, product);
    boolean isBrandChanged = !StringUtils.equals(savedProduct.getBrand(), product.getBrand());
    boolean productLevelContentUpdated = CommonUtil.contentUpdateAtProductLevel(product, savedProduct);
    Set<String> updatedItemSkuCodes =
        CommonUtil.skuCodesOfUpdatedItems(savedProduct.getProductItems(), product.getProductItems(), false).getRight();
    Set<String> updatedFields = CommonUtil.getUpdatedFields(savedProduct, product);
    BeanUtils.copyProperties(product, savedProduct, "createdMerchant", "productAttributes",
        "productCategories", "productImages", "productItems", "version", "createdBy", "createdDate",
        "video", "distributionInfo", "aiGeneratedFields");

    if (StringUtils.isNotBlank(product.getDistributionInfo())) {
      savedProduct.setDistributionInfo(product.getDistributionInfo());
    }

    CategoryChangeDTO categoryChangeDTO = productService.regenerateProductCategories(savedProduct, product);
    ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory = null;
    boolean categoryUpdate = false;
    if (Objects.nonNull(categoryChangeDTO) && (Objects.nonNull(categoryChangeDTO.getNewCategoryId())) && (Objects
        .nonNull(categoryChangeDTO.getOldCategoryId()))) {
      salesCategoryReferenceByMasterCategory = categoryReferenceService
          .getSalesCategoryReferenceByMasterCategory(categoryChangeDTO.getOldCategoryId(),
              categoryChangeDTO.getNewCategoryId(), false);
      categoryUpdate = true;
    }
    productService.regenerateProductAttributeContent(savedProduct, product, updateFromVendor, categoryUpdate);
    productService.regenerateProductItemContent(savedProduct, product);
    productService.regenerateProductImageContent(savedProduct, product);
    savedProduct.getProductItems().forEach(productItem -> {
      productItem.setActivated(product.isActivated());
      productItem.setViewable(product.isViewable());
    });
    productService.saveAndFlush(savedProduct);
    if (productDetailChanged) {
      productAttributeExtractionService.addProductsToProductAttributeExtraction(product.getProductCode(),
          product.getProductCategories().get(0).getCategory());
    }
    productService.evictAllProductDetailCache(savedProduct.getStoreId(), savedProduct);
    if (publishProductEvent) {
      domainEventPublisherService.publishProductChangeCategory(
          new ProductPublishUpdateDTO(savedProduct, productLevelContentUpdated, updatedItemSkuCodes),
          salesCategoryReferenceByMasterCategory, isBrandChanged, false, false, false, false, new HashSet<>(), false,
          false, updatedFields);
    }
  }


  @Override
  public void updateAndMarkForNeedRevision(Product product) throws Exception {
    UpdateNeedRevisionDTO updateNeedRevisionDTO =
      productService.updateAndMarkForNeedRevision(product);
    log.info("#updateAndMarkForNeedRevision ProductCode : {}, requestVersion: {}, savedVersion: {}",
      product.getProductCode(), product.getVersion(), updateNeedRevisionDTO.getProduct().getVersion());
    productService.evictCacheOnMarkForNeedRevision(updateNeedRevisionDTO.getProduct(),
      updateNeedRevisionDTO.isBrandChanged(),
      updateNeedRevisionDTO.getProductSalesCategoryMapping());
  }

  @Override
  public Product getProductAndAttributeDetailsByProductCode(String storeId, String productCode, boolean inAllProducts) {
    Product product;
    if(inAllProducts){
       product = productService.getProductByStoreIdAndProductCodeCached(storeId, productCode);
    }else {
       product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    }
    productService.setProductAttributesWithValuesAndAttributeCached(storeId, product, false);
    return product;
  }

  @Override
  public List<ProductAttribute> getProductAttributeDetailsByProductId(String storeId, String productId) {
    return productService.getProductAttributes(storeId, productId);
  }

  @Override
  public void migrateImagesFromGfsToGcsForProducts(String storeId, String productCode) {
    ProductMigration productMigration =
        productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(productCode,
            ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name());
    if (Objects.nonNull(productMigration)) {
      try {
        productMigration = updateProductMigration(productMigration, ProductMigrationStatus.IN_PROGRESS.name());
        Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
        productService.setProductAndItemImagesCached(storeId, product, false);
        Set<Pair<String, String>> imagesToBeMigrated = migrateImagesFromGfsToGcs(product);
        if (CollectionUtils.isNotEmpty(imagesToBeMigrated)) {
          fileStorageService.migrateImagesFromGfsToGcs(productCode, imagesToBeMigrated);
          productService.saveAndFlush(product);
          productService.evictProductAndItemImageCache(product);
          domainEventPublisherService.publishProductChangeCategory(product, null, false, true, true, false, new HashSet<>());
          domainEventPublisherService.publishImagePathUpdateEvent(storeId, productCode, imagesToBeMigrated);
        }
        productMigration = updateProductMigration(productMigration, ProductMigrationStatus.SUCCESS.name());
      } catch (Exception e) {
        log.info("Exception while migration imgaes from gfs to gcs for product : {} ", productCode, e);
        productMigration = updateProductMigration(productMigration, ProductMigrationStatus.FAILED.name());
      }
    }
  }

  @Override
  public void migrateFinalImagesFromGfsToGcsForProducts(String storeId, String productCode) {
    ProductMigration productMigration =
        productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(productCode,
            ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name());
    if (Objects.nonNull(productMigration)) {
      try {
        productMigration = updateProductMigration(productMigration, ProductMigrationStatus.IN_PROGRESS.name());
        migrateFinalImageFromGfsToGcs(Arrays.asList(productCode), storeId);
        productMigration = updateProductMigration(productMigration, ProductMigrationStatus.SUCCESS.name());
      } catch (Exception e) {
        log.info("Exception while migration imgaes from gfs to gcs for product : {} ", productCode, e);
        productMigration = updateProductMigration(productMigration, ProductMigrationStatus.FAILED.name());
      }
    }
  }

  @Override
  public void updateFlagsOnNeedCorrectionAndEvictCache(String storeId, String productCode,
    NeedRevisionConfigRequest request) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
      ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode),
      ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
    Product product = productService.updateFlagsOnNeedCorrection(storeId, productCode, request);
    productService.evictProductCache(storeId, product);
  }

  @Override
  public void updateProductReviewPendingAndEvictCache(String storeId, String productCode,
    boolean reviewPending) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
      ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode),
      ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
    Product product =
      productService.updateProductReviewPending(storeId, productCode, reviewPending);
    productService.evictProductCache(storeId, product);
  }

  @Override
  public Product regenerateProductAndEvictCache(String storeId, Product savedProduct, Product product, Boolean pristineCategory, boolean onlyVatChanged, boolean scoreUpdated,
      boolean productDetailChanged, boolean computeCommonImage, boolean resetExtractedAttributeValue, boolean ignoreSalesCategoryPublish,
      ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO, Set<String> updatedFields)
    throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
      ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(product.getProductCode()),
      ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_EMPTY.getMessage());
    Pair<Map<String, ProductDTO>, Map<ProductItem, String>> productAndProductItemMap =
      productService.regenerateProductItem(storeId, savedProduct, product, pristineCategory,
        onlyVatChanged, scoreUpdated, productDetailChanged, computeCommonImage,
        resetExtractedAttributeValue, false);
    ProductDTO oldProductDTO = productAndProductItemMap.getKey().get(Constants.OLD_PRODUCT);
    Product oldProduct = ConverterUtil.convertProductDTOToProduct(oldProductDTO);
    oldProduct.getProductItems().forEach(productItem -> productItem.setProduct(oldProduct));
    Map<String, ProductItemImage> productItemImageMap =
      oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
        .flatMap(List::stream).collect(Collectors.toMap(ProductItemImage::getProductItemId, Function.identity(), (oldId, newId) -> oldId));
    mergeProductItemImages(oldProduct, productItemImageMap);
    Product newProduct =
      ConverterUtil.convertProductDTOToProduct(productAndProductItemMap.getKey().get(Constants.NEW_PRODUCT));
    productService.evictCacheAndPublishForProductItemUpdate(storeId, oldProduct, newProduct
      , pristineCategory, onlyVatChanged, scoreUpdated,
      convertItemWithOldVatValueMap(productAndProductItemMap.getValue()),
      ignoreSalesCategoryPublish, productAndItemLevelUpdatesDTO, updatedFields, oldProductDTO.getDeletedItems());
    return ConverterUtil.convertProductDTOToProduct(productAndProductItemMap.getKey().get(Constants.OLD_PRODUCT));
  }

  private void mergeProductItemImages(Product oldProduct,
    Map<String, ProductItemImage> productItemImageMap) {
    oldProduct.getProductItems().stream().map(ProductItem::getProductItemImages)
      .flatMap(List::stream).forEach(productItemImage -> {
        ProductItem productItem =
          productItemImageMap.get(productItemImage.getProductItemId()).getProductItem();
        productItemImage.setProductItem(productItem);
      });
  }

  private Map<ProductItem, String> convertItemWithOldVatValueMap(Map<ProductItem, String> itemWithOldVatValueMapDTO) {
    return itemWithOldVatValueMapDTO.entrySet()
      .stream()
      .collect(Collectors.toMap(Map.Entry::getKey,
        Map.Entry::getValue
      ));
  }

  @Override
  public void saveProductItemDetails(String storeId, List<ProductItem> productItemFromRequest, String productCode)
      throws Exception {
    Product product = productItemService.saveProductItemDetails(storeId, productItemFromRequest, productCode);
    if (Objects.nonNull(product)) {
      productService.evictProductCache(storeId, product);
      applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(storeId, product.getId());
      applicationCacheServiceBean.evictProductItemImagesCacheByStoreIdAndProductId(storeId, product.getId());
      applicationCacheServiceBean.evictProductItemAttributeValuesCacheByStoreIdAndProductId(storeId, product.getId());
    }
  }

  @Override
  public void markForDeleteProductAndEvictCache(String storeId, String productId) throws Exception {
    Pair<Product, Set<String>> productSetPair = productService.markForDeleteProduct(storeId, productId);
    this.productService.evictAllProductDetailCache(storeId, productSetPair.getLeft());
    this.domainEventPublisherService.publishProduct(productSetPair.getLeft());
    for (ProductImage productImage : productSetPair.getLeft().getProductImages()) {
      if (Boolean.TRUE.equals(productImage.getOriginalImage())) {
        productSetPair.getRight().add(productImage.getLocationPath());
      }
    }
    if (!productSetPair.getLeft().isEdited()) {
      this.productService.deleteImageByImageLocations(productSetPair.getRight());
    }
  }

  @Override
  public EditProductItemAndImageResponse updateProductMasterDataAndImagesAndUpcCode(
    Product newProduct, EditProductDetailRequest editProductDetailRequest, String productCode,
    String storeId, List<NewlySavedItemResponse> newlySavedItemResponseList) throws Exception {

    ProductDetailEditDTO productDetailEditDTO =
      productService.updateProductContentAndImages(storeId, newProduct, editProductDetailRequest, newlySavedItemResponseList);

    Product finalProduct = productDetailEditDTO.getFinalProduct();
    setExtractedProductAttributes(newProduct, editProductDetailRequest, productCode, storeId, productDetailEditDTO);

    productService.evictCacheAndPublishForProductItemUpdate(storeId, finalProduct, newProduct, editProductDetailRequest.getProductRequest().getPristineCategory(),
      false, editProductDetailRequest.getProductRequest().isScoreUpdated(), convertItemWithOldVatValueMap(productDetailEditDTO.
          getProductAndProductItemMap().getValue()), false, productDetailEditDTO.getProductAndItemLevelUpdatesDTO(),
        new HashSet<>(), new HashSet<>());

    return new EditProductItemAndImageResponse(newlySavedItemResponseList,
      productDetailEditDTO.getProductImageMapPair().getValue());
  }


  @Transactional(readOnly = false)
  public void setExtractedProductAttributes(Product newProduct,
    EditProductDetailRequest editProductDetailRequest,
    String productCode, String storeId, ProductDetailEditDTO productDetailEditDTO) {
    ProductAttributeExtracted productAttributeExtracted = null;
    if (productDetailEditDTO.isProductDetailsChanged()) {
      productAttributeExtracted =
        productAttributeExtractionService.fetchAndSetProductAttributeExtraction(productCode,
          newProduct.getProductCategories().stream().findFirst().orElse(new ProductCategory())
            .getCategory(), storeId);
      editProductDetailRequest.setResetExtractedAttributeValue(false);
    }
    if (Objects.nonNull(productAttributeExtracted)) {
      productAttributeExtractionService.saveProductToAttributeExtraction(productAttributeExtracted);
    }
  }


  private Set<Pair<String, String>> migrateImagesFromGfsToGcs(Product product) {
    Set<Pair<String, String>> imagesToBeMigrated = new HashSet<>();

    for (ProductImage productImage : product.getProductImages()) {
      if (isImageNeedsToBeMigrated(productImage.isMarkForDelete(), productImage.isActive(),
          productImage.getLocationPath())) {
        String gcsPath = fileStorageService.getGcsPathWithPrefix(productImage.getLocationPath());
        imagesToBeMigrated.add(Pair.of(productImage.getLocationPath(), gcsPath));
        productImage.setLocationPath(gcsPath);
      }
    }

    for (ProductItem productItem : product.getProductItems()) {
      for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
        if (isImageNeedsToBeMigrated(productItemImage.isMarkForDelete(), productItemImage.isActive(),
            productItemImage.getLocationPath())) {
          String gcsPath = fileStorageService.getGcsPathWithPrefix(productItemImage.getLocationPath());
          imagesToBeMigrated.add(Pair.of(productItemImage.getLocationPath(), gcsPath));
          productItemImage.setLocationPath(gcsPath);
        }
      }
    }

    return imagesToBeMigrated;
  }

  private boolean isImageNeedsToBeMigrated(boolean markForDelete, boolean isActive, String locationPath) {
    return !markForDelete && !isActive && !fileStorageService.isAlreadyGcsImage(locationPath);
  }


  @Override
  public List<AttributeHistoryResponse> autoFillAttributes(String storeId, String productCode) throws Exception {
    List<AttributeHistoryResponse> attributeHistoryResponseList = new ArrayList<>();
    Product savedProduct = productService.getProductByStoreIdAndProductCodeCached(storeId, productCode);
    productService.setCompleteProductDetailsCached(savedProduct.getStoreId(), savedProduct, false);
    List<CategoryAttribute> categoryAttributes =
        categoryService.getCategoryAttributes(storeId, savedProduct.getProductCategories().get(0).getCategoryId());
    savedProduct.getProductCategories().get(0).getCategory().setCategoryAttributes(categoryAttributes);
    List<ProductAttribute> productAttributeListEligibleForAutoFill =
        ProductUtil.getProductAttributesEligibleForAutoFill(savedProduct);
    List<ProductAttribute> newProductAttributeListEligibleForAutoFill =
        ProductUtil.getCategoryNewAttributesEligibleForAutoFill(savedProduct);
    if (CollectionUtils.isNotEmpty(productAttributeListEligibleForAutoFill) || CollectionUtils.isNotEmpty(
        newProductAttributeListEligibleForAutoFill)) {
      Map<String, String> attributeNameAndValueMap = extractAttributeNameAndValuesFromMatrixSystem(savedProduct);
      if (MapUtils.isNotEmpty(attributeNameAndValueMap)) {
        attributeHistoryResponseList = ProductUtil.extractAndSetValue(savedProduct, productAttributeListEligibleForAutoFill,
            newProductAttributeListEligibleForAutoFill, attributeNameAndValueMap);
        if (CollectionUtils.isNotEmpty(attributeHistoryResponseList)) {
          productService.saveAndFlush(savedProduct);
          productService.evictAllProductDetailCache(savedProduct.getStoreId(), savedProduct);
          domainEventPublisherService.publishProduct(savedProduct);
        }
      }
    }
    return attributeHistoryResponseList;
  }

  private Map<String, String> extractAttributeNameAndValuesFromMatrixSystem(Product savedProduct) {
    Map<String, String> attributeNameAndValueMap = new HashMap<>();
    SingleBaseResponse<MatrixAttributeExtractionResponse> matrixAttributeExtractionResponse =
        matrixOutbound.extractProductAttributesByTextDetails(
            ConverterUtil.toMatrixAttributeExtractionRequest(savedProduct));
    if (ProductUtil.isAttributeExtractionResponseValid(matrixAttributeExtractionResponse)) {
      for (Map.Entry<String, Object> entry : matrixAttributeExtractionResponse.getValue().getExtractedAttributes()
          .entrySet()) {
        if (ProductUtil.isExtractedValueValid(entry)) {
          attributeNameAndValueMap.put(entry.getKey(), String.valueOf(entry.getValue()));
        }
      }
    }
    return attributeNameAndValueMap;
  }


  @Override
  public void validateAndProcessProductDsAttributeMapping(
      ProductSuitabilityEventModel productSuitabilityEventModel)
      throws Exception {
    String storeId = productSuitabilityEventModel.getStoreId();
    Product product = productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(storeId,
        productSuitabilityEventModel.getProductId());
    productService.setCompleteProductDetailsCached(storeId, product, true);
    if (Objects.isNull(product)) {
      log.error("Product not found with product id : {} ", productSuitabilityEventModel.getProductId());
      return;
    }
    ProductCategory productCategory =
        Optional.ofNullable(product.getProductCategories()).orElse(new ArrayList<>()).stream()
            .filter(Predicate.not(ProductCategory::isMarkForDelete)).findFirst().orElse(null);
    if (Objects.isNull(productCategory)) {
      log.error("Category not found for product id : {} ", productSuitabilityEventModel.getProductId());
      return;
    }
    Map<String, List<ProductSuitabilityAttributeModel>> attributeCodeAndAttributeModelMap =
        productSuitabilityEventModel.getAttributeValueExtractions().stream().collect(
            Collectors.groupingBy(ProductSuitabilityAttributeModel::getAttributeCode,
                Collectors.toCollection(ArrayList::new)));
    List<Attribute> attributes = attributeService.findAttributesByStoreIdAndAttributeCodeList(storeId,
        new ArrayList<>(attributeCodeAndAttributeModelMap.keySet()));
    // Filter out attributes which are variant creation true or special attribute
    attributes =
        attributes.stream().filter(Predicate.not(Attribute::isVariantCreation)).filter(Attribute::isDsExtraction)
            .filter(Predicate.not(Attribute::isSkuValue)).collect(Collectors.toCollection(ArrayList::new));
    Map<String, Attribute> attributeCodeToAttributeMap =
        attributes.stream().collect(Collectors.toMap(Attribute::getAttributeCode, Function.identity()));
    Set<String> validAttributeCodes =
        attributes.stream().map(Attribute::getAttributeCode).collect(Collectors.toCollection(HashSet::new));
    attributeCodeAndAttributeModelMap.keySet().retainAll(validAttributeCodes);
    // Filter out attributes which does not belong to the product category
    List<String> attributesMappedToCategory = getCategoryAttributeCodes(storeId, productCategory);
    validAttributeCodes.retainAll(attributesMappedToCategory);
    attributeCodeAndAttributeModelMap.keySet().retainAll(validAttributeCodes);
    List<ProductAttribute> productAttributes =
        Optional.ofNullable(product.getProductAttributes()).orElseGet(ArrayList::new).stream()
            .filter(Predicate.not(ProductAttribute::isMarkForDelete)).collect(Collectors.toCollection(ArrayList::new));
    List<String> attributeIdsMappedToProduct = productAttributes.stream().map(ProductAttribute::getAttributeId)
        .collect(Collectors.toCollection(ArrayList::new));
    List<InternalProductHistoryEventModel> internalProductHistoryEventModelList = new ArrayList<>();
    // For each attribute code proceed with value add, update or remove.
    processDsAttributeMapping(storeId, product, attributeCodeAndAttributeModelMap, attributeCodeToAttributeMap,
        productAttributes, attributeIdsMappedToProduct, internalProductHistoryEventModelList);
    if(CollectionUtils.isNotEmpty(internalProductHistoryEventModelList)) {
      productService.saveAndFlush(product);
      productService.evictAllProductDetailCache(product.getStoreId(), product);
      domainEventPublisherService.publishProductChangeCategory(product, null, false, false, false, false,
          new HashSet<>());
      publishEventToVendor(product);
      publishInternalHistory(internalProductHistoryEventModelList);
    }
  }

  @Override
  public void processCompressedUpdatedVideo(
    CompressedVideoUpdateEventModel compressedVideoUpdateEventModel) throws Exception {
    if (clientIdForVideoCompression.equalsIgnoreCase(
      compressedVideoUpdateEventModel.getClientId())) {
      String productCode =
        compressedVideoUpdateEventModel.getAdditionalFields().get(Constants.PRODUCT_CODE);
      GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(productCode),
        ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(compressedVideoUpdateEventModel.getVideoId()),
        ErrorMessage.VIDEO_ID_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(compressedVideoUpdateEventModel.getFinalUrl()),
        ErrorMessage.FINAL_VIDEO_URL_MUST_NOT_BE_BLANK.getMessage());
      VideoDTO videoDTO;
      Product product = productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Constants.DEFAULT_STORE_ID, productCode);
      GdnPreconditions.checkArgument(Objects.nonNull(product),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productCode);
      videoDTO = getVideoAddEditRequest(compressedVideoUpdateEventModel, product);
      productService.processCompressedUpdatedVideo(product.getProductCode(), videoDTO);
      productService.evictAllProductDetailCache(Constants.DEFAULT_STORE_ID, product);
    }
  }

  private static VideoDTO getVideoAddEditRequest(
    CompressedVideoUpdateEventModel compressedVideoUpdateEventModel, Product product)
    throws JsonProcessingException {
    VideoDTO videoDTO;
    if (StringUtils.isNotEmpty(product.getVideo())) {
      videoDTO = new ObjectMapper().readValue(product.getVideo(), VideoDTO.class);
      videoDTO.setFinalUrl(compressedVideoUpdateEventModel.getFinalUrl());
    } else {
      log.info("Video Data missing for product : {} , creating new video object",
        product.getProductCode());
      videoDTO = new VideoDTO();
      videoDTO.setFinalUrl(compressedVideoUpdateEventModel.getFinalUrl());
      videoDTO.setVideoId(compressedVideoUpdateEventModel.getVideoId());
      videoDTO.setCoverImagePath(Optional.of(compressedVideoUpdateEventModel)
        .map(CompressedVideoUpdateEventModel::getCoverImagePath).orElse(StringUtils.EMPTY));
      videoDTO.setVideoName(Optional.of(compressedVideoUpdateEventModel)
        .map(CompressedVideoUpdateEventModel::getVideoName).orElse(StringUtils.EMPTY));
      videoDTO.setSourceUrl(Optional.of(compressedVideoUpdateEventModel)
        .map(CompressedVideoUpdateEventModel::getSourceUrl).orElse(StringUtils.EMPTY));
    }
    return videoDTO;
  }

  private void publishInternalHistory(List<InternalProductHistoryEventModel> internalProductHistoryEventModelList) {
    for(InternalProductHistoryEventModel internalProductHistoryEventModel : internalProductHistoryEventModelList) {
      domainEventPublisherService.publishInternalHistoryEvent(internalProductHistoryEventModel);
    }
  }

  private void processDsAttributeMapping(String storeId, Product product,
      Map<String, List<ProductSuitabilityAttributeModel>> attributeCodeAndAttributeModelMap,
      Map<String, Attribute> attributeCodeToAttributeMap, List<ProductAttribute> productAttributes,
      List<String> attributeIdsMappedToProduct,
      List<InternalProductHistoryEventModel> internalProductHistoryEventModelList) {
    for (Map.Entry<String, List<ProductSuitabilityAttributeModel>> attributeCodeAndAttributeModelEntry :
        attributeCodeAndAttributeModelMap.entrySet()) {
      Attribute attribute = attributeCodeToAttributeMap.get(attributeCodeAndAttributeModelEntry.getKey());
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList =
          attributeCodeAndAttributeModelEntry.getValue();
      List<String> existingValues = new ArrayList<>();
      // if attribute is not already mapped to product , Add product attribute then add product attribute values.
      if (!attributeIdsMappedToProduct.contains(attribute.getId())) {
        existingValues = List.of(Constants.HYPHEN);
        List<ProductSuitabilityAttributeModel> nullRemovedAttributes =
          ConverterUtil.removeNullAttribute(productSuitabilityAttributeModelList);
        if (CollectionUtils.isEmpty(nullRemovedAttributes)) {
          return;
        }
        createAndMapProductAttributesAndAttributeValues(storeId, product, attribute, nullRemovedAttributes);
      } else {
        // If the attribute already mapped to product , modify attribute values.
        ProductAttribute productAttribute = productAttributes.stream()
            .filter(productAttribute1 -> StringUtils.equals(attribute.getId(), productAttribute1.getAttributeId()))
            .findFirst().orElse(new ProductAttribute());
        existingValues = CommonUtil.getExistingAttributeValues(attribute, existingValues, productAttribute);
        createAndMapProductAttributeValues(storeId, productAttribute, attribute, productSuitabilityAttributeModelList);
      }
      List<String> newValues = CommonUtil.getNewAttributeValues(productSuitabilityAttributeModelList,
          attribute.getAttributeType());
      if (!CollectionUtils.isEqualCollection(existingValues, newValues)) {
        InternalProductHistoryEventModel internalProductHistoryEventModel =
            CommonUtil.getInternalProductHistoryEventModel(product, Constants.DS_EXTRACTED_ATTRIBUTE,
                Constants.DS_ATTRIBUTE_CHANGE);
        internalProductHistoryEventModel.setNotes(
            CommonUtil.getInternalHistoryNotes(existingValues, newValues, attribute.getDsAttributeName()));
        internalProductHistoryEventModelList.add(internalProductHistoryEventModel);
      }
    }
  }

  public void publishEventToVendor(Product product) {
    if (product.isReviewPending() && publishVendorEventForDsAttributeMappingEnabled) {
      domainEventPublisherService.publishVendorEvent(
          VendorPublishEventModel.builder().storeId(product.getStoreId()).productCode(product.getProductCode())
              .reviewType(EditedReviewTypeConstants.CONTENT_REFRESH).build());
    }
  }

  public void createAndMapProductAttributeValues(String storeId, ProductAttribute productAttribute, Attribute attribute,
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList) {
    switch (attribute.getAttributeType()) {
      case DESCRIPTIVE_ATTRIBUTE ->
          CommonUtil.processExistingDescriptiveAttributeValue(productSuitabilityAttributeModelList, productAttribute);
      case PREDEFINED_ATTRIBUTE ->
          processExistingPredefinedAttributeValues(storeId, attribute, productSuitabilityAttributeModelList,
              productAttribute);
      case DESCRIPTIVE_MULTIVALUE ->
          CommonUtil.processExistingDescriptiveMultiValueAttributes(productSuitabilityAttributeModelList,
              productAttribute);
      case PREDEFINED_MULTIVALUE ->
          processExistingPredefinedMultiValueAttributes(storeId, attribute, productSuitabilityAttributeModelList,
              productAttribute);
      default -> {
      }
    }
  }

  public void createAndMapProductAttributesAndAttributeValues(String storeId, Product product, Attribute attribute,
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList) {
    ProductAttribute productAttribute =
        CommonUtil.getProductAttribute(product, attribute, Constants.DS_EXTRACTED_ATTRIBUTE);
    switch (attribute.getAttributeType()) {
      case DESCRIPTIVE_ATTRIBUTE -> CommonUtil.createDescriptiveAttributeValue(productAttribute,
          productSuitabilityAttributeModelList.stream().findFirst().map(ProductSuitabilityAttributeModel::getValue)
              .orElse(null));
      case PREDEFINED_ATTRIBUTE ->
          processPredefinedAttributes(storeId, attribute, productSuitabilityAttributeModelList, productAttribute);
      case DESCRIPTIVE_MULTIVALUE ->
          CommonUtil.processDescriptiveMultiValueAttributes(productSuitabilityAttributeModelList, productAttribute);
      case PREDEFINED_MULTIVALUE ->
          processPredefinedMultiValueAttributes(storeId, attribute, productSuitabilityAttributeModelList,
              productAttribute, 0);
      default -> {
      }
    }
    product.getProductAttributes().add(productAttribute);
  }

  public void processExistingPredefinedAttributeValues(String storeId, Attribute attribute,
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList, ProductAttribute productAttribute) {
    // check if predefined value exist , if not add predefined and then map it with product
    String attributeValue =
        productSuitabilityAttributeModelList.stream().findFirst().map(ProductSuitabilityAttributeModel::getValue)
            .orElse(null);
    String attributeValueEn =
        productSuitabilityAttributeModelList.stream().findFirst().map(ProductSuitabilityAttributeModel::getValueEn)
            .orElse(null);
    Map<String, ProductAttributeValue> predefinedValueToProductAttributeValueMap =
        CommonUtil.getPredefinedValueToProductAttributeValueMap(productAttribute);
    Optional<ProductAttributeValue> optionalProductAttributeValue =
        productAttribute.getProductAttributeValues().stream()
            .filter(Predicate.not(ProductAttributeValue::isMarkForDelete)).findFirst();
    if (Objects.isNull(attributeValue)) {
      // if the attribute value from event is sent null , then unmap the existing ds attribute mapped to product
      optionalProductAttributeValue.ifPresent(productAttributeValue -> productAttributeValue.setMarkForDelete(true));
      productAttribute.setMarkForDelete(true);
      productAttribute.setUpdatedBy(Constants.DS_EXTRACTED_ATTRIBUTE);
    } else {
      addOrUpdatePredefinedAttributeValue(storeId, attribute, productAttribute, attributeValue, attributeValueEn,
          predefinedValueToProductAttributeValueMap, optionalProductAttributeValue);
    }
  }

  private void addOrUpdatePredefinedAttributeValue(String storeId, Attribute attribute,
      ProductAttribute productAttribute, String attributeValue, String attributeValueEn,
      Map<String, ProductAttributeValue> predefinedValueToProductAttributeValueMap,
      Optional<ProductAttributeValue> optionalProductAttributeValue) {
    // The attribute value needs to be updated to the new value sent in event
    if (optionalProductAttributeValue.isPresent()) {
      ProductAttributeValue existingProductAttributeValue = optionalProductAttributeValue.get();
      PredefinedAllowedAttributeValue existingPredefinedValue =
          existingProductAttributeValue.getPredefinedAllowedAttributeValue();
      if (Objects.isNull(existingPredefinedValue)) {
        existingPredefinedValue =
          getPredefinedAllowedAttributeValue(storeId, attribute, attributeValue, attributeValueEn,
            0);
        existingProductAttributeValue.setPredefinedAllowedAttributeValue(existingPredefinedValue);
        return;
      }
      if (!StringUtils.equals(existingPredefinedValue.getValue(), attributeValue)) {
        existingProductAttributeValue.setMarkForDelete(true);
        if (predefinedValueToProductAttributeValueMap.containsKey(attributeValue)) {
          predefinedValueToProductAttributeValueMap.get(attributeValue).setMarkForDelete(false);
        } else {
          ProductAttributeValue productAttributeValue =
            generateProductAttributeValue(storeId, productAttribute, attribute, attributeValue,
              attributeValueEn, 0);
          productAttribute.getProductAttributeValues().add(productAttributeValue);
        }
      }
      return;
    }
    // else not present then
    ProductAttributeValue productAttributeValue =
        generateProductAttributeValue(storeId, productAttribute, attribute, attributeValue, attributeValueEn, 0);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));  }

  public PredefinedAllowedAttributeValue getPredefinedAllowedAttributeValue(String storeId, Attribute attribute,
      String attributeValue, String attributeValueEn, int sequence) {
    // if predefined attribute value exist then fetch form db, else create new value.
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues =
        predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(storeId, attribute,
            attributeValue);
    return CollectionUtils.isNotEmpty(predefinedAllowedAttributeValues) ?
        predefinedAllowedAttributeValues.get(0) :
        predefinedAllowedAttributeValueService.addPredefinedAllowedAttributeValue(storeId, attribute,
            Constants.DS_EXTRACTED_ATTRIBUTE, attributeValue, sequence, attributeValueEn);
  }

  public void processExistingPredefinedMultiValueAttributes(String storeId, Attribute attribute,
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList, ProductAttribute productAttribute)
       {
    if (Objects.isNull(productSuitabilityAttributeModelList.get(0).getValue())) {
      // if the attribute value from event is sent null , then unmap all the existing ds attribute mapped to product
      CommonUtil.removeProductAttributeValuesAndProductAttribute(productAttribute);
    } else {
      // Filter out the existing values which are sent in event
      Set<String> predefinedAttributeValuesWithNullMapping = new HashSet<>();
      Map<String, String> existingPredefinedIdAndValueMap =
          CommonUtil.getExistingPredefinedIdAndValueMap(productAttribute, predefinedAttributeValuesWithNullMapping);
      CommonUtil.removeExistingPredefinedValues(productSuitabilityAttributeModelList, productAttribute,
          existingPredefinedIdAndValueMap, predefinedAttributeValuesWithNullMapping,
          dsExtractionListenerRemoveEmptyPredefinedValue);
      productSuitabilityAttributeModelList = productSuitabilityAttributeModelList.stream().filter(Predicate.not(
          productSuitabilityAttributeModel -> existingPredefinedIdAndValueMap.containsValue(
              productSuitabilityAttributeModel.getValue()))).collect(Collectors.toCollection(ArrayList::new));
      int sequence = existingPredefinedIdAndValueMap.size() + 1;

      // For the new attribute values , check if value exist then map it to product or else create and map
      processPredefinedMultiValueAttributes(storeId, attribute, productSuitabilityAttributeModelList, productAttribute,
          sequence);
      CommonUtil.removeProductAttributeIfAllValuesDeleted(productAttribute);
    }
  }

  public void processPredefinedMultiValueAttributes(String storeId, Attribute attribute,
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList, ProductAttribute productAttribute,
      int sequence) {
    for (ProductSuitabilityAttributeModel productSuitabilityAttributeModel : productSuitabilityAttributeModelList) {
      ProductAttributeValue productAttributeValue = generateProductAttributeValue(storeId, productAttribute, attribute,
          productSuitabilityAttributeModel.getValue(), productSuitabilityAttributeModel.getValueEn(), sequence++);
      productAttribute.getProductAttributeValues().add(productAttributeValue);
    }
  }

  public void processPredefinedAttributes(String storeId, Attribute attribute,
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList, ProductAttribute productAttribute)
       {
    String attributeValue =
        productSuitabilityAttributeModelList.stream().findFirst().map(ProductSuitabilityAttributeModel::getValue)
            .orElse(null);
    String attributeValueEn =
        productSuitabilityAttributeModelList.stream().findFirst().map(
                ProductSuitabilityAttributeModel::getValueEn)
            .orElse(null);
    ProductAttributeValue productAttributeValue =
        generateProductAttributeValue(storeId, productAttribute, attribute, attributeValue, attributeValueEn, 0);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));
  }

  public ProductAttributeValue generateProductAttributeValue(String storeId, ProductAttribute productAttribute,
      Attribute attribute, String attributeValue, String attributeValueEn, int sequence) {
    ProductAttributeValue productAttributeValue =
        CommonUtil.createBaseProductAttributeValue(productAttribute);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
        getPredefinedAllowedAttributeValue(storeId, attribute, attributeValue, attributeValueEn, sequence);
    CommonUtil.setBasicProductAttributeValueDetails(productAttributeValue, predefinedAllowedAttributeValue);
    return productAttributeValue;
  }

  public List<String> getCategoryAttributeCodes(String storeId, ProductCategory productCategory) {
    List<CategoryAttribute> categoryAttributes =
        categoryService.getCategoryAttributes(storeId, productCategory.getCategoryId());
    return categoryAttributes.stream().filter(Predicate.not(CategoryAttribute::isMarkForDelete))
        .map(CategoryAttribute::getAttribute).map(Attribute::getAttributeCode)
        .collect(Collectors.toCollection(ArrayList::new));
  }

  @Override
  public void deleteMfdTrueImagesAndAttributes(
      CommonImageBackfillingEventModel deleteMfdTrueEventModel) {
    String storeId = deleteMfdTrueEventModel.getStoreId();
    String productCode = deleteMfdTrueEventModel.getProductCode();
    log.info("Deleting mfd true rows for product code: {}", productCode);
    schedulerService.updateProductMigrationStatus(storeId, null,
        CommonUtil.getProductMigrationRequest(deleteMfdTrueEventModel, null,
            ProductMigrationStatus.IN_PROGRESS));
    Product product =
        productService.getProductByStoreIdAndProductCodeCached(deleteMfdTrueEventModel.getStoreId(),
            productCode);
    GdnPreconditions.checkArgument(Objects.nonNull(product),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productCode);
    if (product.isMarkForDelete() && skipMfdTrueProductsForDeletion) {
      log.info("Product with code: {} is marked for deletion, skipping deletion of mfd true rows.",
          productCode);
      schedulerService.updateProductMigrationStatus(storeId, null,
          CommonUtil.getProductMigrationRequest(deleteMfdTrueEventModel, null,
              ProductMigrationStatus.SKIPPED));
      return;
    }
    productService.setCompleteProductDetailsCached(storeId, product, true);
    productService.deleteMfdTrueRowsFromProduct(product);
    productService.evictAllProductDetailCache(storeId, product);
    log.info("Successfully deleted all marked for deletion records for product code: {}",
        productCode);
    schedulerService.updateProductMigrationStatus(deleteMfdTrueEventModel.getStoreId(), null,
        CommonUtil.getProductMigrationRequest(deleteMfdTrueEventModel, null,
            ProductMigrationStatus.SUCCESS));
  }

  @Override
  public void updateMasterDataAndEvictCache(String storeId, String username,
      ProductMasterDataUpdateRequest productMasterDataUpdateRequest) throws Exception {
    if (StringUtils.isNotBlank(productMasterDataUpdateRequest.getSizeChartCode())) {
      sizeChartService.validateSizeChartForBusinessPartnerCodeAndCategoryCode(storeId,
          productMasterDataUpdateRequest.getSizeChartCode(),
          productMasterDataUpdateRequest.getBusinessPartnerCode(),
          productMasterDataUpdateRequest.getCategoryCode());
    }

    Product savedProduct = getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
        productMasterDataUpdateRequest.getProductCode());

    AuditTrailListResponse auditTrailListResponse =
        productService.updateMasterDataAndGenerateHistory(storeId, username, productMasterDataUpdateRequest,
            savedProduct);

    productService.saveAndFlush(savedProduct);
    productService.evictProductCache(storeId, savedProduct);
    boolean evictProductItemCache = Optional.ofNullable(auditTrailListResponse)
        .map(AuditTrailListResponse::getAuditTrailResponseList).orElseGet(List::of).stream()
        .filter(Objects::nonNull)
        .anyMatch(dto -> Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME.equals(dto.getActionKey()));
    if(evictProductItemCache) {
      productService.evictProductItemCache(savedProduct);
    }

    if(CollectionUtils.isNotEmpty(productMasterDataUpdateRequest.getCommonImages())) {
      domainEventPublisherService.publishProduct(savedProduct);
    }
    publishExternalHistory(auditTrailListResponse, productMasterDataUpdateRequest);
  }

  @Override
  @Transactional(readOnly = true)
  public List<ProductMasterDataItemResponse> getProductMasterDataByProductCode(String storeId, ProductCodesRequest request) {
    List<Product> productCodeDetails = new ArrayList<>();
    for (String productCode : request.getProductCodes()) {
      try {
        Product product = productService.getProductByStoreIdAndProductCodeCached(storeId, productCode);
        productItemServiceWrapper.setProductItemsCached(storeId, product, true);
        productService.setProductCategoriesWithCategoriesCached(storeId, product, false);
        productCodeDetails.add(product);
      } catch (Exception e) {
        log.error("Error while fetching product details for product code: {} ", productCode, e);
      }
    }
    return CommonUtil.setProductMasterData(productCodeDetails);
  }

  @Override
  public ProductBrandUpdateResponse updateProductBrandDataWithBrandInfo(String storeId,
      ProductBrandDataUpdateRequest productBrandDataUpdateRequest) throws Exception {
    GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(productBrandDataUpdateRequest.getOldBrandCode()),
        ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK.getMessage());
    if (productBrandDataUpdateRequest.isOnlyBrandNameUpdate()) {
      if (productBrandDataUpdateRequest.isBrandLevelUpdateRequired()) {
        GdnPreconditions.checkArgument(
            StringUtils.isNotBlank(productBrandDataUpdateRequest.getNewBrandName()),
            ErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK.getMessage());
        Triple<BrandHistoryEventModel, BrandWip, Brand> brandHistoryEventModelBrandWipBrandTriple =
            brandService.updateOnlyBrandName(storeId,
                productBrandDataUpdateRequest.getNewBrandName(),
                productBrandDataUpdateRequest.getOldBrandCode());
        domainEventPublisherService.publishBrandHistory(
            brandHistoryEventModelBrandWipBrandTriple.getLeft());
        domainEventPublisherService.publishSolrUpdateBrandEvent(
            SolrUpdateBrandDomainEventModel.builder().updateBrandModels(List.of(
                SolrUpdateBrandModel.builder()
                    .id(brandHistoryEventModelBrandWipBrandTriple.getMiddle().getId())
                    .brandCode(productBrandDataUpdateRequest.getOldBrandCode())
                    .brandNameUpdated(true)
                    .brandName(productBrandDataUpdateRequest.getNewBrandName()).build())).build());
        domainEventPublisherService.publishBrandUpdated(
            brandHistoryEventModelBrandWipBrandTriple.getRight());
      }
      Product product =
          productService.updateOnlyBrandNameOfProduct(storeId, productBrandDataUpdateRequest);
      productService.evictCompleteProductAndItemsCache(storeId, product);
      Product updatedProduct =
          productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
              productBrandDataUpdateRequest.getProductCode());
      domainEventPublisherService.publishProductChangeCategory(updatedProduct, null, true, true, true,
          false, new HashSet<>());
      ProductBrandUpdateResponse response = new ProductBrandUpdateResponse();
      response.setProductCode(updatedProduct.getProductCode());
      response.setBrandName(updatedProduct.getBrand());
      response.setBrandCode(productBrandDataUpdateRequest.getOldBrandCode());
      return response;
    } else {
      Pair<Product, String> productWithBrandName = productService.updateProductBrand(storeId,
          new ProductBrandUpdateDTO(productBrandDataUpdateRequest.getProductCode(),
              productBrandDataUpdateRequest.getOldBrandCode(),
              productBrandDataUpdateRequest.getNewBrandCode(),
              productBrandDataUpdateRequest.getBusinessPartnerCodes()));
      productService.evictCacheForSimpleMasterProductUpdate(storeId,
          productWithBrandName.getKey().getId(), productWithBrandName.getKey().getProductCode());
      Product updatedProduct =
          productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
              productBrandDataUpdateRequest.getProductCode());
      domainEventPublisherService.publishProductChangeCategory(updatedProduct, null,
          true, true, true, false, new HashSet<>());
      ProductBrandUpdateResponse response = new ProductBrandUpdateResponse();
      response.setProductCode(updatedProduct.getProductCode());
      response.setBrandName(updatedProduct.getBrand());
      response.setBrandCode(productBrandDataUpdateRequest.getNewBrandCode());
      return response;
    }
  }
}
