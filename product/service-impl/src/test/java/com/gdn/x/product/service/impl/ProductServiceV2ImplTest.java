package com.gdn.x.product.service.impl;

import static com.gdn.x.product.service.util.CommonUtil.setItemPickupPointDetailsInItemWithCncFlagUpdate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.x.product.model.entity.PreOrder;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.dao.solr.api.ProductSolrRepository;
import com.gdn.x.product.domain.event.model.HalalHistoryUpdateEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.PristineItemAndSiblingsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.request.DistributionInfoByOmniChannelSkusRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryMappingUpdateRequest;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicMasterDataProductDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.DistributionInfoByOmniChannelSkusResponse;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.HalalProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.DimensionsAndUOMResponse;
import com.gdn.x.product.rest.web.model.response.OmniChannelSkuDetailResponse;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsSummaryResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductDetailResponseV2;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PristineCacheableService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;
import com.gdn.x.product.service.util.ProductAttributesUtil;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionsAndUomResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionItemInfoResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;

public class ProductServiceV2ImplTest {
  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String ITEM_SKU = "itemSku";
  private static final String PRODUCT_SKU = "productSku";

  private static final String PRODUCT_SKU_1 = "BLI-15014-17039";
  private static final String PRODUCT_SKU_2 = "BLI-15014-17040";

  private static final String ITEM_SKU_1 = "BLI-15014-17039-00001";
  private static final String ITEM_SKU_2 = "BLI-15014-17040-00001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PRISTINE_ID = "pristineId";
  private static final String PRISTINE_CATEGORY = "HANDPHONE";
  private static final String PRISTINE_MASTER_ID = "pristineMasterId";
  private static final String BRAND = "Brand";
  private static final String ITEM_CODE = "itemCode" ;
  private static final String DESCRIPTION = "description";
  private static final String CATEGORY_CODE = "category code";
  private static final String CNC = "CNC";
  private static final String DEFAULT = "DEFAULT";
  private static final String SELLER_CODE = "sellerCode";
  private static final String OMNICHANNEL_SKU_CODE = "omnichannelSkuCode";
  private static final String ITEM_NAME = "itemName";
  private static final String OMNICHANNEL_SKU = "omnichannelSku";
  private static final String MAIN_IMAGE_URL = "mainImageUrl";
  private static final String HALAL_VALUE = "halalValue";
  private static final String STORAGE_VALUE = "storageValue";
  private static final String UOM_CODE = "CM";
  private static final Set<ItemViewConfig> ALL_VIEW_CONFIGS = getAllViewConfigs();
  private static final PreOrder PRE_ORDER = new PreOrder(true,"preOrderType",null, new Date());

  @InjectMocks
  private ProductServiceV2Impl productServiceV2;

  @Mock
  private SkuValidator skuValidator;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private ProductCacheableService productCacheableService;

  @Mock
  private ProductService productService;

  @Mock
  private GdnMapper gdnMapper;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private ProductSearchHelperService productSearchHelperService;

  @Mock
  private ProductSolrRepository productSolrRepository;

  @Mock
  private ItemPriceService itemPriceService;

  @Mock
  private MasterDataConstructorService masterDataConstructorService;

  @Mock
  private PristineCacheableService pristineCacheableService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ProductAttributesUtil productAttributesUtil;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private CatalogService catalogService;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Captor
  private ArgumentCaptor<SalesCategoryMappingUpdateRequest> salesCategoryMappingUpdateRequest;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<HalalHistoryUpdateEventModel> halalHistoryUpdateEventModelArgumentCaptor;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  private ItemPickupPoint itemPickupPoint;
  private Item item;
  private Product product;
  private SystemParameter systemParameter = new SystemParameter();
  private ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2;
  private ItemResponse itemResponse;
  private ProductSkuAndProductCodeRequest productSkuAndProductCodeRequest = new ProductSkuAndProductCodeRequest();
  private BasicItemDTO basicItemDTO = new BasicItemDTO();
  private BasicMasterDataProductDTO masterDataProduct = new BasicMasterDataProductDTO();
  private BasicProductAndItemDTO basicProductAndItemDTO = new BasicProductAndItemDTO();
  private ProductResponse productResponse = new ProductResponse();
  private CategoryDetailResponse categoryDetailResponse;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    ReflectionTestUtils.setField(productServiceV2, "isProductVisibilityEnabled", true);
    ReflectionTestUtils.setField(productServiceV2, "omniChannelSkuMaxRequestSize", 10);

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setProductSku(PRODUCT_SKU);
    itemResponse = new ItemResponse();
    itemResponse.setItemSku(ITEM_SKU);
    itemResponse.setItemCode(ITEM_CODE);
    productAndItemsSummaryResponseV2 = new ProductAndItemsSummaryResponseV2();
    productAndItemsSummaryResponseV2.setItemDetailResponsesV2(Collections.singletonList(
      itemResponse));
    productAndItemsSummaryResponseV2.setProductDetailResponseV2(ProductDetailResponseV2.builder().productSku(PRODUCT_SKU).markForDelete(false)
      .build());
    product = new Product();
    product.setProductSku(PRODUCT_SKU);
    basicItemDTO.setProductSku(PRODUCT_SKU);

    masterDataProduct.setDescription(DESCRIPTION);
    basicProductAndItemDTO.setMasterDataProduct(masterDataProduct);
    CategoryReferenceResponse halalCategoryReferenceResponse = new CategoryReferenceResponse();
    CategoryResponse halalCategoryResponse = new CategoryDetailResponse();
    halalCategoryResponse.setCatalog(new CatalogResponse());
    halalCategoryReferenceResponse.setHalalSalesCategoryReference(halalCategoryResponse);
    categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setHalalSalesCategoryReferences(List.of(halalCategoryReferenceResponse));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(skuValidator, itemPickupPointService, cacheItemHelperService,
        productCacheableService, productService, productHelperService, productSearchHelperService,
        masterDataConstructorService, itemPriceService, pristineCacheableService, objectConverterService,
        productAttributesUtil, mandatoryParameterHelper , productRepository);
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(itemPriceService.getDiscountItemPickupPoint(Arrays.asList(itemPickupPoint)))
        .thenReturn(new HashMap<>());
    Mockito.doNothing().when(productHelperService)
        .findAndConstructOfflineItemsByPickupPointCode(STORE_ID, Arrays.asList(item), PICKUP_POINT_CODE);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
            anyList(), anyMap());
    Mockito.doNothing().when(productService)
        .setSellerPromoBundlings(eq(STORE_ID), anyList());

    productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
        PICKUP_POINT_CODE, true);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(productHelperService)
        .findAndConstructOfflineItemsByPickupPointCode(STORE_ID, Arrays.asList(item), PICKUP_POINT_CODE);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    Mockito.verify(productService)
        .setSellerPromoBundlings(eq(STORE_ID),  anyList());
  }

  @Test
  public void getProductAndSingleItemByItemSkuWithPermanentDeleteTrueAndPickupPointCodeTest() throws Exception {
    item.setPermanentDelete(true);
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
          PICKUP_POINT_CODE, true));
    } finally {
      Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
      Mockito.verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
      Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeMarkForDeleteFalseTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(
            itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(itemPriceService.getDiscountItemPickupPoint(Arrays.asList(itemPickupPoint)))
        .thenReturn(new HashMap<>());
    Mockito.doNothing().when(productHelperService)
        .findAndConstructOfflineItemsByPickupPointCode(STORE_ID, Arrays.asList(item), PICKUP_POINT_CODE);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    Mockito.doNothing().when(productService)
        .setSellerPromoBundlings(eq(STORE_ID),  anyList());

    productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
        PICKUP_POINT_CODE, false);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(itemPickupPointService)
        .findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(productHelperService)
        .findAndConstructOfflineItemsByPickupPointCode(STORE_ID, Arrays.asList(item), PICKUP_POINT_CODE);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    Mockito.verify(productService)
        .setSellerPromoBundlings(eq(STORE_ID),  anyList());
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeInvalidItemSkuTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
          PICKUP_POINT_CODE, false));
    } finally {
      Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    }
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeNullItemPickupPointTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(
            itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
          PICKUP_POINT_CODE, false));
    } finally {
      Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
      Mockito.verify(itemPickupPointService)
          .findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    }
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeNullItemTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(
            itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
          PICKUP_POINT_CODE, false));
    } finally {
      Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
      Mockito.verify(itemPickupPointService)
          .findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
      Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeNullProductTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(
            itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
          PICKUP_POINT_CODE, false));
    } finally {
      Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
      Mockito.verify(itemPickupPointService)
          .findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
      Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
      Mockito.verify(productCacheableService)
          .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    }
  }

  @Test
  public void getProductAndSingleItemByItemSkuTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
            anyList(), anyMap());

    productServiceV2.getProductAndSingleItemByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, false);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
  }

  @Test
  public void getProductAndSingleItemByItemSkuInvalidItemSkuTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceV2.getProductAndSingleItemByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, false));
    } finally {
      Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    }
  }

  @Test
  public void getProductAndSingleItemByItemSkuNullItemTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceV2.getProductAndSingleItemByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, false));
    } finally {
      Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
      Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void getProductAndSingleItemByItemSkuNullProductTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceV2.getProductAndSingleItemByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, false));
    } finally {
      Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
      Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
      Mockito.verify(productCacheableService)
          .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    }
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeMfdTrueTest() throws Exception {
    itemPickupPoint.setMarkForDelete(true);
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(
            itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
          PICKUP_POINT_CODE, false));
    } finally {
      Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
      Mockito.verify(itemPickupPointService)
          .findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    }
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeNullTest() throws Exception {
    itemPickupPoint.setMarkForDelete(true);
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(
            itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
          PICKUP_POINT_CODE, false));
    } finally {
      Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
      Mockito.verify(itemPickupPointService)
          .findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    }
  }

  @Test
  public void getProductAndItemsByItemSkusTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<ProductItemsVo> result =
        productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, itemSkus, false, false);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void getProductAndItemsByItemSkusWithOff2OnTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<ProductItemsVo> result =
        productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, itemSkus, false, true);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    Assertions.assertEquals(0, result.size());
  }

  @Test
  public void getProductAndItemsByItemSkusWithOff2OnProductOff2OnTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    product.setOff2OnChannelActive(true);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<ProductItemsVo> result =
        productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, itemSkus, false, true);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(),  anyMap());
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void getProductAndItemsByItemSkusWithPristineTrueTest() throws Exception {
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineMasterId(PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);
    pristineDataItem.setPristineBrand(BRAND);
    pristineDataItem.setPristineModel(PRISTINE_MASTER_ID);
    item.setPristineDataItem(pristineDataItem);

    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineItemAndSiblingsVO.setSiblings(Arrays.asList(pristineDataItem));
    Map<String, String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");
    Mockito.when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    Mockito.when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<ProductItemsVo> result =
        productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, itemSkus, true, false);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(),  anyMap());
    Assertions.assertEquals(1, result.size());
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(objectConverterService).convertPristineAttributeToProductAttribute(pristineDataItem);
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.isNull());
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
  }

  @Test
  public void getProductAndItemsByItemSkusWithPristineTrueNullBrandTest() throws Exception {
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineMasterId(PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);
    pristineDataItem.setPristineModel(PRISTINE_MASTER_ID);
    item.setPristineDataItem(pristineDataItem);

    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineItemAndSiblingsVO.setSiblings(Arrays.asList(pristineDataItem));
    Map<String, String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");
    Mockito.when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    Mockito.when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<ProductItemsVo> result =
        productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, itemSkus, true, false);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    Assertions.assertEquals(1, result.size());
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(objectConverterService).convertPristineAttributeToProductAttribute(pristineDataItem);
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.isNull());
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
  }

  @Test
  public void getProductAndItemsByItemSkusWithPristineTrueNullModelTest() throws Exception {
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineMasterId(PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);
    pristineDataItem.setPristineBrand(BRAND);

    item.setPristineDataItem(pristineDataItem);

    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineItemAndSiblingsVO.setSiblings(Arrays.asList(pristineDataItem));
    Map<String, String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");
    Mockito.when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    Mockito.when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<ProductItemsVo> result =
        productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, itemSkus, true, false);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    Assertions.assertEquals(1, result.size());
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(objectConverterService).convertPristineAttributeToProductAttribute(pristineDataItem);
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.isNull());
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
  }

  @Test
  public void getProductAndItemsByItemSkusWithPristineTrueNullModelNullBrandTest() throws Exception {
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineMasterId(PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);

    item.setPristineDataItem(pristineDataItem);

    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineItemAndSiblingsVO.setSiblings(Arrays.asList(pristineDataItem));
    Map<String, String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");
    Mockito.when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    Mockito.when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);
    when(pristineCacheableService.findFirstItemByPristine(any(PristineDataItem.class))).thenReturn(new Item());
    Item item1 = new Item();
    item1.setPristineDataItem(pristineDataItem);
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(any(Item.class))).thenReturn(item1);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<ProductItemsVo> result =
        productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, itemSkus, true, false);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    Assertions.assertEquals(1, result.size());
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(objectConverterService).convertPristineAttributeToProductAttribute(pristineDataItem);
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.isNull());
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
    verify(pristineCacheableService).findFirstItemByPristine(any(PristineDataItem.class));
    verify(masterDataConstructorService, Mockito.times(2)).constructPristineDataItemWithMasterData(any(Item.class));
  }

  @Test
  public void getProductAndItemsByItemSkusWithPristineTrueNoSiblingsTest() throws Exception {
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineMasterId(PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);

    item.setPristineDataItem(pristineDataItem);

    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineItemAndSiblingsVO.setSiblings(new ArrayList<>());
    Map<String, String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");
    Mockito.when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    Mockito.when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);
    when(pristineCacheableService.findFirstItemByPristine(any(PristineDataItem.class))).thenReturn(new Item());
    Item item1 = new Item();
    item1.setPristineDataItem(pristineDataItem);
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(any(Item.class))).thenReturn(item1);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<ProductItemsVo> result =
        productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, itemSkus, true, false);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    Assertions.assertEquals(1, result.size());
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.isNull());
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
    verify(masterDataConstructorService).constructPristineDataItemWithMasterData(any(Item.class));
  }

  @Test
  public void getProductAndItemsByItemSkusPristineTrueItemNonPristineTest() throws Exception {
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(
        masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
            Arrays.asList(item), true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    Mockito.doNothing().when(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<ProductItemsVo> result =
        productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, itemSkus, true, false);

    Mockito.verify(skuValidator).isItemSku(ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(masterDataConstructorService)
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true,
            false);
    Mockito.verify(productSearchHelperService)
        .setItemCatalogs(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(true),
                anyList(), anyMap());
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void isCncUpdateRequiredFalseTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    boolean cncupdateRequired = productServiceV2.isCncUpdateRequired(itemPickupPoint, true);
    Assertions.assertFalse(cncupdateRequired);
  }

  @Test
  public void isCncUpdateRequiredFalse1Test() {
    ItemPickupPoint itemPickupPoint = null;
    boolean cncupdateRequired = productServiceV2.isCncUpdateRequired(itemPickupPoint, false);
    Assertions.assertFalse(cncupdateRequired);
  }

  @Test
  public void isCncUpdateRequiredTrueTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    boolean cncupdateRequired = productServiceV2.isCncUpdateRequired(itemPickupPoint, false);
    Assertions.assertTrue(cncupdateRequired);
  }

  @Test
  public void isCncUpdateRequiredTrue1Test() {
    ItemPickupPoint itemPickupPoint = null;
    boolean cncupdateRequired = productServiceV2.isCncUpdateRequired(itemPickupPoint, true);
    Assertions.assertTrue(cncupdateRequired);
  }

  @Test
  public void updateCncFlagAtProductAndItemLevelTest() throws Exception {
    item.setProductSku(PRODUCT_SKU);
    when(itemPickupPointService.findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE))
        .thenReturn(itemPickupPoint);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    when(itemPickupPointService.findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, Boolean.TRUE))
        .thenReturn(itemPickupPoint);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    productServiceV2.updateCncFlagAtProductAndItemLevel(STORE_ID, USERNAME, Arrays.asList(ITEM_SKU));
    verify(itemPickupPointService).findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemPickupPointService).findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, Boolean.TRUE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).updateCncActivated(STORE_ID, ITEM_SKU, true,USERNAME);
    verify(productService).updateCncActivatedByProductSkuAndSolrL3(eq(STORE_ID), any(), eq(true),eq(USERNAME));
    verify(productAndItemSolrIndexerService).applyItems(any());
  }

  @Test
  public void updateCncFlagAtProductAndItemLevelCncFlagUpdateTest() throws Exception {
    item.setProductSku(PRODUCT_SKU);
    item.setCncActivated(true);
    product.setCncActivated(true);
    when(itemPickupPointService.findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE))
        .thenReturn(null);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    when(itemPickupPointService.findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, Boolean.TRUE))
        .thenReturn(null);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    productServiceV2.updateCncFlagAtProductAndItemLevel(STORE_ID, USERNAME, Arrays.asList(ITEM_SKU));
    verify(itemPickupPointService).findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemPickupPointService).findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, Boolean.TRUE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).updateCncActivated(STORE_ID, ITEM_SKU, false,USERNAME);
    verify(productService).updateCncActivatedByProductSkuAndSolrL3(eq(STORE_ID), any(), eq(false),eq(USERNAME));
    verify(productAndItemSolrIndexerService).applyItems(any());
  }

  @Test
  public void updateCncFlagAtProductAndItemLevelNoProductCncUpdateTest() throws Exception {
    item.setProductSku(PRODUCT_SKU);
    product.setCncActivated(true);
    when(itemPickupPointService.findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE))
        .thenReturn(itemPickupPoint);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    when(itemPickupPointService.findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, Boolean.TRUE))
        .thenReturn(itemPickupPoint);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    productServiceV2.updateCncFlagAtProductAndItemLevel(STORE_ID, USERNAME, Arrays.asList(ITEM_SKU));
    verify(itemPickupPointService).findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemPickupPointService).findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, Boolean.TRUE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).updateCncActivated(STORE_ID, ITEM_SKU, true,USERNAME);
    verify(productAndItemSolrIndexerService).applyItems(any());
  }

  @Test
  public void updateCncFlagAtProductAndItemLevelItemNullTest() throws Exception {
    item.setProductSku(PRODUCT_SKU);
    when(itemPickupPointService.findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE))
        .thenReturn(itemPickupPoint);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(null);
    productServiceV2.updateCncFlagAtProductAndItemLevel(STORE_ID, USERNAME, Arrays.asList(ITEM_SKU));
    verify(itemPickupPointService).findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void updateCncFlagAtProductAndItemLevelNoCncUpdateTest() throws Exception {
    item.setProductSku(PRODUCT_SKU);
    item.setCncActivated(true);
    when(itemPickupPointService.findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE))
        .thenReturn(itemPickupPoint);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    productServiceV2.updateCncFlagAtProductAndItemLevel(STORE_ID, USERNAME, Arrays.asList(ITEM_SKU));
    verify(itemPickupPointService).findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void validateDuplicateProductBySellerSkuTest() {
    when(productService.findByStoreIdAndProductSku(STORE_ID, item.getProductSku())).thenReturn(product);
    when(objectConverterService.convertProductToDuplicateProductDetailsResponse(product))
        .thenReturn(new DuplicateProductDetailsResponse());
    when(itemService.getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE, MERCHANT_CODE))
        .thenReturn(item);
    productServiceV2.validateDuplicateProductBySellerSku(STORE_ID, MERCHANT_CODE, MERCHANT_CODE);
    verify(itemService).getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE, MERCHANT_CODE);
    verify(productService).findByStoreIdAndProductSku(STORE_ID, item.getProductSku());
    verify(objectConverterService).convertProductToDuplicateProductDetailsResponse(product);
  }

  @Test
  public void validateDuplicateProductBySellerSkuSuspendedTest() {
    when(productService.getProductsByStoreIdAndMerchantCodeAndIsSuspended(STORE_ID, MERCHANT_CODE, true))
        .thenReturn(Arrays.asList(product));
    when(objectConverterService.convertProductToDuplicateProductDetailsResponse(product))
        .thenReturn(new DuplicateProductDetailsResponse());
    when(itemService.getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE, MERCHANT_CODE))
        .thenReturn(null);
    when(itemService.getOneItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(STORE_ID, MERCHANT_CODE
        , Arrays.asList(PRODUCT_SKU), MERCHANT_CODE))
        .thenReturn(item);
    productServiceV2.validateDuplicateProductBySellerSku(STORE_ID, MERCHANT_CODE, MERCHANT_CODE);
    verify(itemService).getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE, MERCHANT_CODE);
    verify(itemService).getOneItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(STORE_ID, MERCHANT_CODE
        , Arrays.asList(PRODUCT_SKU), MERCHANT_CODE);
    verify(productService).getProductsByStoreIdAndMerchantCodeAndIsSuspended(STORE_ID, MERCHANT_CODE, true);
    verify(objectConverterService).convertProductToDuplicateProductDetailsResponse(product);
  }

  @Test
  public void validateDuplicateProductBySellerSkuSuspendedNullTest() {
    when(productService.getProductsByStoreIdAndMerchantCodeAndIsSuspended(STORE_ID, MERCHANT_CODE, true))
        .thenReturn(Arrays.asList(product));
    when(itemService.getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE, MERCHANT_CODE))
        .thenReturn(null);
    when(itemService.getOneItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(STORE_ID, MERCHANT_CODE
        , Arrays.asList(PRODUCT_SKU), MERCHANT_CODE))
        .thenReturn(null);
    productServiceV2.validateDuplicateProductBySellerSku(STORE_ID, MERCHANT_CODE, MERCHANT_CODE);
    verify(itemService).getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE, MERCHANT_CODE);
    verify(itemService).getOneItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(STORE_ID, MERCHANT_CODE
        , Arrays.asList(PRODUCT_SKU), MERCHANT_CODE);
    verify(productService).getProductsByStoreIdAndMerchantCodeAndIsSuspended(STORE_ID, MERCHANT_CODE, true);
  }

  @Test
  public void validateDuplicateProductBySellerSkuSuspendedProductNullTest() {
    when(productService.getProductsByStoreIdAndMerchantCodeAndIsSuspended(STORE_ID, MERCHANT_CODE, true))
        .thenReturn(null);
    when(itemService.getOneItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(STORE_ID, MERCHANT_CODE
        , Arrays.asList(PRODUCT_SKU), MERCHANT_CODE))
        .thenReturn(null);
    productServiceV2.validateDuplicateProductBySellerSku(STORE_ID, MERCHANT_CODE, MERCHANT_CODE);
    verify(itemService).getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE, MERCHANT_CODE);
    verify(productService).getProductsByStoreIdAndMerchantCodeAndIsSuspended(STORE_ID, MERCHANT_CODE, true);
  }

  @Test
  public void getPrdProductDetailByProductSkuOrProductCodeByProductSkuTest() {
    productSkuAndProductCodeRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(new Product());
    Mockito.when(this.gdnMapper.deepCopy(any(), any())).thenReturn(new PrdProductResponse());
    productServiceV2.getPrdProductDetailByProductSkuOrProductCode(STORE_ID, productSkuAndProductCodeRequest);
    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(gdnMapper).deepCopy(any(), any());
  }

  @Test
  public void getPrdProductDetailByProductSkuOrProductCodeByProductCodeTest() {
    productSkuAndProductCodeRequest.setProductCode(PRODUCT_CODE);
    List<Product> productList = new ArrayList<>();
    productList.add(new Product());
    Mockito.when(productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productList);
    Mockito.when(this.gdnMapper.deepCopy(any(), any())).thenReturn(new PrdProductResponse());
    productServiceV2.getPrdProductDetailByProductSkuOrProductCode(STORE_ID, productSkuAndProductCodeRequest);
    Mockito.verify(productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(gdnMapper).deepCopy(any(), any());
  }

  @Test
  public void getPrdProductDetailByProductSkuOrProductCodeByEmptyRequestTest() {
    List<PrdProductResponse> response =
        productServiceV2.getPrdProductDetailByProductSkuOrProductCode(STORE_ID, productSkuAndProductCodeRequest);
    Assertions.assertTrue(response.isEmpty());
  }

  @Test
  public void getPrdProductDetailByProductSkuOrProductCodeByNullListTest() {
    productSkuAndProductCodeRequest.setProductCode(PRODUCT_CODE);
    List<Product> productList = new ArrayList<>();
    productList.add(null);
    Mockito.when(productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productList);
    productServiceV2.getPrdProductDetailByProductSkuOrProductCode(STORE_ID, productSkuAndProductCodeRequest);
    Mockito.verify(productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void getPrdProductDetailByProductSkuOrProductCodeByProductCodeListEmptyTest() {
    productSkuAndProductCodeRequest.setProductCode(PRODUCT_CODE);
    List<Product> productList = new ArrayList<>();
    Mockito.when(productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productList);
    productServiceV2.getPrdProductDetailByProductSkuOrProductCode(STORE_ID, productSkuAndProductCodeRequest);
    Mockito.verify(productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void getProductAndItemsForView() throws Exception {
      Mockito.when(skuValidator.isProductSku(PRODUCT_SKU)).thenReturn(true);
    systemParameter.setValue(Boolean.TRUE.toString());
    Mockito.when(productService.getProductForView(STORE_ID,REQUEST_ID,USERNAME,PRODUCT_SKU,true,
        false)).thenReturn(product);
    Mockito.when(itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(STORE_ID,
      PRODUCT_SKU, PICKUP_POINT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(this.itemService.getItemsForViewByProductSkuAndPickUpPoint(eq(STORE_ID),
      eq(PRODUCT_SKU),eq(false), eq(true), eq(false), any())).thenReturn(Collections.singletonList(item));
    Mockito.when(itemService.getItemsWithDiscountAndPickUpPointDetails(eq(STORE_ID), eq(PRODUCT_SKU),
      eq(PICKUP_POINT_CODE), eq(Collections.singletonList(item)), anyList(), anyMap(), eq(true))).thenReturn(Collections.singletonList(item));
    Mockito.when(objectConverterService.constructProductAndItemsForView(eq(product),anyMap(),
      anyMap())).thenReturn(productAndItemsSummaryResponseV2);
      this.productServiceV2.getProductAndItemsForView(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU,
        PICKUP_POINT_CODE, false, true, false, true, false);
    verify(skuValidator).isProductSku(PRODUCT_SKU);
    verify(productService).getProductForView(STORE_ID,REQUEST_ID,USERNAME,PRODUCT_SKU,true,false);
    verify(itemPickupPointService).findItemPickupPointsByProductSkuAndPPcode(STORE_ID,PRODUCT_SKU
      ,PICKUP_POINT_CODE);
    verify(this.itemService).getItemsForViewByProductSkuAndPickUpPoint(eq(STORE_ID),
      eq(PRODUCT_SKU), eq(false), eq(true), eq(false), anyMap());
    verify(itemService).getItemsWithDiscountAndPickUpPointDetails(eq(STORE_ID), eq(PRODUCT_SKU),
      eq(PICKUP_POINT_CODE), eq(Collections.singletonList(item)), anyList(), anyMap(), eq(false));
    verify(objectConverterService).constructProductAndItemsForView(eq(product),anyMap(),anyMap());
  }

  @Test
  public void getProductAndItemsForView_WithSkuValidationException() throws Exception {
    Mockito.when(skuValidator.isProductSku(PRODUCT_SKU)).thenReturn(false);
    systemParameter.setValue(Boolean.TRUE.toString());
    Mockito.when(productService.getProductForView(STORE_ID,REQUEST_ID,USERNAME,PRODUCT_SKU,true,
      false)).thenReturn(product);
    Mockito.when(itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(STORE_ID,
      PRODUCT_SKU, PICKUP_POINT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(this.itemService.getItemsForViewByProductSkuAndPickUpPoint(eq(STORE_ID),
      eq(PRODUCT_SKU),eq(false), eq(true), eq(false), any())).thenReturn(Collections.singletonList(item));
    Mockito.when(itemService.getItemsWithDiscountAndPickUpPointDetails(eq(STORE_ID), eq(PRODUCT_SKU),
      eq(PICKUP_POINT_CODE), eq(Collections.singletonList(item)), anyList(), anyMap(), eq(true))).thenReturn(Collections.singletonList(item));
    Mockito.when(objectConverterService.constructProductAndItemsForView(eq(product),anyMap(),
      anyMap())).thenReturn(productAndItemsSummaryResponseV2);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productServiceV2.getProductAndItemsForView(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU,
        PICKUP_POINT_CODE, false, true, false, true, false));
    }
    finally {
      verify(skuValidator).isProductSku(PRODUCT_SKU);

    }
  }

  @Test
  public void getProductAndItemsForView_WithItemNotFoundException() throws Exception {
    Mockito.when(skuValidator.isProductSku(PRODUCT_SKU)).thenReturn(true);
    systemParameter.setValue(Boolean.TRUE.toString());
    Mockito.when(productService.getProductForView(STORE_ID,REQUEST_ID,USERNAME,PRODUCT_SKU,true,
      false)).thenReturn(product);
    Mockito.when(itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(STORE_ID,
      PRODUCT_SKU, PICKUP_POINT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(this.itemService.getItemsForViewByProductSkuAndPickUpPoint(eq(STORE_ID),
      eq(PRODUCT_SKU),eq(false), eq(true), eq(false), any())).thenReturn(Collections.emptyList());
    Mockito.when(itemService.getItemsWithDiscountAndPickUpPointDetails(eq(STORE_ID), eq(PRODUCT_SKU),
      eq(PICKUP_POINT_CODE), eq(Collections.singletonList(item)), anyList(), anyMap(), eq(true))).thenReturn(Collections.singletonList(item));
    Mockito.when(objectConverterService.constructProductAndItemsForView(eq(product),anyMap(),
      anyMap())).thenReturn(productAndItemsSummaryResponseV2);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productServiceV2.getProductAndItemsForView(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU,
        PICKUP_POINT_CODE, false, true, false, true, false));
    }
    finally {
      verify(skuValidator).isProductSku(PRODUCT_SKU);
      verify(productService).getProductForView(STORE_ID,REQUEST_ID,USERNAME,PRODUCT_SKU,true,false);
      verify(itemPickupPointService).findItemPickupPointsByProductSkuAndPPcode(STORE_ID,PRODUCT_SKU
        ,PICKUP_POINT_CODE);
      verify(this.itemService).getItemsForViewByProductSkuAndPickUpPoint(eq(STORE_ID),
        eq(PRODUCT_SKU), eq(false), eq(true), eq(false), anyMap());
    }
  }

  @Test
  public void getProductAndItemsForView_WithL5NotFoundException() throws Exception {
    Mockito.when(skuValidator.isProductSku(PRODUCT_SKU)).thenReturn(true);
    systemParameter.setValue(Boolean.TRUE.toString());
    Mockito.when(productService.getProductForView(STORE_ID,REQUEST_ID,USERNAME,PRODUCT_SKU,true,
      false)).thenReturn(product);
    Mockito.when(itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(STORE_ID,
      PRODUCT_SKU, PICKUP_POINT_CODE)).thenReturn(Collections.emptyList());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productServiceV2.getProductAndItemsForView(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU,
        PICKUP_POINT_CODE, false, true, false, true, false));
    }
    finally {
      verify(skuValidator).isProductSku(PRODUCT_SKU);
      verify(productService).getProductForView(STORE_ID,REQUEST_ID,USERNAME,PRODUCT_SKU,true,false);
      verify(itemPickupPointService).findItemPickupPointsByProductSkuAndPPcode(STORE_ID,PRODUCT_SKU
        ,PICKUP_POINT_CODE);
    }
  }

  @Test
  public void getBasicProductAndItemDetailsExceptionTest() throws Exception {
    Mockito.when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class, () -> productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, false, false));
    } finally {
      verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
      verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    }
  }

  @Test
  public void getBasicProductAndItemDetailsTest() throws Exception {
    product.setSynchronized(true);
    Mockito.when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    Mockito.when(objectConverterService.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO)).thenReturn(new BasicProductAndItemDTO());
    productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, false, false);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(product, new ArrayList<>(), basicItemDTO);
  }

  @Test
  public void getBasicProductAndItemDetailsUnSyncTest() throws Exception {
    product.setSynchronized(false);
    product.setMasterDataProduct(new MasterDataProduct());
    Mockito.when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    Mockito.when(objectConverterService.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO)).thenReturn(basicProductAndItemDTO);
    productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, false, false);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(product, new ArrayList<>(), basicItemDTO);
  }

  @Test
  public void getBasicProductAndItemDetailsSpecificationNeededTest() throws Exception {
    product.setSynchronized(true);
    product.setMasterDataProduct(new MasterDataProduct());
    basicProductAndItemDTO.setMasterDataProduct(new BasicMasterDataProductDTO());
    Mockito.when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    Mockito.when(objectConverterService.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO)).thenReturn(basicProductAndItemDTO);
    Mockito.when(productCategoryBaseOutbound.getProductAndAttributeDetails(productResponse.getProductCode(), false))
        .thenReturn(new ProductAndAttributeDetailResponse());
    Mockito.when(objectConverterService.convertBytesToString(any())).thenReturn(DESCRIPTION);
    Mockito.when(objectConverterService.convertToMasterDataProductAttribute(any())).thenReturn(new ArrayList<>());
    productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, true, false);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(product, new ArrayList<>(), basicItemDTO);
    verify(objectConverterService).convertBytesToString(any());
    verify(objectConverterService).convertToMasterDataProductAttribute(any());
    verify(productCategoryBaseOutbound).getProductAndAttributeDetails(productResponse.getProductCode(), false);
  }

  @Test
  public void getBasicProductAndItemDetailsSpecificationNeededRemoveHideOnCustomerTest() throws Exception {
    product.setSynchronized(true);
    product.setMasterDataProduct(new MasterDataProduct());
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setHideForCustomer(true);
    productAttributeResponse.setAttribute(attributeResponse);
    productAndAttributeDetailResponse.setProductAttributeResponses(new ArrayList<>(Collections.singletonList(productAttributeResponse)));
    ReflectionTestUtils.setField(productServiceV2, "excludeHideOnCustomerSideAttributesEnabled", true);
    basicProductAndItemDTO.setMasterDataProduct(new BasicMasterDataProductDTO());
    Mockito.when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    Mockito.when(objectConverterService.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO)).thenReturn(basicProductAndItemDTO);
    Mockito.when(productCategoryBaseOutbound.getProductAndAttributeDetails(productResponse.getProductCode(), false))
        .thenReturn(productAndAttributeDetailResponse);
    Mockito.when(objectConverterService.convertBytesToString(any())).thenReturn(DESCRIPTION);
    Mockito.when(objectConverterService.convertToMasterDataProductAttribute(any())).thenReturn(new ArrayList<>());
    productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, true, false);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(product, new ArrayList<>(), basicItemDTO);
    verify(objectConverterService).convertBytesToString(any());
    verify(objectConverterService).convertToMasterDataProductAttribute(any());
    verify(productCategoryBaseOutbound).getProductAndAttributeDetails(productResponse.getProductCode(), false);
  }

  @Test
  public void getBasicProductAndItemDetailsDescriptionNeededTest() throws Exception {
    product.setSynchronized(true);
    product.setMasterDataProduct(new MasterDataProduct());
    Mockito.when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    Mockito.when(objectConverterService.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO)).thenReturn(basicProductAndItemDTO);
    Mockito.when(productCategoryBaseOutbound.getProductBasicDetails(productResponse.getProductCode())).thenReturn(productResponse);
    Mockito.when(objectConverterService.convertBytesToString(any())).thenReturn(DESCRIPTION);
    productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, false, true);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(product, new ArrayList<>(), basicItemDTO);
    verify(objectConverterService).convertBytesToString(any());
    verify(productCategoryBaseOutbound).getProductBasicDetails(productResponse.getProductCode());
  }

  @Test
  public void getPriceRangeForSkusSuccessProductSkuListTest() {
    List<String> productSkuList = new ArrayList<>();
    productSkuList.add(PRODUCT_SKU_1);
    productSkuList.add(PRODUCT_SKU_2);
    Set<String> productSkuSet = new HashSet<>(productSkuList);
    when(productSolrRepository.findByProductSkuList(STORE_ID, MERCHANT_CODE, productSkuSet)).thenReturn(
        constructProductSolr());
    when(skuValidator.isProductSku(anyString())).thenReturn(true);
    List<PriceRangeResponse> priceRangeResponseList =
        productServiceV2.getPriceRangeForSkus(STORE_ID, MERCHANT_CODE, productSkuList);
    verify(skuValidator, times(2)).isProductSku(anyString());
    verify(productSolrRepository).findByProductSkuList(STORE_ID, MERCHANT_CODE, productSkuSet);
    Assertions.assertEquals(PRODUCT_SKU_1, priceRangeResponseList.get(0).getWebSku());
  }

  @Test
  public void getPriceRangeForSkusSuccessProductSkuListEmptyResponseFromDbTest() {
    List<String> productSkuList = new ArrayList<>();
    productSkuList.add(PRODUCT_SKU_1);
    productSkuList.add(PRODUCT_SKU_2);
    Set<String> productSkuSet = new HashSet<>(productSkuList);
    when(productSolrRepository.findByProductSkuList(STORE_ID, MERCHANT_CODE,
        productSkuSet)).thenReturn(new ArrayList<>());
    when(skuValidator.isProductSku(anyString())).thenReturn(true);
    try {
      productServiceV2.getPriceRangeForSkus(STORE_ID, MERCHANT_CODE, productSkuList);
    } catch (Exception e) {
      verify(skuValidator, times(2)).isProductSku(anyString());
      verify(productSolrRepository).findByProductSkuList(STORE_ID, MERCHANT_CODE, productSkuSet);
    }
  }

  @Test
  public void getPriceRangeForSkusSuccessItemSkuListEmptyResponseFromDbTest() {
    List<String> itemSkuList = new ArrayList<>();
    itemSkuList.add(ITEM_SKU_1);
    itemSkuList.add(ITEM_SKU_2);
    Item item1 = new Item();
    item1.setGeneratedItemName(ITEM_SKU);
    Item item2 = new Item();
    item2.setGeneratedItemName(ITEM_SKU);
    List<Item> itemList = new ArrayList<>();
    itemList.add(item1);
    itemList.add(item2);
    Map<String, Item> itemMap = new HashMap<>();
    itemMap.put(ITEM_SKU_1, item1);
    itemMap.put(ITEM_SKU_2, item2);
    Set<String> itemSkuSet = new HashSet<>(itemSkuList);
    List<ItemPickupPoint> itemPickupPointSet = constructItemPickupPointSet();
    for (ItemPickupPoint itemPickupPoint1 : itemPickupPointSet) {
      itemPickupPoint1.setPrice(new HashSet<>());
    }
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        itemSkuList)).thenReturn(itemPickupPointSet);
    when(skuValidator.isProductSku(anyString())).thenReturn(false);
    when(itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuSet))
        .thenReturn(itemList);
    when(cacheItemHelperService.getCacheableItemsByItemSkus(STORE_ID, itemSkuSet))
        .thenReturn(itemMap);
    when(skuValidator.isItemSku(anyString())).thenReturn(true);
    try {
      productServiceV2.getPriceRangeForSkus(STORE_ID, MERCHANT_CODE, itemSkuList);
    } catch (Exception e) {
      verify(skuValidator, times(1)).isProductSku(anyString());
      verify(skuValidator, times(2)).isItemSku(anyString());
      verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList);
      verify(cacheItemHelperService).getCacheableItemsByItemSkus(STORE_ID, itemSkuSet);
    }
  }

  @Test
  public void getPriceRangeForSkusSuccessItemSkuListTest() {
    List<String> itemSkuList = new ArrayList<>();
    itemSkuList.add(ITEM_SKU_1);
    itemSkuList.add(ITEM_SKU_2);
    Set<String> itemSkuSet = new HashSet<>(itemSkuList);
    Item item1 = new Item();
    item1.setGeneratedItemName(ITEM_SKU);
    item1.setItemSku(ITEM_SKU_1);
    Item item2 = new Item();
    item2.setItemSku(ITEM_SKU_2);
    item2.setGeneratedItemName(ITEM_SKU);
    List<Item> itemList = new ArrayList<>();
    itemList.add(item1);
    itemList.add(item2);
    Map<String, Item> itemMap = new HashMap<>();
    itemMap.put(ITEM_SKU_1, item1);
    itemMap.put(ITEM_SKU_2, item2);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        itemSkuList)).thenReturn(constructItemPickupPointSet());
    when(skuValidator.isItemSku(anyString())).thenReturn(true);
    when(itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuSet))
        .thenReturn(itemList);
    when(cacheItemHelperService.getCacheableItemsByItemSkus(STORE_ID, itemSkuSet))
        .thenReturn(itemMap);
    when(skuValidator.isProductSku(anyString())).thenReturn(false);
    List<PriceRangeResponse> priceRangeResponseList =
        productServiceV2.getPriceRangeForSkus(STORE_ID, MERCHANT_CODE, itemSkuList);
    verify(skuValidator, times(2)).isItemSku(anyString());
    verify(skuValidator, times(1)).isProductSku(anyString());
    verify(cacheItemHelperService).getCacheableItemsByItemSkus(STORE_ID, itemSkuSet);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList);
    Assertions.assertEquals(ITEM_SKU_1, priceRangeResponseList.get(0).getWebSku());
  }

  @Test
  public void getPriceRangeForSkusSuccessItemSkuListEmptyResponseWithRepositoryAndCacheMisMatchFromDbTest() {

    List<String> itemSkuList = new ArrayList<>();
    itemSkuList.add(ITEM_SKU_1);
    itemSkuList.add(ITEM_SKU_2);
    Set<String> itemSkuSet = new HashSet<>(itemSkuList);
    Item item1 = new Item();
    item1.setGeneratedItemName(ITEM_SKU);
    item1.setItemSku(ITEM_SKU_1);
    Item item2 = new Item();
    item2.setItemSku(ITEM_SKU_2);
    item2.setGeneratedItemName(ITEM_SKU);
    List<Item> itemList = new ArrayList<>();
    itemList.add(item2);
    Map<String, Item> itemMap = new HashMap<>();
    itemMap.put(ITEM_SKU_1, item1);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        itemSkuList)).thenReturn(constructItemPickupPointSet());
    when(skuValidator.isItemSku(anyString())).thenReturn(true);
    when(itemService.findByStoreIdAndItemSkus(anyString(), anySet()))
        .thenReturn(itemList);
    when(cacheItemHelperService.getCacheableItemsByItemSkus(STORE_ID, itemSkuSet))
        .thenReturn(itemMap);
    when(skuValidator.isProductSku(anyString())).thenReturn(false);
    List<PriceRangeResponse> priceRangeResponseList =
        productServiceV2.getPriceRangeForSkus(STORE_ID, MERCHANT_CODE, itemSkuList);
    verify(skuValidator, times(2)).isItemSku(anyString());
    verify(skuValidator, times(1)).isProductSku(anyString());
    verify(cacheItemHelperService).getCacheableItemsByItemSkus(anyString(), anySet());
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList);
    verify(itemService).findByStoreIdAndItemSkus(anyString(), anySet());
    Assertions.assertEquals(ITEM_SKU_1, priceRangeResponseList.get(0).getWebSku());
  }

  @Test
  public void getPriceRangeForSkusSuccessProductSkuEmptyListTest() {
    List<String> productSkuList = new ArrayList<>();
    productSkuList.add(PRODUCT_SKU_2);
    productSkuList.add(PRODUCT_SKU);
    try {
      productServiceV2.getPriceRangeForSkus(STORE_ID, MERCHANT_CODE, productSkuList);
    } catch (Exception e) {
      verify(skuValidator, times(1)).isProductSku(anyString());
      verify(skuValidator, times(1)).isItemSku(anyString());
    }
  }

  @Test
  public void getHalalProductResponseByProductSkusTest() {
    List<String> productSkus = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    productSkus.add("productSku1");
    Product product1 = new Product();
    product1.setBrand(BRAND);
    product1.setProductCode(PRODUCT_CODE);
    product1.setProductSku(PRODUCT_SKU);
    productList.add(product1);
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkus)).thenReturn(productList);
    List<HalalProductResponse> halalProductResponse =
        productServiceV2.getHalalProductResponseByProductSkus(STORE_ID, productSkus);
    Mockito.verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkus);
    Assertions.assertEquals(1, halalProductResponse.size());
    Assertions.assertEquals(CurationStatus.NONE.name(),halalProductResponse.get(0).getCurationStatus());
    Assertions.assertEquals(false,halalProductResponse.get(0).isHalalProduct());
    Assertions.assertEquals(PRODUCT_CODE,halalProductResponse.get(0).getProductCode());
  }

  @Test
  public void getHalalProductResponseByProductSkusForProductSkusNullTest() {
    List<HalalProductResponse> halalProductResponse =
        productServiceV2.getHalalProductResponseByProductSkus(STORE_ID, null);
    Assertions.assertEquals(0, halalProductResponse.size());
  }

  @Test
  public void getHalalProductResponseByProductSkusForCurationStatusIsApprovedTest() {
    List<String> productSkus = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    productSkus.add("productSku1");
    Product product1 = new Product();
    product1.setBrand(BRAND);
    product1.setProductCode(PRODUCT_CODE);
    product1.setProductSku(PRODUCT_SKU);
    product1.setCurationStatus(CurationStatus.APPROVED);
    productList.add(product1);
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkus)).thenReturn(productList);
    List<HalalProductResponse> halalProductResponse =
        productServiceV2.getHalalProductResponseByProductSkus(STORE_ID, productSkus);
    Mockito.verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkus);
    Assertions.assertEquals(1, halalProductResponse.size());
    Assertions.assertEquals(CurationStatus.APPROVED.name(), halalProductResponse.get(0).getCurationStatus());
    Assertions.assertEquals(true, halalProductResponse.get(0).isHalalProduct());
  }

  @Test
  public void updateHalalConfigOfProductApprovedTest() {
    product.setCurationStatus(CurationStatus.NEED_CURATION);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(productCategoryBaseOutbound.getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    Mockito.doNothing().when(productService).updateSalesCategoryOfProduct(any(), any());
    productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.APPROVED.name(),
        USERNAME);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productService)
        .updateSalesCategoryOfProduct(salesCategoryMappingUpdateRequest.capture(), any());
    Mockito.verify(productService).saveProductHalalConfigAndCaptureHistory(productArgumentCaptor.capture(),
        halalHistoryUpdateEventModelArgumentCaptor.capture(), eq(STORE_ID));
    Assertions.assertEquals(CurationStatus.APPROVED, productArgumentCaptor.getValue().getCurationStatus());
    Assertions.assertEquals(Constants.APPROVE_HALAL_PRODUCT,
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getActivity());
    Assertions.assertEquals(String.valueOf(false),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getPreviousValue());
    Assertions.assertEquals(1, salesCategoryMappingUpdateRequest.getValue().getAddedCategories().size());
  }

  @Test
  public void updateHalalConfigOfProductCurationStatusNullTest() {
    product.setCurationStatus(null);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(productCategoryBaseOutbound.getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    Mockito.doNothing().when(productService).updateSalesCategoryOfProduct(any(), any());
    productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.APPROVED.name(),
        USERNAME);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productService)
        .updateSalesCategoryOfProduct(salesCategoryMappingUpdateRequest.capture(), any());
    Mockito.verify(productService).saveProductHalalConfigAndCaptureHistory(productArgumentCaptor.capture(),
        halalHistoryUpdateEventModelArgumentCaptor.capture(), eq(STORE_ID));
    Assertions.assertEquals(CurationStatus.APPROVED, productArgumentCaptor.getValue().getCurationStatus());
    Assertions.assertEquals(Constants.APPROVE_HALAL_PRODUCT,
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getActivity());
    Assertions.assertEquals(String.valueOf(false),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getPreviousValue());
    Assertions.assertEquals(1, salesCategoryMappingUpdateRequest.getValue().getAddedCategories().size());
  }

  @Test
  public void updateHalalConfigOfProductEditHalalFlagTest() {
    product.setCurationStatus(CurationStatus.REJECTED);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(productCategoryBaseOutbound.getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    Mockito.doNothing().when(productService).updateSalesCategoryOfProduct(any(), any());
    productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.APPROVED.name(),
        USERNAME);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productService)
        .updateSalesCategoryOfProduct(salesCategoryMappingUpdateRequest.capture(), any());
    Mockito.verify(productService).saveProductHalalConfigAndCaptureHistory(productArgumentCaptor.capture(),
        halalHistoryUpdateEventModelArgumentCaptor.capture(), eq(STORE_ID));
    Assertions.assertEquals(CurationStatus.APPROVED, productArgumentCaptor.getValue().getCurationStatus());
    Assertions.assertEquals(Constants.EDIT_HALAL_PRODUCT,
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getActivity());
    Assertions.assertEquals(String.valueOf(false),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getPreviousValue());
    Assertions.assertEquals(String.valueOf(true),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getCurrentValue());
    Assertions.assertEquals(1, salesCategoryMappingUpdateRequest.getValue().getAddedCategories().size());
  }


  @Test
  public void updateHalalConfigOfProductRejectedTest() {
    product.setCurationStatus(CurationStatus.NEED_CURATION);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(productCategoryBaseOutbound.getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    Mockito.doNothing().when(productService).updateSalesCategoryOfProduct(any(), any());
    productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.REJECTED.name(),
        USERNAME);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productService)
        .updateSalesCategoryOfProduct(salesCategoryMappingUpdateRequest.capture(), any());
    Mockito.verify(productService).saveProductHalalConfigAndCaptureHistory(productArgumentCaptor.capture(),
        halalHistoryUpdateEventModelArgumentCaptor.capture(), eq(STORE_ID));
    Assertions.assertEquals(CurationStatus.REJECTED, productArgumentCaptor.getValue().getCurationStatus());
    Assertions.assertEquals(Constants.REJECT_HALAL_PRODUCT,
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getActivity());
    Assertions.assertEquals(String.valueOf(false),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getPreviousValue());
    Assertions.assertEquals(String.valueOf(false),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getCurrentValue());
    Assertions.assertEquals(1, salesCategoryMappingUpdateRequest.getValue().getDeletedCategories().size());
  }

  @Test
  public void updateHalalConfigOfProductRejectingApprovedTest() {
    product.setCurationStatus(CurationStatus.APPROVED);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(productCategoryBaseOutbound.getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    Mockito.doNothing().when(productService).updateSalesCategoryOfProduct(any(), any());
    productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.REJECTED.name(),
        USERNAME);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productService)
        .updateSalesCategoryOfProduct(salesCategoryMappingUpdateRequest.capture(), any());
    Mockito.verify(productService).saveProductHalalConfigAndCaptureHistory(productArgumentCaptor.capture(),
        halalHistoryUpdateEventModelArgumentCaptor.capture(), eq(STORE_ID));
    Assertions.assertEquals(CurationStatus.REJECTED, productArgumentCaptor.getValue().getCurationStatus());
    Assertions.assertEquals(Constants.EDIT_HALAL_PRODUCT,
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getActivity());
    Assertions.assertEquals(String.valueOf(true),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getPreviousValue());
    Assertions.assertEquals(String.valueOf(false),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getCurrentValue());
    Assertions.assertEquals(1, salesCategoryMappingUpdateRequest.getValue().getDeletedCategories().size());
  }

  @Test
  public void updateHalalConfigOfProductInvalidCurationStatusTest() {
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.NONE.name(),
        USERNAME));
  }

  @Test
  public void updateHalalConfigOfProductProductNotFoundTest() {
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.APPROVED.name(),
          USERNAME));
    } finally {
      Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    }
  }

  @Test
  public void updateHalalConfigOfProductWithoutHalalCategoriesTest() {
    product.setCurationStatus(CurationStatus.NEED_CURATION);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    categoryDetailResponse.setHalalSalesCategoryReferences(new ArrayList<>());
    Mockito.when(productCategoryBaseOutbound.getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.APPROVED.name(),
        USERNAME);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productService).saveProductHalalConfigAndCaptureHistory(productArgumentCaptor.capture(),
        halalHistoryUpdateEventModelArgumentCaptor.capture(), eq(STORE_ID));
    Assertions.assertEquals(CurationStatus.APPROVED, productArgumentCaptor.getValue().getCurationStatus());
    Assertions.assertEquals(Constants.APPROVE_HALAL_PRODUCT,
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getActivity());
    Assertions.assertEquals(String.valueOf(false),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getPreviousValue());
  }

  @Test
  public void updateHalalConfigOfProductWithoutHalalCategoriesTest2() {
    product.setCurationStatus(CurationStatus.NEED_CURATION);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    categoryDetailResponse.getHalalSalesCategoryReferences().get(0).setHalalSalesCategoryReference(null);
    Mockito.when(productCategoryBaseOutbound.getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.APPROVED.name(),
        USERNAME);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productService).saveProductHalalConfigAndCaptureHistory(productArgumentCaptor.capture(),
        halalHistoryUpdateEventModelArgumentCaptor.capture(), eq(STORE_ID));
    Assertions.assertEquals(CurationStatus.APPROVED, productArgumentCaptor.getValue().getCurationStatus());
    Assertions.assertEquals(Constants.APPROVE_HALAL_PRODUCT,
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getActivity());
    Assertions.assertEquals(String.valueOf(false),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getPreviousValue());
  }

  @Test
  public void updateHalalConfigOfProductWithoutHalalCategoriesTest3() {
    product.setCurationStatus(CurationStatus.NEED_CURATION);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    categoryDetailResponse.getHalalSalesCategoryReferences().get(0).getHalalSalesCategoryReference().setCatalog(null);
    Mockito.when(productCategoryBaseOutbound.getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.APPROVED.name(),
        USERNAME);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productService).saveProductHalalConfigAndCaptureHistory(productArgumentCaptor.capture(),
        halalHistoryUpdateEventModelArgumentCaptor.capture(), eq(STORE_ID));
    Assertions.assertEquals(CurationStatus.APPROVED, productArgumentCaptor.getValue().getCurationStatus());
    Assertions.assertEquals(Constants.APPROVE_HALAL_PRODUCT,
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getActivity());
    Assertions.assertEquals(String.valueOf(false),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getPreviousValue());
  }

  @Test
  public void updateHalalConfigOfProductWithoutHalalCategoriesTest4() {
    product.setCurationStatus(CurationStatus.NEED_CURATION);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    categoryDetailResponse.getHalalSalesCategoryReferences().get(0).getHalalSalesCategoryReference().setCatalog(null);
    Mockito.when(productCategoryBaseOutbound.getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productServiceV2.updateHalalConfigOfProduct(STORE_ID, product.getProductSku(), CurationStatus.REJECTED.name(),
        USERNAME);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productService).saveProductHalalConfigAndCaptureHistory(productArgumentCaptor.capture(),
        halalHistoryUpdateEventModelArgumentCaptor.capture(), eq(STORE_ID));
    Assertions.assertEquals(CurationStatus.REJECTED, productArgumentCaptor.getValue().getCurationStatus());
    Assertions.assertEquals(Constants.REJECT_HALAL_PRODUCT,
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getActivity());
    Assertions.assertEquals(String.valueOf(false),
        halalHistoryUpdateEventModelArgumentCaptor.getValue().getPreviousValue());
  }

  private List<ItemPickupPoint> constructItemPickupPointSet() {
    Price price = new Price();
    price.setChannel(Constants.DEFAULT);
    price.setListPrice(4.0);

    Price price1 = new Price();
    price1.setChannel(Constants.DEFAULT);
    price1.setListPrice(8.0);

    Set<Price> priceSet = new HashSet<>();
    priceSet.add(price);
    priceSet.add(price1);

    List<ItemPickupPoint> itemPickupPointSet = new ArrayList<>();
    itemPickupPointSet.add(ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_1)
        .price(priceSet)
        .build());
    return itemPickupPointSet;
  }

  private List<ProductSolr> constructProductSolr() {
    List<ProductSolr> productSolrList = new ArrayList<>();
    productSolrList.add(ProductSolr.builder()
        .productSku(PRODUCT_SKU_1)
        .minimumSellingPrice(5.0)
        .maximumSellingPrice(8.0)
        .build());
    productSolrList.add(ProductSolr.builder()
        .productSku(PRODUCT_SKU_2)
        .minimumSellingPrice(7.0)
        .maximumSellingPrice(10.0)
        .build());
    return productSolrList;
  }

  @Test
  public void getBasicProductDetailsTest() {
    product.setProductType(ProductType.REGULAR);
    product.setProductCode(PRODUCT_CODE);
    when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(productRepository.findFirst2ByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE, false))
        .thenReturn(Collections.emptyList());
    BasicProductResponse basicProductDetails = productServiceV2.getBasicProductDetails(STORE_ID, PRODUCT_SKU, true);
    verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(productRepository).findFirst2ByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE, false);
    Assertions.assertEquals(ProductType.REGULAR, basicProductDetails.getProductType());
    Assertions.assertFalse(basicProductDetails.isSharedProduct());
  }
  @Test
  public void getBasicProductDetailsNonNullPreOrderTest() {
    product.setProductType(ProductType.REGULAR);
    product.setProductCode(PRODUCT_CODE);
    product.setPreOrder(PRE_ORDER);
    when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(productRepository.findFirst2ByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE, false))
        .thenReturn(Collections.emptyList());
    BasicProductResponse basicProductDetails = productServiceV2.getBasicProductDetails(STORE_ID, PRODUCT_SKU, true);
    verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(productRepository).findFirst2ByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE, false);
    Assertions.assertEquals(ProductType.REGULAR, basicProductDetails.getProductType());
    Assertions.assertFalse(basicProductDetails.isSharedProduct());
    Assertions.assertNotNull(basicProductDetails.getPreOrder());
  }

  @Test
  public void getBasicProductDetailsTestForSharedProduct() {
    product.setProductType(ProductType.REGULAR);
    product.setProductCode(PRODUCT_CODE);
    Product resultProduct = new Product();
    Product resultProduct1 = new Product();
    resultProduct.setProductCode(PRODUCT_CODE);
    resultProduct1.setProductCode(PRODUCT_CODE);
    List<Product> productResult = Arrays.asList(resultProduct, resultProduct1);
    when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(productRepository.findFirst2ByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE, false))
        .thenReturn(productResult);
    BasicProductResponse basicProductDetails = productServiceV2.getBasicProductDetails(STORE_ID, PRODUCT_SKU, true);
    verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(productRepository).findFirst2ByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE, false);
    Assertions.assertEquals(ProductType.REGULAR, basicProductDetails.getProductType());
    Assertions.assertTrue(basicProductDetails.isSharedProduct());
  }

  @Test
  public void getBasicProductDetailsBundleProductTest() {
    product.setMarkForDelete(true);
    product.setBundleProduct(true);
    product.setProductSku(PRODUCT_SKU);
    product.setProductCode(PRODUCT_CODE);
    when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    BasicProductResponse basicProductResponse = productServiceV2.getBasicProductDetails(STORE_ID, PRODUCT_SKU, false);
    verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Assertions.assertTrue(basicProductResponse.isMarkForDelete());
    Assertions.assertTrue(basicProductResponse.isBundleProduct());
    Assertions.assertEquals(PRODUCT_SKU, basicProductResponse.getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, basicProductResponse.getProductCode());
  }

  @Test
  public void getBasicProductDetailsExceptionTest() {
    when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(null);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> productServiceV2.getBasicProductDetails(STORE_ID, PRODUCT_SKU,
          false));
    } finally {
      verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    }
  }

  @Test
  public void setItemPickupPointDetailsInItemGetAllTrueTest() {
    itemPickupPoint.setItemViewConfig(ALL_VIEW_CONFIGS);
    setItemPickupPointDetailsInItemWithCncFlagUpdate(item, itemPickupPoint, true);
    Assertions.assertEquals(ALL_VIEW_CONFIGS, item.getItemViewConfigs());
  }

  @Test
  public void setItemPickupPointDetailsInItemGetAllFalseTest() {
    itemPickupPoint.setItemViewConfig(ALL_VIEW_CONFIGS);
    setItemPickupPointDetailsInItemWithCncFlagUpdate(item, itemPickupPoint, false);
    Assertions.assertEquals(1, item.getItemViewConfigs().size());
    Assertions.assertEquals(DEFAULT, item.getItemViewConfigs().stream().findFirst().get().getChannel());
  }

  private static Set<ItemViewConfig> getAllViewConfigs() {
    return Set.of(
        ItemViewConfig.builder().channel(CNC).isBuyable(true).isDiscoverable(false).build(),
        ItemViewConfig.builder().channel(DEFAULT).isBuyable(true).isDiscoverable(false).build());
  }

  @Test
  public void testBackFillSpecialAttributesInProduct_AddNewAttribute() {
    String productCode = "productCode1";
    String attributeCode = "attr1";
    String attributeName = "Attribute 1";
    String attributeValue = "Value 1";
    Product product = new Product();
    product.setProductSpecialAttributes(new ArrayList<>());
    product.setProductCode(productCode);
    List<Product> productList = Arrays.asList(product);
    when(productService.findByStoreIdAndProductCode(anyString(), eq(productCode))).thenReturn(productList);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    productServiceV2.backFillSpecialAttributesInProduct(productCode, attributeCode, attributeName, attributeValue);
    ProductSpecialAttribute addedAttribute = product.getProductSpecialAttributes().get(0);
    verify(mandatoryParameterHelper).getStoreId();
    verify(productService).findByStoreIdAndProductCode(Mockito.any(),Mockito.any());
    verify(productRepository).save(Mockito.any());
    verify(cacheEvictHelperService).evictProductData(Mockito.any(),Mockito.any());
    assertEquals(attributeCode, addedAttribute.getAttributeCode());
    assertEquals(attributeName, addedAttribute.getAttributeName());
    assertEquals(attributeValue, addedAttribute.getAttributeValue());
  }

  @Test
  public void backFillSpecialAttributesInProduct_AddNewAttribute_attributeValueIsBlankTest() {
    String productCode = "productCode1";
    String attributeCode = "attr1";
    String attributeName = "Attribute 1";
    String attributeValue = "";
    Product product = new Product();
    product.setProductSpecialAttributes(new ArrayList<>());
    product.setProductCode(productCode);
    List<Product> productList = Arrays.asList(product);
    when(productService.findByStoreIdAndProductCode(anyString(), eq(productCode))).thenReturn(productList);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    productServiceV2.backFillSpecialAttributesInProduct(productCode, attributeCode, attributeName, attributeValue);
    ProductSpecialAttribute addedAttribute = product.getProductSpecialAttributes().get(0);
    verify(mandatoryParameterHelper).getStoreId();
    verify(productService).findByStoreIdAndProductCode(Mockito.any(),Mockito.any());
    verify(productRepository).save(Mockito.any());
    verify(cacheEvictHelperService).evictProductData(Mockito.any(),Mockito.any());
    assertEquals(attributeCode, addedAttribute.getAttributeCode());
    assertEquals(attributeName, addedAttribute.getAttributeName());
    assertEquals("-", addedAttribute.getAttributeValue());
  }

  @Test
  @Disabled
  //Disabling it due to change in flow to update the attribute value when there is an existing
  // value present
  public void testBackFillSpecialAttributesInProduct_DuplicateAttribute() {
    String productCode = "productCode1";
    String attributeCode = "attr1";
    String attributeName = "Attribute 1";
    String attributeValue = "Value 1";
    ProductSpecialAttribute existingAttribute = new ProductSpecialAttribute();
    existingAttribute.setAttributeCode(attributeCode);
    existingAttribute.setAttributeName(attributeName);
    existingAttribute.setAttributeValue(attributeValue);
    Product product = new Product();
    product.setProductSpecialAttributes(new ArrayList<>(Collections.singletonList(existingAttribute)));
    List<Product> productList = Arrays.asList(product);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(productService.findByStoreIdAndProductCode(anyString(), eq(productCode))).thenReturn(productList);
    productServiceV2.backFillSpecialAttributesInProduct(productCode, attributeCode, attributeName, attributeValue);
    verify(mandatoryParameterHelper).getStoreId();
    verify(productService).findByStoreIdAndProductCode(Mockito.any(),Mockito.any());
    assertEquals(1, product.getProductSpecialAttributes().size());
  }

  @Test
  public void testBackFillSpecialAttributesInProduct_updateExistingAttributeValue() {
    String productCode = "productCode1";
    String attributeCode = "attr1";
    String attributeName = "Attribute 1";
    String attributeValue = "Value 1";
    Product product = new Product();
    product.setProductSpecialAttributes(
        List.of(new ProductSpecialAttribute(attributeCode, attributeName, attributeValue + "2")));
    product.setProductCode(productCode);
    Product copyProduct = new Product();
    copyProduct.setProductCode(productCode);
    copyProduct.setProductSpecialAttributes(
        List.of(new ProductSpecialAttribute(attributeCode, attributeName, attributeValue + "2")));
    List<Product> productList = Arrays.asList(product);
    when(productService.findByStoreIdAndProductCode(anyString(), eq(productCode))).thenReturn(productList);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    productServiceV2.backFillSpecialAttributesInProduct(productCode, attributeCode, attributeName, attributeValue);
    ProductSpecialAttribute addedAttribute = product.getProductSpecialAttributes().get(0);
    verify(mandatoryParameterHelper).getStoreId();
    verify(productService).findByStoreIdAndProductCode(Mockito.any(),Mockito.any());
    copyProduct.setProductSpecialAttributes(
        List.of(new ProductSpecialAttribute(attributeCode, attributeName, attributeValue)));
    verify(productRepository).save(Mockito.eq(copyProduct));
    verify(cacheEvictHelperService).evictProductData(Mockito.any(),Mockito.any());
    assertEquals(attributeCode, addedAttribute.getAttributeCode());
    assertEquals(attributeName, addedAttribute.getAttributeName());
    assertEquals(attributeValue, addedAttribute.getAttributeValue());
  }

  @Test
  public void getBasicProductAndItemDetails_WhenSpecificationNeeded_Success() throws Exception {
    when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    when(objectConverterService.convertToBasicProductAndItemDTO(any(), anyList(), any())).thenReturn(basicProductAndItemDTO);
    product.setSynchronized(true);
    product.setProductCode(PRODUCT_CODE);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    List<ProductAttributeResponse> attributeResponses = new ArrayList<>();
    ProductAttributeResponse attributeResponse = new ProductAttributeResponse();
    attributeResponses.add(attributeResponse);
    productAndAttributeDetailResponse.setProductAttributeResponses(attributeResponses);
    productAndAttributeDetailResponse.setDescription("Test Description".getBytes());
    when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, false))
        .thenReturn(productAndAttributeDetailResponse);
    List<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<>();
    when(objectConverterService.convertToMasterDataProductAttribute(productAndAttributeDetailResponse))
        .thenReturn(masterDataProductAttributes);
    when(objectConverterService.convertBytesToString(productAndAttributeDetailResponse.getDescription()))
        .thenReturn("Test Description");
    BasicProductAndItemDTO result = productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID,
        ITEM_SKU, PICKUP_POINT_CODE, true, false);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(any(), anyList(), any());
    verify(productCategoryBaseOutbound).getProductAndAttributeDetails(PRODUCT_CODE, false);
    verify(objectConverterService).convertToMasterDataProductAttribute(productAndAttributeDetailResponse);
    verify(objectConverterService).convertBytesToString(productAndAttributeDetailResponse.getDescription());
    assertEquals(masterDataProductAttributes, result.getMasterDataProductAttributes());
    assertEquals("Test Description", result.getMasterDataProduct().getDescription());
  }

  @Test
  public void getBasicProductAndItemDetails_WhenAttributeResponseListNull_Success() throws Exception {
    ReflectionTestUtils.setField(productServiceV2, "excludeHideOnCustomerSideAttributesEnabled", true);
    when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    when(objectConverterService.convertToBasicProductAndItemDTO(any(), anyList(), any())).thenReturn(basicProductAndItemDTO);
    product.setSynchronized(true);
    product.setProductCode(PRODUCT_CODE);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    productAndAttributeDetailResponse.setProductAttributeResponses(null);
    productAndAttributeDetailResponse.setDescription("Test Description".getBytes());
    when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, false))
        .thenReturn(productAndAttributeDetailResponse);
    List<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<>();
    when(objectConverterService.convertToMasterDataProductAttribute(productAndAttributeDetailResponse))
        .thenReturn(masterDataProductAttributes);
    when(objectConverterService.convertBytesToString(productAndAttributeDetailResponse.getDescription()))
        .thenReturn("Test Description");
    BasicProductAndItemDTO result = productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID,
        ITEM_SKU, PICKUP_POINT_CODE, true, false);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(any(), anyList(), any());
    verify(productCategoryBaseOutbound).getProductAndAttributeDetails(PRODUCT_CODE, false);
    verify(objectConverterService).convertToMasterDataProductAttribute(productAndAttributeDetailResponse);
    verify(objectConverterService).convertBytesToString(productAndAttributeDetailResponse.getDescription());
    assertNull(productAndAttributeDetailResponse.getProductAttributeResponses());
  }

  @Test
  public void getBasicProductAndItemDetails_WhenAttributeResponseNull_Success() throws Exception {
    ReflectionTestUtils.setField(productServiceV2, "excludeHideOnCustomerSideAttributesEnabled", true);
    when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    when(objectConverterService.convertToBasicProductAndItemDTO(any(), anyList(), any())).thenReturn(basicProductAndItemDTO);
    product.setSynchronized(true);
    product.setProductCode(PRODUCT_CODE);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    List<ProductAttributeResponse> attributeResponses = new ArrayList<>();
    attributeResponses.add(null);
    productAndAttributeDetailResponse.setProductAttributeResponses(attributeResponses);
    productAndAttributeDetailResponse.setDescription("Test Description".getBytes());
    when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, false))
        .thenReturn(productAndAttributeDetailResponse);
    List<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<>();
    when(objectConverterService.convertToMasterDataProductAttribute(productAndAttributeDetailResponse))
        .thenReturn(masterDataProductAttributes);
    when(objectConverterService.convertBytesToString(productAndAttributeDetailResponse.getDescription()))
        .thenReturn("Test Description");
    BasicProductAndItemDTO result = productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID,
        ITEM_SKU, PICKUP_POINT_CODE, true, false);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(any(), anyList(), any());
    verify(productCategoryBaseOutbound).getProductAndAttributeDetails(PRODUCT_CODE, false);
    verify(objectConverterService).convertToMasterDataProductAttribute(productAndAttributeDetailResponse);
    verify(objectConverterService).convertBytesToString(productAndAttributeDetailResponse.getDescription());
    assertEquals(1, productAndAttributeDetailResponse.getProductAttributeResponses().size());
    assertNull(productAndAttributeDetailResponse.getProductAttributeResponses().get(0));
  }

  @Test
  public void getBasicProductAndItemDetails_WhenAttributeNull_Success() throws Exception {
    ReflectionTestUtils.setField(productServiceV2, "excludeHideOnCustomerSideAttributesEnabled", true);
    when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    when(objectConverterService.convertToBasicProductAndItemDTO(any(), anyList(), any())).thenReturn(basicProductAndItemDTO);
    product.setSynchronized(true);
    product.setProductCode(PRODUCT_CODE);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    List<ProductAttributeResponse> attributeResponses = new ArrayList<>();
    ProductAttributeResponse responseWithNullAttribute = new ProductAttributeResponse();
    responseWithNullAttribute.setAttribute(null);
    attributeResponses.add(responseWithNullAttribute);
    productAndAttributeDetailResponse.setProductAttributeResponses(attributeResponses);
    productAndAttributeDetailResponse.setDescription("Test Description".getBytes());
    when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, false))
        .thenReturn(productAndAttributeDetailResponse);
    List<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<>();
    when(objectConverterService.convertToMasterDataProductAttribute(productAndAttributeDetailResponse))
        .thenReturn(masterDataProductAttributes);
    when(objectConverterService.convertBytesToString(productAndAttributeDetailResponse.getDescription()))
        .thenReturn("Test Description");
    BasicProductAndItemDTO result = productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID,
        ITEM_SKU, PICKUP_POINT_CODE, true, false);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(any(), anyList(), any());
    verify(productCategoryBaseOutbound).getProductAndAttributeDetails(PRODUCT_CODE, false);
    verify(objectConverterService).convertToMasterDataProductAttribute(productAndAttributeDetailResponse);
    verify(objectConverterService).convertBytesToString(productAndAttributeDetailResponse.getDescription());

    assertEquals(1, productAndAttributeDetailResponse.getProductAttributeResponses().size());
    assertNull(productAndAttributeDetailResponse.getProductAttributeResponses().get(0).getAttribute());
  }

  @Test
  public void getBasicProductAndItemDetailsAttributeResponseIsFalse() throws Exception {
    ReflectionTestUtils.setField(productServiceV2, "excludeHideOnCustomerSideAttributesEnabled", true);
    when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    when(objectConverterService.convertToBasicProductAndItemDTO(any(), anyList(), any())).thenReturn(basicProductAndItemDTO);
    product.setSynchronized(true);
    product.setProductCode(PRODUCT_CODE);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    List<ProductAttributeResponse> attributeResponses = new ArrayList<>();
    ProductAttributeResponse responseWithNullAttribute = new ProductAttributeResponse();
    responseWithNullAttribute.setAttribute(null);
    attributeResponses.add(null);
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(null));
    productAndAttributeDetailResponse.setDescription("Test Description".getBytes());
    when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, false))
        .thenReturn(productAndAttributeDetailResponse);
    List<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<>();
    when(objectConverterService.convertToMasterDataProductAttribute(productAndAttributeDetailResponse))
        .thenReturn(masterDataProductAttributes);
    when(objectConverterService.convertBytesToString(productAndAttributeDetailResponse.getDescription()))
        .thenReturn("Test Description");
    productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID,
            ITEM_SKU, PICKUP_POINT_CODE, true, false);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(any(), anyList(), any());
    verify(productCategoryBaseOutbound).getProductAndAttributeDetails(PRODUCT_CODE, false);
    verify(objectConverterService).convertToMasterDataProductAttribute(any());
    verify(objectConverterService).convertBytesToString(any());
  }

  @Test
  public void getBasicProductAndItemDetails_WhenEmptyList_Success() throws Exception {
    ReflectionTestUtils.setField(productServiceV2, "excludeHideOnCustomerSideAttributesEnabled", true);
    when(itemService.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(basicItemDTO);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(catalogService.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product)).thenReturn(new ArrayList<>());
    when(objectConverterService.convertToBasicProductAndItemDTO(any(), anyList(), any())).thenReturn(basicProductAndItemDTO);
    product.setSynchronized(true);
    product.setProductCode(PRODUCT_CODE);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    productAndAttributeDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productAndAttributeDetailResponse.setDescription("Test Description".getBytes());
    when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, false))
        .thenReturn(productAndAttributeDetailResponse);
    List<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<>();
    when(objectConverterService.convertToMasterDataProductAttribute(productAndAttributeDetailResponse))
        .thenReturn(masterDataProductAttributes);
    when(objectConverterService.convertBytesToString(productAndAttributeDetailResponse.getDescription()))
        .thenReturn("Test Description");
    BasicProductAndItemDTO result = productServiceV2.getBasicProductAndItemDetails(STORE_ID, USERNAME, REQUEST_ID,
        ITEM_SKU, PICKUP_POINT_CODE, true, false);
    verify(itemService).getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(catalogService).getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    verify(objectConverterService).convertToBasicProductAndItemDTO(any(), anyList(), any());
    verify(productCategoryBaseOutbound).getProductAndAttributeDetails(PRODUCT_CODE, false);
    verify(objectConverterService).convertToMasterDataProductAttribute(productAndAttributeDetailResponse);
    verify(objectConverterService).convertBytesToString(productAndAttributeDetailResponse.getDescription());
    assertTrue(productAndAttributeDetailResponse.getProductAttributeResponses().isEmpty());
  }

  @Test
  public void checkOmniChannelSkusInSellerTest() throws Exception {
    DistributionInfoByOmniChannelSkusRequest request = new DistributionInfoByOmniChannelSkusRequest();
    request.setSellerCode(SELLER_CODE);
    request.setOmnichannelSkuCodes(Arrays.asList(OMNICHANNEL_SKU_CODE));

    ValidOmniChannelSkuResponse validOmniChannelSkuResponse = new ValidOmniChannelSkuResponse();
    Map<String, ProductL1AndL2CodeResponse> existingOmniChannelSkusMap = new HashMap<>();

    ProductL1AndL2CodeResponse productL1AndL2CodeResponse = new ProductL1AndL2CodeResponse();
    productL1AndL2CodeResponse.setSkuCode(ITEM_SKU);
    productL1AndL2CodeResponse.setProductCode(PRODUCT_CODE);
    productL1AndL2CodeResponse.setItemName(ITEM_NAME);
    productL1AndL2CodeResponse.setOmniChannelSku(OMNICHANNEL_SKU);

    DistributionItemInfoResponse distributionItemInfoResponse = new DistributionItemInfoResponse();
    productL1AndL2CodeResponse.setDistributionItemInfoResponse(distributionItemInfoResponse);

    List<DimensionsAndUomResponse> dimensionsAndUomResponses = new ArrayList<>();
    DimensionsAndUomResponse dimensionsAndUomResponse = new DimensionsAndUomResponse();
    dimensionsAndUomResponse.setLength(10.5);
    dimensionsAndUomResponse.setWidth(5.0);
    dimensionsAndUomResponse.setHeight(2.0);
    dimensionsAndUomResponse.setWeight(1.5);
    dimensionsAndUomResponse.setUomCode(UOM_CODE);
    dimensionsAndUomResponses.add(dimensionsAndUomResponse);
    productL1AndL2CodeResponse.setDimensionsAndUomResponse(dimensionsAndUomResponses);

    existingOmniChannelSkusMap.put(OMNICHANNEL_SKU_CODE, productL1AndL2CodeResponse);
    validOmniChannelSkuResponse.setExistingOmniChannelSkusAndProductDetailsMap(existingOmniChannelSkusMap);

    Item testItem = new Item();
    testItem.setItemCode(ITEM_SKU);
    testItem.setProductSku(PRODUCT_SKU);
    testItem.setItemSku(ITEM_SKU);
    testItem.setCategoryCode(CATEGORY_CODE);
    testItem.setMainImageUrl(MAIN_IMAGE_URL);
    testItem.setCategoryCode(CATEGORY_CODE);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    productAndAttributeDetailResponse.setProductCode(PRODUCT_CODE);
    List<ProductAttributeResponse> productAttributeResponses = new ArrayList<>();
    ProductAttributeResponse halalAttribute = new ProductAttributeResponse();
    AttributeResponse halalAttr = new AttributeResponse();
    halalAttr.setAttributeCode("halal");
    halalAttr.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    halalAttribute.setAttribute(halalAttr);
    List<ProductAttributeValueResponse> halalValues = new ArrayList<>();
    ProductAttributeValueResponse halalValue = new ProductAttributeValueResponse();
    halalValue.setDescriptiveAttributeValue(HALAL_VALUE);
    halalValues.add(halalValue);
    halalAttribute.setProductAttributeValues(halalValues);
    productAttributeResponses.add(halalAttribute);
    ProductAttributeResponse storageAttribute = new ProductAttributeResponse();
    AttributeResponse storageAttr = new AttributeResponse();
    storageAttr.setAttributeCode("storage");
    storageAttr.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    storageAttribute.setAttribute(storageAttr);
    List<ProductAttributeValueResponse> storageValues = new ArrayList<>();
    ProductAttributeValueResponse storageValue = new ProductAttributeValueResponse();
    storageValue.setDescriptiveAttributeValue(STORAGE_VALUE);
    storageValues.add(storageValue);
    storageAttribute.setProductAttributeValues(storageValues);
    productAttributeResponses.add(storageAttribute);
    productAndAttributeDetailResponse.setProductAttributeResponses(productAttributeResponses);
    when(productCategoryBaseOutbound.checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        eq(true), any(OmniChannelSkuRequest.class))).thenReturn(validOmniChannelSkuResponse);
    when(itemService.findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID,
        Collections.singleton(ITEM_SKU))).thenReturn(Arrays.asList(testItem));
    itemPickupPoint.setInsuredAmount(100);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, true)).thenReturn(
        productAndAttributeDetailResponse);
    CategoryNamesResponse categoryNamesResponse = new CategoryNamesResponse();
    Map<String, String> categoryMap = new HashMap<>();
    categoryMap.put(CATEGORY_CODE, CATEGORY_CODE);
    categoryNamesResponse.setCategoryMap(categoryMap);
    when(productCategoryBaseOutbound.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    ReflectionTestUtils.setField(productServiceV2, "halalAttributesValues", Arrays.asList("halal"));
    ReflectionTestUtils.setField(productServiceV2, "storageAttributesValues", Arrays.asList("storage"));
    DistributionInfoByOmniChannelSkusResponse result =
        productServiceV2.checkOmniChannelSkusInSeller(STORE_ID, REQUEST_ID, USERNAME, request);
    verify(productCategoryBaseOutbound).checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        eq(true), any(OmniChannelSkuRequest.class));
    verify(itemService).findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    verify(productCategoryBaseOutbound).getProductAndAttributeDetails(PRODUCT_CODE, true);
    verify(productCategoryBaseOutbound).getCategoryNames(Mockito.anyList());
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
    assertThat(result).isNotNull();
    assertThat(result.getOmniChannelSkuDetails()).hasSize(1);
    Assertions.assertTrue(result.getOmniChannelSkuDetails().get(0).isDistribution());
    Assertions.assertEquals(100,
        result.getOmniChannelSkuDetails().get(0).getPickupPointResponse().get(0).getInsuredAmount());
    OmniChannelSkuDetailResponse omniChannelSkuDetailResponse = result.getOmniChannelSkuDetails().get(0);
    assertThat(omniChannelSkuDetailResponse.getSkuCode()).isEqualTo(ITEM_SKU);
    assertThat(omniChannelSkuDetailResponse.getItemName()).isEqualTo(ITEM_NAME);
    assertThat(omniChannelSkuDetailResponse.getCategoryCode()).isEqualTo(CATEGORY_CODE);
    assertThat(omniChannelSkuDetailResponse.getOmniChannelSku()).isEqualTo(OMNICHANNEL_SKU);
    assertThat(omniChannelSkuDetailResponse.getProductSku()).isEqualTo(PRODUCT_SKU);
    assertThat(omniChannelSkuDetailResponse.getMainImageUrl()).isEqualTo(MAIN_IMAGE_URL);
    assertThat(omniChannelSkuDetailResponse.getHalal()).isEqualTo(HALAL_VALUE);
    assertThat(omniChannelSkuDetailResponse.getStorage()).isEqualTo(STORAGE_VALUE);

    assertThat(omniChannelSkuDetailResponse.getDimensionsAndUOMResponse()).hasSize(1);
    DimensionsAndUOMResponse dimensionsAndUOMResponse = omniChannelSkuDetailResponse.getDimensionsAndUOMResponse().get(0);
    assertThat(dimensionsAndUOMResponse.getLength()).isEqualTo(10.5);
    assertThat(dimensionsAndUOMResponse.getWidth()).isEqualTo(5.0);
    assertThat(dimensionsAndUOMResponse.getHeight()).isEqualTo(2.0);
    assertThat(dimensionsAndUOMResponse.getWeight()).isEqualTo(1.5);
    assertThat(dimensionsAndUOMResponse.getUomCode()).isEqualTo(UOM_CODE);
  }

  @Test
  public void checkOmniChannelSkusInSeller2Test() throws Exception {
    DistributionInfoByOmniChannelSkusRequest request = new DistributionInfoByOmniChannelSkusRequest();
    request.setSellerCode(SELLER_CODE);
    request.setOmnichannelSkuCodes(Arrays.asList(OMNICHANNEL_SKU_CODE));

    ValidOmniChannelSkuResponse validOmniChannelSkuResponse = new ValidOmniChannelSkuResponse();
    Map<String, ProductL1AndL2CodeResponse> existingOmniChannelSkusMap = new HashMap<>();

    ProductL1AndL2CodeResponse productL1AndL2CodeResponse = new ProductL1AndL2CodeResponse();
    productL1AndL2CodeResponse.setSkuCode(ITEM_SKU);
    productL1AndL2CodeResponse.setProductCode(PRODUCT_CODE);
    productL1AndL2CodeResponse.setItemName(ITEM_NAME);
    productL1AndL2CodeResponse.setOmniChannelSku(OMNICHANNEL_SKU);

    List<DimensionsAndUomResponse> dimensionsAndUomResponses = new ArrayList<>();
    DimensionsAndUomResponse dimensionsAndUomResponse = new DimensionsAndUomResponse();
    dimensionsAndUomResponse.setLength(10.5);
    dimensionsAndUomResponse.setWidth(5.0);
    dimensionsAndUomResponse.setHeight(2.0);
    dimensionsAndUomResponse.setWeight(1.5);
    dimensionsAndUomResponse.setUomCode(UOM_CODE);
    dimensionsAndUomResponses.add(dimensionsAndUomResponse);
    productL1AndL2CodeResponse.setDimensionsAndUomResponse(dimensionsAndUomResponses);

    existingOmniChannelSkusMap.put(OMNICHANNEL_SKU_CODE, productL1AndL2CodeResponse);
    validOmniChannelSkuResponse.setExistingOmniChannelSkusAndProductDetailsMap(existingOmniChannelSkusMap);

    Item testItem = new Item();
    testItem.setItemCode(ITEM_SKU);
    testItem.setProductSku(PRODUCT_SKU);
    testItem.setItemSku(ITEM_SKU);
    testItem.setCategoryCode(CATEGORY_CODE);
    testItem.setMainImageUrl(MAIN_IMAGE_URL);
    testItem.setCategoryCode(CATEGORY_CODE);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    productAndAttributeDetailResponse.setProductCode(PRODUCT_CODE);
    List<ProductAttributeResponse> productAttributeResponses = new ArrayList<>();
    ProductAttributeResponse halalAttribute = new ProductAttributeResponse();
    AttributeResponse halalAttr = new AttributeResponse();
    halalAttr.setAttributeCode("halal");
    halalAttr.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    halalAttribute.setAttribute(halalAttr);
    List<ProductAttributeValueResponse> halalValues = new ArrayList<>();
    ProductAttributeValueResponse halalValue = new ProductAttributeValueResponse();
    halalValue.setDescriptiveAttributeValue(HALAL_VALUE);
    halalValues.add(halalValue);
    halalAttribute.setProductAttributeValues(halalValues);
    productAttributeResponses.add(halalAttribute);
    ProductAttributeResponse storageAttribute = new ProductAttributeResponse();
    AttributeResponse storageAttr = new AttributeResponse();
    storageAttr.setAttributeCode("storage");
    storageAttr.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    storageAttribute.setAttribute(storageAttr);
    List<ProductAttributeValueResponse> storageValues = new ArrayList<>();
    ProductAttributeValueResponse storageValue = new ProductAttributeValueResponse();
    storageValue.setDescriptiveAttributeValue(STORAGE_VALUE);
    storageValues.add(storageValue);
    storageAttribute.setProductAttributeValues(storageValues);
    productAttributeResponses.add(storageAttribute);
    productAndAttributeDetailResponse.setProductAttributeResponses(productAttributeResponses);
    when(productCategoryBaseOutbound.checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        eq(true), any(OmniChannelSkuRequest.class))).thenReturn(validOmniChannelSkuResponse);
    when(itemService.findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID,
        Collections.singleton(ITEM_SKU))).thenReturn(Arrays.asList(testItem));
    itemPickupPoint.setInsuredAmount(100);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, true)).thenReturn(
        productAndAttributeDetailResponse);
    CategoryNamesResponse categoryNamesResponse = new CategoryNamesResponse();
    Map<String, String> categoryMap = new HashMap<>();
    categoryMap.put(CATEGORY_CODE, CATEGORY_CODE);
    categoryNamesResponse.setCategoryMap(categoryMap);
    when(productCategoryBaseOutbound.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    ReflectionTestUtils.setField(productServiceV2, "halalAttributesValues", Arrays.asList("halal"));
    ReflectionTestUtils.setField(productServiceV2, "storageAttributesValues", Arrays.asList("storage"));
    DistributionInfoByOmniChannelSkusResponse result =
        productServiceV2.checkOmniChannelSkusInSeller(STORE_ID, REQUEST_ID, USERNAME, request);
    verify(productCategoryBaseOutbound).checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        eq(true), any(OmniChannelSkuRequest.class));
    verify(itemService).findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    verify(productCategoryBaseOutbound).getProductAndAttributeDetails(PRODUCT_CODE, true);
    verify(productCategoryBaseOutbound).getCategoryNames(Mockito.anyList());
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
    assertThat(result).isNotNull();
    assertThat(result.getOmniChannelSkuDetails()).hasSize(1);
    Assertions.assertTrue(result.getOmniChannelSkuDetails().get(0).isDistribution());
    Assertions.assertEquals(100,
        result.getOmniChannelSkuDetails().get(0).getPickupPointResponse().get(0).getInsuredAmount());
    OmniChannelSkuDetailResponse omniChannelSkuDetailResponse = result.getOmniChannelSkuDetails().get(0);
    assertThat(omniChannelSkuDetailResponse.getSkuCode()).isEqualTo(ITEM_SKU);
    assertThat(omniChannelSkuDetailResponse.getItemName()).isEqualTo(ITEM_NAME);
    assertThat(omniChannelSkuDetailResponse.getCategoryCode()).isEqualTo(CATEGORY_CODE);
    assertThat(omniChannelSkuDetailResponse.getOmniChannelSku()).isEqualTo(OMNICHANNEL_SKU);
    assertThat(omniChannelSkuDetailResponse.getProductSku()).isEqualTo(PRODUCT_SKU);
    assertThat(omniChannelSkuDetailResponse.getMainImageUrl()).isEqualTo(MAIN_IMAGE_URL);
    assertThat(omniChannelSkuDetailResponse.getHalal()).isEqualTo(HALAL_VALUE);
    assertThat(omniChannelSkuDetailResponse.getStorage()).isEqualTo(STORAGE_VALUE);
  }

  @Test
  public void checkOmniChannelSkusInSellerWithNullResponseTest() throws Exception {
    DistributionInfoByOmniChannelSkusRequest request = new DistributionInfoByOmniChannelSkusRequest();
    request.setSellerCode(SELLER_CODE);
    request.setOmnichannelSkuCodes(Arrays.asList(OMNICHANNEL_SKU_CODE));
    when(productCategoryBaseOutbound.checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        eq(true), any(OmniChannelSkuRequest.class))).thenReturn(null);
    DistributionInfoByOmniChannelSkusResponse result =
        productServiceV2.checkOmniChannelSkusInSeller(STORE_ID, REQUEST_ID, USERNAME, request);
    verify(productCategoryBaseOutbound).checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        eq(true), any(OmniChannelSkuRequest.class));
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
    assertThat(result).isNotNull();
    assertThat(result.getOmniChannelSkuDetails()).isEmpty();
  }

  @Test
  public void checkOmniChannelSkusInSellerWithEmptyMapTest() throws Exception {
    DistributionInfoByOmniChannelSkusRequest request = new DistributionInfoByOmniChannelSkusRequest();
    request.setSellerCode(SELLER_CODE);
    request.setOmnichannelSkuCodes(Arrays.asList(OMNICHANNEL_SKU_CODE));
    ValidOmniChannelSkuResponse validOmniChannelSkuResponse = new ValidOmniChannelSkuResponse();
    validOmniChannelSkuResponse.setExistingOmniChannelSkusAndProductDetailsMap(new HashMap<>());
    when(productCategoryBaseOutbound.checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        eq(true), any(OmniChannelSkuRequest.class))).thenReturn(validOmniChannelSkuResponse);
    when(productCategoryBaseOutbound.getCategoryNames(Mockito.anyList())).thenReturn(new CategoryNamesResponse());
    itemPickupPoint.setInsuredAmount(100);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Collections.singletonList(itemPickupPoint));
    DistributionInfoByOmniChannelSkusResponse result =
        productServiceV2.checkOmniChannelSkusInSeller(STORE_ID, REQUEST_ID, USERNAME, request);
    verify(productCategoryBaseOutbound).checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        eq(true), any(OmniChannelSkuRequest.class));
    verify(productCategoryBaseOutbound).getCategoryNames(Mockito.anyList());
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
    assertThat(result).isNotNull();
    assertThat(result.getOmniChannelSkuDetails()).isEmpty();
  }

  @Test
  public void checkOmniChannelSkusInSellerExceptionTest() throws Exception {
    ReflectionTestUtils.setField(productServiceV2, "omniChannelSkuMaxRequestSize", 1);
    DistributionInfoByOmniChannelSkusRequest request = new DistributionInfoByOmniChannelSkusRequest();
    request.setOmnichannelSkuCodes(Arrays.asList(OMNICHANNEL_SKU_CODE, OMNICHANNEL_SKU_CODE));
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> productServiceV2.checkOmniChannelSkusInSeller(STORE_ID, REQUEST_ID, USERNAME, request));
    assertThat(request).isNotNull();
  }

}
