package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.gdn.x.product.model.vo.SimpleItemPickupPointVO;
import com.gdn.x.product.model.vo.SimpleProductAndItemsAndItemPickupPointV0;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.ItemAndItemPickupPointVo;
import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.SimpleItemVO;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;
import com.gdn.x.product.model.vo.SimpleProductAndItemsVO;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductService;

/**
 * Created by govind on 08/08/2018 AD.
 */
public class ProductReindexingServiceImplTest {

  private static final String PRODUCT_CODE = "product-code";

  private static final String PRODUCT_SKU = "product-sku";

  private static final String REQUESTID = "requestid";

  private static final String USERNAME = "username";

  private static final String STORE_ID = "store-id";

  private static final String ITEM_SKU = "item-sku";
  private static final int PAGE = 0;
  private static final int PAGE_SIZE = 10;

  private static final String PICKUP_POINT_CODE = "pickup-point-code";

  private static final String OFFLINE_ITEM_ID = "offline-item-id";
  private static final Pageable pageable = PageRequest.of(0, 10);

  private Item item;
  private OfflineItem offlineItem;
  private ItemPickupPoint itemPickupPoint;
  private OfflineItemDetailVo offlineItemDetailVo;
  private Product product;
  private List<SimpleProductAndItemsVO> productAndItems;
  private SimpleProductAndItemsVO simpleProductAndItemsVO;
  private ItemAndItemPickupPointVo itemAndItemPickupPointVo;
  private SimpleMasterDataDetailWithProductAndItemsV2ResponseVo simpleMasterDataDetailWithProductAndItemsV2ResponseVo;
  private SimpleItemPickupPointVO simpleItemPickupPointVO;
  private SimpleProductAndItemsAndItemPickupPointV0 simpleProductAndItemsAndItemPickupPointV0;

  @InjectMocks
  private ProductReindexingServiceImpl productReindexingServiceImpl;

  @Mock
  private ProductSearchHelperService productSearchHelper;

  @Mock
  private ProductService productService;

  @Mock
  private ItemService itemService;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private SystemParameterServiceImpl systemParameterService;

  private SimpleMasterDataDetailWithProductAndItemsResponseVo
      simpleMasterDataDetailWithProductAndItemsResponseVo;

  @BeforeEach
  public void init() throws IOException {
    initMocks(this);

    simpleMasterDataDetailWithProductAndItemsResponseVo = new SimpleMasterDataDetailWithProductAndItemsResponseVo();

    item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setProductSku(PRODUCT_SKU);

    offlineItem = new OfflineItem();
    offlineItem.setItemSku(OFFLINE_ITEM_ID);
    offlineItem.setListPrice(2.0);
    offlineItem.setOfferPrice(1.0);
    offlineItem.setNewData(false);

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(OFFLINE_ITEM_ID);
    Price price = new Price();
    price.setOfferPrice(1.0);
    price.setListPrice(2.0);
    itemPickupPoint.setPrice(Collections.singleton(price));

    offlineItemDetailVo = new OfflineItemDetailVo();
    offlineItemDetailVo.setItemSku(OFFLINE_ITEM_ID);

    product = new Product();
    product.setStoreId(STORE_ID);
    product.setProductSku(PRODUCT_SKU);

    productAndItems = new ArrayList<>();
    SimpleItemVO simpleItemVO = new SimpleItemVO();
    simpleItemVO.setItemSku(ITEM_SKU);
    simpleProductAndItemsVO = new SimpleProductAndItemsVO();
    simpleProductAndItemsVO.setSimpleItems(Collections.singletonList(simpleItemVO));
    productAndItems.add(simpleProductAndItemsVO);

    simpleMasterDataDetailWithProductAndItemsResponseVo.setProductAndItems(productAndItems);

    itemAndItemPickupPointVo = new ItemAndItemPickupPointVo();
    itemAndItemPickupPointVo.setItem(Arrays.asList(item));
    itemAndItemPickupPointVo.setItemPickupPoints(Arrays.asList(itemPickupPoint));

    simpleMasterDataDetailWithProductAndItemsV2ResponseVo = new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo();
    ReflectionTestUtils.setField(productReindexingServiceImpl, "searchL5ReindexAPIEnabled", Boolean.TRUE);

    this.simpleItemPickupPointVO = new SimpleItemPickupPointVO();
    simpleItemPickupPointVO.setCncActive(true);

    this.simpleProductAndItemsAndItemPickupPointV0 = new SimpleProductAndItemsAndItemPickupPointV0();
    simpleProductAndItemsAndItemPickupPointV0.setItemPickupPoints(Collections.singletonList(this.simpleItemPickupPointVO));

  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productService, productSearchHelper);
    verifyNoMoreInteractions(offlineItemService);
    verifyNoMoreInteractions(itemService);
    verifyNoMoreInteractions(productHelperService);
    verifyNoMoreInteractions(itemPickupPointService);
  }

  @Test public void getMasterDataProductDetailResponseByProductCodesTest() throws Exception {
    Set<String> productCodes = new HashSet();
    productCodes.add(PRODUCT_CODE);
    List<Product> products = new ArrayList<>();
    Product product = new Product();
    products.add(product);
    Mockito.when(this.productService
            .getProductsByProductCodes(STORE_ID, productCodes, CommonConstants.productFields))
        .thenReturn(products);
    Mockito.when(this.productSearchHelper
            .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
                REQUESTID, products, false, null, null))
        .thenReturn(simpleMasterDataDetailWithProductAndItemsResponseVo);
    this.productReindexingServiceImpl
        .getMasterDataProductDetailResponseByProductCodes(STORE_ID, USERNAME, REQUESTID,
            productCodes);
    Mockito.verify(this.productSearchHelper).validateParameters(STORE_ID, productCodes);
    Mockito.verify(this.productService)
        .getProductsByProductCodes(STORE_ID, productCodes, CommonConstants.productFields);
    Mockito.verify(this.productSearchHelper)
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
            REQUESTID, products, false, null, null);
    Mockito.verify(this.productSearchHelper).setSimpleItemCatalogs(STORE_ID, USERNAME, REQUESTID,
        simpleMasterDataDetailWithProductAndItemsResponseVo.getProductAndItems(),
        simpleMasterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts());

  }

  @Test
  public void getMasterDataProductDetailResponseByProductCodes_whenProductsNullTest()
      throws Exception {
    Set<String> productCodes = new HashSet();
    productCodes.add(PRODUCT_CODE);
    try {
      Mockito.when(this.productService
              .getProductsByProductCodes(STORE_ID, productCodes, CommonConstants.productFields))
          .thenReturn(null);
      this.productReindexingServiceImpl
          .getMasterDataProductDetailResponseByProductCodes(STORE_ID, USERNAME, REQUESTID,
              productCodes);
    } catch (ApplicationRuntimeException ex) {
      Mockito.verify(this.productSearchHelper).validateParameters(STORE_ID, productCodes);
      Mockito.verify(this.productService)
          .getProductsByProductCodes(STORE_ID, productCodes, CommonConstants.productFields);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByProductCodes_whenStoreIdNullTest()
      throws Exception {
    Set<String> productCodes = new HashSet();
    productCodes.add(PRODUCT_CODE);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productSearchHelper)
        .validateParameters(null, productCodes);
    try {
      this.productReindexingServiceImpl
          .getMasterDataProductDetailResponseByProductCodes(null, USERNAME, REQUESTID,
              productCodes);
    }catch(ApplicationRuntimeException ex) {
      Mockito.verify(this.productSearchHelper).validateParameters(null, productCodes);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByProductSkusTest() throws Exception {
    Set<String> productSkus = new HashSet();
    productSkus.add(PRODUCT_SKU);
    List<Product> products = new ArrayList<>();
    Product product = new Product();
    products.add(product);
    Mockito.when(this.productService
            .getProductsByProductSkus(STORE_ID, productSkus,  CommonConstants.productFields, false))
        .thenReturn(products);
    Mockito.when(this.productSearchHelper
            .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
                REQUESTID, products, false, null, null))
        .thenReturn(simpleMasterDataDetailWithProductAndItemsResponseVo);
    this.productReindexingServiceImpl
        .getMasterDataProductDetailResponseByProductSkus(STORE_ID, USERNAME, REQUESTID,
            productSkus);
    Mockito.verify(this.productSearchHelper).validateParameters(STORE_ID, productSkus);
    Mockito.verify(this.productService)
        .getProductsByProductSkus(STORE_ID, productSkus,  CommonConstants.productFields, false);
    Mockito.verify(this.productSearchHelper)
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
            REQUESTID, products, false, null, null);
    Mockito.verify(this.productSearchHelper).setSimpleItemCatalogs(STORE_ID, USERNAME, REQUESTID,
        simpleMasterDataDetailWithProductAndItemsResponseVo.getProductAndItems(),
        simpleMasterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts());

  }

  @Test
  public void getMasterDataProductDetailResponseByProductSkus_whenProductsNullTest()
      throws Exception {
    Set<String> productSkus = new HashSet();
    productSkus.add(PRODUCT_SKU);
    try {
      Mockito.when(this.productService
              .getProductsByProductSkus(STORE_ID, productSkus,  CommonConstants.productFields, false))
          .thenReturn(null);
      this.productReindexingServiceImpl
          .getMasterDataProductDetailResponseByProductSkus(STORE_ID, USERNAME, REQUESTID,
              productSkus);
    } catch (ApplicationRuntimeException ex) {
      Mockito.verify(this.productSearchHelper).validateParameters(STORE_ID, productSkus);
      Mockito.verify(this.productService)
          .getProductsByProductSkus(STORE_ID, productSkus,  CommonConstants.productFields, false);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByProductSkus_whenStoreIdNullTest()
      throws Exception {
    Set<String> productSkus = new HashSet();
    productSkus.add(PRODUCT_SKU);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productSearchHelper)
        .validateParameters(null, productSkus);
    try {
      this.productReindexingServiceImpl
          .getMasterDataProductDetailResponseByProductSkus(null, USERNAME, REQUESTID, productSkus);
    } catch (ApplicationRuntimeException ex) {
      Mockito.verify(this.productSearchHelper).validateParameters(null, productSkus);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSkuTest() throws Exception {
    Product product = new Product();
    Mockito.when(this.itemService
        .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false,
            PICKUP_POINT_CODE, false, false)).thenReturn(item);
    Mockito.when(this.productService.getProduct(STORE_ID, item.getProductSku()))
        .thenReturn(product);
    Mockito.when(this.productSearchHelper
            .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
                REQUESTID, Arrays.asList(product), false, Arrays.asList(item), PICKUP_POINT_CODE))
        .thenReturn(simpleMasterDataDetailWithProductAndItemsResponseVo);
    this.productReindexingServiceImpl
        .getMasterDataProductDetailResponseByItemSku(STORE_ID, USERNAME, REQUESTID, ITEM_SKU, false,
            PICKUP_POINT_CODE);
    Mockito.verify(this.productService).getProduct(STORE_ID, item.getProductSku());
    Mockito.verify(this.itemService)
        .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false,
            PICKUP_POINT_CODE, false, false);
    Mockito.verify(this.productSearchHelper)
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
            REQUESTID, Arrays.asList(product), false, Arrays.asList(item), PICKUP_POINT_CODE);
    Mockito.verify(this.productSearchHelper).setSimpleItemCatalogs(STORE_ID, USERNAME, REQUESTID,
        simpleMasterDataDetailWithProductAndItemsResponseVo.getProductAndItems(),
        simpleMasterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts());
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSku_WhenItemNotExistTest() throws Exception {
    Mockito.when(this.itemService
        .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false,
            PICKUP_POINT_CODE, false, false)).thenReturn(null);
    try {
      this.productReindexingServiceImpl
          .getMasterDataProductDetailResponseByItemSku(STORE_ID, USERNAME, REQUESTID, ITEM_SKU,
              false, PICKUP_POINT_CODE);
    } catch (ApplicationRuntimeException e) {
      Mockito.verify(this.itemService)
          .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false,
              PICKUP_POINT_CODE, false, false);
      Mockito.verify(this.itemPickupPointService).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSku_WhenItemIsArchived() throws Exception {
    item.setArchived(true);
    Mockito.when(this.itemService
        .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false,
            PICKUP_POINT_CODE, false, false)).thenReturn(item);
    try {
      this.productReindexingServiceImpl
          .getMasterDataProductDetailResponseByItemSku(STORE_ID, USERNAME, REQUESTID, ITEM_SKU,
              false, PICKUP_POINT_CODE);
    } catch (ApplicationRuntimeException e) {
      Mockito.verify(this.itemService)
          .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false,
              PICKUP_POINT_CODE, false, false);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByL5ItemSku() throws Exception {
    Mockito.when(this.itemService
            .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false, PICKUP_POINT_CODE, false, false))
        .thenReturn(null);
    Mockito.when(this.productService.getProduct(STORE_ID, item.getProductSku())).thenReturn(product);
    Mockito.when(this.productSearchHelper
            .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME, REQUESTID,
                Collections.singletonList(product), false, Collections.singletonList(item), offlineItem.getPickupPointCode()))
        .thenReturn(simpleMasterDataDetailWithProductAndItemsResponseVo);
    Mockito.when(itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(itemPickupPoint);
    Mockito.when(itemService
            .getItem(STORE_ID, REQUESTID, USERNAME, OFFLINE_ITEM_ID, true, true, false, false, PICKUP_POINT_CODE, false, false))
        .thenReturn(item);
    Mockito.when(productService.getProduct(STORE_ID, item.getProductSku())).thenReturn(product);

    SimpleMasterDataDetailWithProductAndItemsResponseVo response =
        this.productReindexingServiceImpl
            .getMasterDataProductDetailResponseByItemSku(STORE_ID, USERNAME, REQUESTID, ITEM_SKU, false,
                PICKUP_POINT_CODE);

    Mockito.verify(this.productService).getProduct(STORE_ID, item.getProductSku());
    Mockito.verify(this.itemService)
        .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false, PICKUP_POINT_CODE, false, false);
    Mockito.verify(this.itemPickupPointService).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(this.itemService)
        .getItem(STORE_ID, REQUESTID, USERNAME, OFFLINE_ITEM_ID, true, true, false, false, PICKUP_POINT_CODE, false, false);
    Mockito.verify(this.productSearchHelper).getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID,
        USERNAME, REQUESTID, Collections.singletonList(product), false,
        Collections.singletonList(item), offlineItem.getPickupPointCode());
    Mockito.verify(this.productSearchHelper).setSimpleItemCatalogs(STORE_ID, USERNAME, REQUESTID,
        simpleMasterDataDetailWithProductAndItemsResponseVo.getProductAndItems(),
        simpleMasterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts());
    Assertions.assertEquals(ITEM_SKU, response.getProductAndItems().get(0).getSimpleItems().get(0).getItemSku());
    Assertions.assertEquals(Boolean.FALSE, response.getProductAndItems().get(0).getSimpleItems().get(0).isSynchronized());
  }

  @Test
  public void getMasterDataProductDetailResponseByL5ItemSku_NotFound() throws Exception {
    Mockito.when(this.itemService
            .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false, PICKUP_POINT_CODE, false, false))
        .thenReturn(null);
    Mockito.when(itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(null);
    try {
      this.productReindexingServiceImpl
          .getMasterDataProductDetailResponseByItemSku(STORE_ID, USERNAME, REQUESTID, ITEM_SKU, false,
              PICKUP_POINT_CODE);
    } catch (ApplicationRuntimeException e) {

      Mockito.verify(this.itemService)
          .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false, PICKUP_POINT_CODE, false, false);
      Mockito.verify(this.itemPickupPointService).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByOfflineItemIdNotFound() throws Exception {
    Mockito.when(this.itemService
            .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false, PICKUP_POINT_CODE, false, false))
        .thenReturn(null);
    Mockito.when(itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(itemPickupPoint);
    Mockito.when(itemService
            .getItem(STORE_ID, REQUESTID, USERNAME, OFFLINE_ITEM_ID, true, true, false, false, PICKUP_POINT_CODE, false, false))
        .thenReturn(null);
    try {
      this.productReindexingServiceImpl
          .getMasterDataProductDetailResponseByItemSku(STORE_ID, USERNAME, REQUESTID, ITEM_SKU, false,
              PICKUP_POINT_CODE);
    } catch (ApplicationRuntimeException ex) {
      Mockito.verify(this.itemService)
          .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false, PICKUP_POINT_CODE, false, false);
      Mockito.verify(this.itemService)
          .getItem(STORE_ID, REQUESTID, USERNAME, OFFLINE_ITEM_ID, true, true, false, false, PICKUP_POINT_CODE, false, false);
      Mockito.verify(this.itemPickupPointService).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSku_WhenProductResultNotExistTest() throws Exception {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setProductSku(PRODUCT_SKU);
    Mockito.when(this.itemService
        .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false,
            PICKUP_POINT_CODE, false, false)).thenReturn(item);
    Mockito.when(this.productService.getProduct(STORE_ID, item.getProductSku()))
        .thenReturn(null);
    try {
      this.productReindexingServiceImpl
          .getMasterDataProductDetailResponseByItemSku(STORE_ID, USERNAME, REQUESTID, ITEM_SKU,
              false, PICKUP_POINT_CODE);
    }catch(ApplicationRuntimeException ex) {
      Mockito.verify(this.productService).getProduct(STORE_ID, item.getProductSku());
      Mockito.verify(this.itemService)
          .getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, true, true, false, false,
              PICKUP_POINT_CODE, false, false);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSku_WhenStoreIdNullTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl
        .getMasterDataProductDetailResponseByItemSku(null, USERNAME, REQUESTID, ITEM_SKU,
            false, PICKUP_POINT_CODE));
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSku_WhenItemSkuNullTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl
        .getMasterDataProductDetailResponseByItemSku(STORE_ID, USERNAME, REQUESTID, null,
            false, PICKUP_POINT_CODE));
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSkuAndPickupPointCodeTest() throws Exception {
    Product product = new Product();
    Mockito.when(this.itemService
        .getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemAndItemPickupPointVo);
    Mockito.when(this.productService.getProductFromDB(STORE_ID, item.getProductSku()))
        .thenReturn(product);
    Mockito.when(this.productSearchHelper
            .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
                REQUESTID, Arrays.asList(product), Arrays.asList(item), Arrays.asList(itemPickupPoint)))
        .thenReturn(new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo());
    this.productReindexingServiceImpl
        .getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUESTID, ITEM_SKU,
            PICKUP_POINT_CODE, null);
    Mockito.verify(this.productService).getProductFromDB(STORE_ID, item.getProductSku());
    Mockito.verify(this.itemService)
        .getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(this.productSearchHelper)
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
            REQUESTID, Arrays.asList(product),  Arrays.asList(item), Arrays.asList(itemPickupPoint));
    Mockito.verify(this.productSearchHelper).setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID,
        simpleMasterDataDetailWithProductAndItemsV2ResponseVo.getProductAndItems(),
        simpleMasterDataDetailWithProductAndItemsV2ResponseVo.getMasterDataProducts());
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSkuAndPickupPointCodeForFetchViewConfig() throws Exception {
    Product product = new Product();
    simpleMasterDataDetailWithProductAndItemsV2ResponseVo.setProductAndItems(Collections.singletonList(simpleProductAndItemsAndItemPickupPointV0));
    Mockito.when(this.itemService
        .getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemAndItemPickupPointVo);
    Mockito.when(this.productService.getProductFromDB(STORE_ID, item.getProductSku()))
        .thenReturn(product);
    Mockito.when(this.productSearchHelper
            .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
                REQUESTID, Arrays.asList(product), Arrays.asList(item), Arrays.asList(itemPickupPoint)))
        .thenReturn(simpleMasterDataDetailWithProductAndItemsV2ResponseVo);
    this.productReindexingServiceImpl
        .getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUESTID, ITEM_SKU,
            PICKUP_POINT_CODE, "CNC");
    Mockito.verify(this.productService).getProductFromDB(STORE_ID, item.getProductSku());
    Mockito.verify(this.itemService)
        .getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(this.productSearchHelper)
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
            REQUESTID, Arrays.asList(product),  Arrays.asList(item), Arrays.asList(itemPickupPoint));
    Mockito.verify(this.productSearchHelper).setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID,
        simpleMasterDataDetailWithProductAndItemsV2ResponseVo.getProductAndItems(),
        simpleMasterDataDetailWithProductAndItemsV2ResponseVo.getMasterDataProducts());
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSkuAndPickupPointCodeItemArchivedTest() throws Exception {
    ReflectionTestUtils.setField(productReindexingServiceImpl, "overrideArchivalFlagAtl4ByL3", true);
    itemAndItemPickupPointVo.getItem().forEach(i -> i.setArchived(true));
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    Mockito.when(this.itemService
        .getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemAndItemPickupPointVo);
    Mockito.when(this.productService.getProductFromDB(STORE_ID, item.getProductSku()))
        .thenReturn(product);
    Mockito.when(this.productSearchHelper
            .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
                REQUESTID, Arrays.asList(product), Arrays.asList(item), Arrays.asList(itemPickupPoint)))
        .thenReturn(new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo());
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo simpleMasterDataDetailWithProductAndItemsV2ResponseVo =
        this.productReindexingServiceImpl.getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
            USERNAME, REQUESTID, ITEM_SKU, PICKUP_POINT_CODE, null);
    Mockito.verify(this.productService).getProductFromDB(STORE_ID, item.getProductSku());
    Mockito.verify(this.itemService)
        .getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(this.productSearchHelper)
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
            REQUESTID, Arrays.asList(product),  Arrays.asList(item), Arrays.asList(itemPickupPoint));
    Mockito.verify(this.productSearchHelper).setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID,
        simpleMasterDataDetailWithProductAndItemsV2ResponseVo.getProductAndItems(),
        simpleMasterDataDetailWithProductAndItemsV2ResponseVo.getMasterDataProducts());
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSkuAndPickupPointCodeL3Test() throws Exception {
    ReflectionTestUtils.setField(productReindexingServiceImpl, "overrideArchivalFlagAtl4ByL3", true);
    itemAndItemPickupPointVo.getItem().forEach(i -> i.setArchived(true));
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    product.setArchived(true);
    Mockito.when(this.itemService
        .getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemAndItemPickupPointVo);
    Mockito.when(this.productService.getProductFromDB(STORE_ID, item.getProductSku()))
        .thenReturn(product);
    try {
      Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl.getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
          USERNAME, REQUESTID, ITEM_SKU, PICKUP_POINT_CODE, null));
    }
    finally {
      Mockito.verify(this.productService).getProductFromDB(STORE_ID, item.getProductSku());
      Mockito.verify(this.itemService).getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSkuAndPickupPointCodeSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(productReindexingServiceImpl, "searchL5ReindexAPIEnabled", Boolean.FALSE);
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo response =
        this.productReindexingServiceImpl.getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
            USERNAME, REQUESTID, ITEM_SKU, PICKUP_POINT_CODE, null);
    Assertions.assertEquals(response, new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo());
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSkuAndPickupPointCodeNullProductTest() throws Exception {
    Mockito.when(this.itemService
        .getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemAndItemPickupPointVo);
    Mockito.when(this.productService.getProductFromDB(STORE_ID, item.getProductSku()))
        .thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl.getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(
          STORE_ID, USERNAME, REQUESTID, ITEM_SKU, PICKUP_POINT_CODE, null));
    } finally {
      Mockito.verify(this.productService).getProductFromDB(STORE_ID, item.getProductSku());
      Mockito.verify(this.itemService)
          .getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    }
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSkuAndPickupPointCode_WhenStoreIdNullTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl
        .getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(null, USERNAME, REQUESTID, ITEM_SKU, PICKUP_POINT_CODE, null));
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSkuAndPickupPointCode_WhenItemSkuNullTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl
        .getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUESTID, null, PICKUP_POINT_CODE, null));
  }

  @Test
  public void getMasterDataProductDetailResponseByItemSkuAndPickupPointCode_WhenPickupPointCodeNullTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl
        .getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUESTID, ITEM_SKU, null, null));
  }

  @Test
  public void getMasterDataProductDetailV2ResponseByProductSkusOrItemSkuTest() throws Exception {
    ReflectionTestUtils.setField(productReindexingServiceImpl, "overrideArchivalFlagAtl4ByL3", true);
    Mockito.when(itemService.getItemAndPickupPointDetails(STORE_ID, Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE)).thenReturn(itemAndItemPickupPointVo);
    Mockito.when(productService.getProductsByProductSkus(eq(STORE_ID), anySet(),eq(CommonConstants.productFields),  eq(false))).thenReturn(Arrays.asList(product));
    Mockito.when(this.productSearchHelper
            .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
                REQUESTID, Arrays.asList(product), Arrays.asList(item), Arrays.asList(itemPickupPoint)))
        .thenReturn(new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo());
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("true");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH)).thenReturn(systemParameter);

    this.productReindexingServiceImpl.getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID,
        USERNAME, REQUESTID, Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE, null);

    verify(itemService).getItemAndPickupPointDetails(STORE_ID, Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE);
    verify(productService).getProductsByProductSkus(eq(STORE_ID), anySet(), eq(CommonConstants.productFields), eq(false));
    Mockito.verify(this.productSearchHelper)
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
            REQUESTID, Arrays.asList(product),  Arrays.asList(item), Arrays.asList(itemPickupPoint));
    Mockito.verify(this.productSearchHelper).setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID,
        simpleMasterDataDetailWithProductAndItemsV2ResponseVo.getProductAndItems(),
        simpleMasterDataDetailWithProductAndItemsV2ResponseVo.getMasterDataProducts());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH);
  }

  @Test
  public void getMasterDataProductDetailV2ResponseByProductSkusOrItemSkuSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(productReindexingServiceImpl, "overrideArchivalFlagAtl4ByL3", false);
    Mockito.when(itemService.getItemAndPickupPointDetails(STORE_ID, Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE)).thenReturn(itemAndItemPickupPointVo);
    Mockito.when(productService.getProductsByProductSkus(eq(STORE_ID), anySet(),eq(CommonConstants.productFields),  eq(false))).thenReturn(Arrays.asList(product));
    Mockito.when(this.productSearchHelper
            .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
                REQUESTID, Arrays.asList(product), Arrays.asList(item), Arrays.asList(itemPickupPoint)))
        .thenReturn(new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo());
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("true");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH)).thenReturn(systemParameter);

    this.productReindexingServiceImpl.getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID,
        USERNAME, REQUESTID, Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE, null);

    verify(itemService).getItemAndPickupPointDetails(STORE_ID, Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE);
    verify(productService).getProductsByProductSkus(eq(STORE_ID), anySet(), eq(CommonConstants.productFields), eq(false));
    Mockito.verify(this.productSearchHelper)
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
            REQUESTID, Arrays.asList(product),  Arrays.asList(item), Arrays.asList(itemPickupPoint));
    Mockito.verify(this.productSearchHelper).setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID,
        simpleMasterDataDetailWithProductAndItemsV2ResponseVo.getProductAndItems(),
        simpleMasterDataDetailWithProductAndItemsV2ResponseVo.getMasterDataProducts());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH);
  }

  @Test
  public void getMasterDataProductDetailV2ResponseByProductSkusOrItemSkuL3ReindexDisabledTest() throws Exception {
    Mockito.when(
        itemService.getItemAndPickupPointDetails(eq(STORE_ID), Mockito.anyList(), eq(Arrays.asList(ITEM_SKU)), eq(false), eq(PAGE),
            eq(PAGE_SIZE))).thenReturn(itemAndItemPickupPointVo);
    Mockito.when(productService.getProductsByProductSkus(eq(STORE_ID), anySet(),eq(CommonConstants.productFields), eq(false)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
            this.productSearchHelper.getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID, USERNAME,
                REQUESTID, Arrays.asList(product), Arrays.asList(item), Arrays.asList(itemPickupPoint)))
        .thenReturn(new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo());
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("false");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH)).thenReturn(systemParameter);

    this.productReindexingServiceImpl.getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID, USERNAME,
        REQUESTID, Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE, null);

    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH);
  }

  @Test
  public void getMasterDataProductDetailV2ResponseByProductSkusOrItemSku_WhenEmptyProductsTest() throws Exception {
    Mockito.when(itemService.getItemAndPickupPointDetails(STORE_ID, Arrays.asList(PRODUCT_SKU), null, false, PAGE, PAGE_SIZE)).thenReturn(itemAndItemPickupPointVo);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("true");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH)).thenReturn(systemParameter);
    try {
      Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl.getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID,
          USERNAME, REQUESTID, Arrays.asList(PRODUCT_SKU), null, false, PAGE, PAGE_SIZE, null));
    } finally {
      verify(itemService).getItemAndPickupPointDetails(STORE_ID, Arrays.asList(PRODUCT_SKU), null, false, PAGE, PAGE_SIZE);
      verify(productService).getProductsByProductSkus(eq(STORE_ID), anySet(), eq(CommonConstants.productFields), eq(false));
    }
  }

  @Test
  public void getMasterDataProductDetailV2ResponseByProductSkusOrItemSku_WhenEmptyProductsAndSkusTest() throws Exception {
    Mockito.when(itemService.getItemAndPickupPointDetails(STORE_ID, null, Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE)).thenReturn(itemAndItemPickupPointVo);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("true");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH)).thenReturn(systemParameter);
    try {
      Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl.getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID,
          USERNAME, REQUESTID, null, Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE, null));
    } finally {
      verify(itemService).getItemAndPickupPointDetails(STORE_ID, null, Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE);
      verify(productService).getProductsByProductSkus(eq(STORE_ID), anySet(),  eq(CommonConstants.productFields), eq(false));
      Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH);
    }
  }

  @Test
  public void getMasterDataProductDetailV2ResponseByProductSkusOrItemSku_WhenStoreIdNullTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl
        .getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(null, USERNAME, REQUESTID, Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE, null));
  }

  @Test
  public void ggetMasterDataProductDetailV2ResponseByProductSkusOrItemSku_WhenItemSkuAndProductSkuNullTest() throws Exception {
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("true");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH)).thenReturn(systemParameter);
    Assertions.assertThrows(Exception.class, () -> this.productReindexingServiceImpl
        .getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID, USERNAME, REQUESTID, null, null, false, PAGE, PAGE_SIZE, null));
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH);
  }

  @Test
  public void getOfflineItemsByItemSkuTest() throws Exception {
    item.setCncActivated(true);
    item.setOfflineItems(Collections.singletonList(offlineItemDetailVo));
    offlineItem.setMarkForDelete(true);
    Mockito.when(itemService.getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, false)).thenReturn(item);
    Mockito.when(itemPickupPointService
            .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false, pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(itemPickupPoint), pageable, 1));
    Page<OfflineItemDetailVo> offlineItemPage =
        productReindexingServiceImpl.getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUESTID, ITEM_SKU, pageable);
    Mockito.verify(itemService).getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, false);
    Mockito.verify(productHelperService).constructOfflineItem(item, offlineItem);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false, pageable);
    Assertions.assertEquals(1, offlineItemPage.getTotalElements());
    Assertions.assertEquals(item.getOfflineItems(), offlineItemPage.getContent());
  }

  @Test
  public void getOfflineItemsByItemSku_ItemSkuEmptyTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> productReindexingServiceImpl.getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUESTID, null, pageable));
  }

  @Test
  public void getOfflineItemsByItemSkuNonCnCItemsTest() throws Exception {
    item.setOfflineItems(Collections.singletonList(offlineItemDetailVo));
    Mockito.when(itemService.getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, false)).thenReturn(item);
    Exception exception = null;
    try {
      productReindexingServiceImpl.getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUESTID, ITEM_SKU, pageable);
    } catch (Exception e) {
      exception = e;
    } finally {
      Mockito.verify(itemService).getItem(STORE_ID, REQUESTID, USERNAME, ITEM_SKU, false);
      Assertions.assertEquals(ApplicationRuntimeException.class, exception.getClass());
    }
  }
}