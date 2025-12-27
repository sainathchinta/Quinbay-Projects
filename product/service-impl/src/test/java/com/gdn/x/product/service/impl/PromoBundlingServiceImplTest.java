package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuDetailResponse;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.ActiveComboRequestVO;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.ComboDetailVo;
import com.gdn.x.product.model.vo.ComboItemVO;
import com.gdn.x.product.model.vo.ComboResponseVO;
import com.gdn.x.product.model.vo.ComboRuleVO;
import com.gdn.x.product.model.vo.ComboVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.x.product.outbound.api.ProductPricingOutbound;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.promotion.enums.PromoBundlingType;
import com.gdn.x.promotion.rest.web.model.dto.request.SimpleSetStringRequest;
import com.gdn.x.promotion.rest.web.model.promo.bundling.WholesaleRule;
import com.gdn.x.promotion.rest.web.model.promo.bundling.response.PromoBundlingDetailResponse;
import com.google.common.collect.ImmutableSet;

/**
 * Created by w.william on 3/9/2018.
 */
public class PromoBundlingServiceImplTest {

  private static final String STORE_ID = "store-id";

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "request-id";

  private static final String CLIENT_ID = "client-id";

  private static final String CHANNEL_WEB = ChannelName.DESKTOP_WEB.toString();

  private static final String CHANNEL_DEFAULT = ChannelName.DEFAULT.toString();

  private static final String ITEM_SKU = "item-sku";

  private static final String ITEM_SKU2 = "item-sku-2";

  private static final String ITEM_SKU_3= "item-sku-3";

  private static final String PICKUP_POINT_CODE = "pickup-point-code";

  private static final String PICKUP_POINT_CODE_2 = "pickup-point-code-2";

  private static final String PICKUP_POINT_CODE_3 = "pickup-point-code-3";

  private static final String ITEM_CODE = "item-code";

  private static final boolean IS_SYNCHRONIZED = true;

  private static final String PRISTINE_ID = "pristine-id";

  private static final String PRODUCT_NAME = "product-name";

  private static final Double DISCOUNT_PERCENTAGE = 10.0;

  private static final String PRODUCT_SKU = "product-sku";

  private static final int PAGE = 0;

  private static final int SIZE = 10;

  private static final String SORT_BY = "name";

  private static final String SORT_TYPE = "ASC";

  private static final String VALUE = "value";

  private static final String CURRENCY = "currency";

  private static final String MERCHANT_CODE = "merchant-code";

  private static final String PRODUCT_CODE = "product-code";

  private static final String ATTRIBUTE_CODE = "attributeCode";

  private static final String ATTRIBUTE_NAME = "attributeName";

  private static final String ATTRIBUTE_VALUE = "attributeValue";

  private static final String PROMO_BUNDLING_NAME = "promoBundlingTest";

  private static final String PROMO_BUNDLING_ID = "promoBundlingId";

  private static final String PROMO_BUNDLING_TYPE = "COMBO";

  private static final String PROMO_BUNDLING_TYPE_WHOLESALE = "WHOLESALE";

  private static final int QUANTITY = 1;

  private SimpleSetStringRequest simpleSetStringRequest;
  private ActivePromoBundlingResponseVO activePromoBundlingResponseVO;
  private Item item;
  private Product product;
  private PristineDataItem pristineDataItem;
  private MandatoryRequestParam mandatoryRequestParam;
  private ComboVO comboVO;
  private PromoBundlingDetailResponseVO promoBundlingDetailResponseVO;
  private MasterDataItem masterDataItem;
  private Price price;
  private DiscountPrice discountPrice;
  private ComboItemVO comboItemVO;
  private MasterDataProduct masterDataProduct;
  private MasterDataAttribute masterDataAttribute;
  private MasterDataItemAttributeValue itemAttributeValue;
  private MasterDataItemAttributeValue masterDataItemAttributeValue;
  private ComboRuleVO comboRule;
  private ComboRuleVO comboRule2;
  private ProductAttribute productAttribute;

  private List<MasterDataItemAttributeValue> masterDataItemAttributeValueList;
  private List<MasterDataItemAttributeValue> itemAttributeValues;
  private List<DiscountPrice> discountPrices;

  private List<ProductAttribute> productAttributes;
  private List<PromoBundlingDetailResponseVO> promoBundlingDetailResponseVOs;
  private List<PromoBundlingDetailResponse> promoBundlingDetailResponses;

  private ComboResponseVO comboResponseVO;
  private List<ComboItemVO> comboItemVOList;
  private List<ComboVO> comboVOList;
  private List<ComboRuleVO> comboRuleList;
  private List<String> level2Ids;

  private Set<String> itemCodes;
  private Set<String> itemSkus;
  private Set<Price> prices;
  private Set<String> pristineIdSet;

  private ActiveComboRequestVO activeComboRequestVO;
  PromoBundlingByItemSkuAndItemCodesResponseVO promoBundlingByItemSkuAndItemCodesResponseVO;

  private boolean isPristine;

  @InjectMocks
  private PromoBundlingServiceImpl promoBundlingServiceImpl;

  @Mock
  private PristineItemRepository pristineItemRepository;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private PromotionOutbound promotionOutbound;

  @Mock
  private ObjectConverterServiceImpl objectConverterService;

  @Mock
  private ProductServiceImpl productService;

  @Mock
  private ItemPriceServiceImpl itemPriceService;

  @Mock
  private ChannelServiceImpl channelService;

  @Mock
  private ItemServiceImpl itemService;

  @Mock
  private PristineServiceImpl pristineService;

  @Mock
  private ProductPricingOutbound productPricingOutbound;

  @Mock
  private SystemParameterService systemParameterService;

  @BeforeEach
  public void setUp() throws Exception {

    openMocks(this);

    when(this.channelService.getDefaultChannel())
        .thenReturn(PromoBundlingServiceImplTest.CHANNEL_DEFAULT);

    this.discountPrice = new DiscountPrice();
    this.discountPrice.setDiscountPrice(5000.0);

    this.discountPrices = new ArrayList<>();
    this.discountPrices.add(discountPrice);

    this.price = new Price();
    this.price.setChannel(PromoBundlingServiceImplTest.CHANNEL_DEFAULT);
    this.price.setOfferPrice(100000.0);
    this.price.setListPrice(10000.0);
    this.price.setListOfDiscountPrices(discountPrices);
    this.price.setCurrency(CURRENCY);

    this.prices = new HashSet<>();
    this.prices.add(this.price);

    this.masterDataAttribute = new MasterDataAttribute();

    this.itemAttributeValue = new MasterDataItemAttributeValue();
    this.itemAttributeValue.setAttributeValue(PromoBundlingServiceImplTest.VALUE);
    this.itemAttributeValue.setMasterDataAttribute(this.masterDataAttribute);

    this.itemAttributeValues = new ArrayList<MasterDataItemAttributeValue>();
    this.itemAttributeValues.add(this.itemAttributeValue);

    this.masterDataItem = new MasterDataItem();
    this.masterDataItem.setMasterDataItemAttributeValues(this.itemAttributeValues);

    this.item = new Item();
    this.item.setItemSku(PromoBundlingServiceImplTest.ITEM_SKU);
    this.item.setProductSku(PromoBundlingServiceImplTest.PRODUCT_SKU);
    this.item.setItemCode(PromoBundlingServiceImplTest.ITEM_CODE);
    this.item.setSynchronized(PromoBundlingServiceImplTest.IS_SYNCHRONIZED);
    this.item.setPrice(this.prices);
    this.item.setMasterDataItem(this.masterDataItem);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    this.item.setPristineDataItem(pristineDataItem);

    this.itemSkus = new HashSet<String>();

    this.level2Ids = new ArrayList<String>();
    this.level2Ids.add(PromoBundlingServiceImplTest.ITEM_SKU);

    this.productAttribute = new ProductAttribute();
    this.productAttribute.setItemSku(PromoBundlingServiceImplTest.ITEM_SKU);
    this.productAttributes = new ArrayList<ProductAttribute>();
    this.productAttributes.add(this.productAttribute);

    this.product = new Product();
    this.product.setProductSku(PromoBundlingServiceImplTest.PRODUCT_SKU);
    this.product.setMerchantCode(PromoBundlingServiceImplTest.MERCHANT_CODE);
    this.product.setDefiningAttributes(this.productAttributes);
    this.product.setProductCode(PromoBundlingServiceImplTest.PRODUCT_CODE);

    this.activeComboRequestVO = new

        ActiveComboRequestVO();

    this.itemCodes = new HashSet<>();
    this.itemSkus = new HashSet<>();
    this.pristineIdSet = new HashSet<>();

    mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_WEB, CLIENT_ID, REQUEST_ID, USERNAME, null);

    this.comboRule = new ComboRuleVO();
    this.comboRule.setItemSku(ITEM_SKU);
    this.comboRule.setMainSku(true);
    this.comboRule.setDiscountPercentage(DISCOUNT_PERCENTAGE);

    this.comboRule2 = new ComboRuleVO();
    this.comboRule2.setItemSku(ITEM_SKU2);
    this.comboRule2.setMainSku(false);
    this.comboRule2.setDiscountPercentage(DISCOUNT_PERCENTAGE);

    this.comboRuleList = new ArrayList<>();
    this.comboRuleList.add(comboRule);
    this.comboRuleList.add(comboRule2);

    this.promoBundlingDetailResponseVO = new PromoBundlingDetailResponseVO();
    this.promoBundlingDetailResponseVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    this.promoBundlingDetailResponseVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    this.promoBundlingDetailResponseVO.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    this.promoBundlingDetailResponseVO.setComboRules(comboRuleList);

    this.promoBundlingDetailResponseVOs = new ArrayList<>();
    this.promoBundlingDetailResponseVOs.add(promoBundlingDetailResponseVO);

    this.comboResponseVO = new ComboResponseVO();
    this.comboResponseVO.setComboList(comboVOList);

    this.promoBundlingDetailResponses = new ArrayList<>();

    this.activePromoBundlingResponseVO = new ActivePromoBundlingResponseVO();
    this.activePromoBundlingResponseVO.setPromoBundlingDetailResponseVOList(promoBundlingDetailResponseVOs);

    this.masterDataAttribute = new MasterDataAttribute();
    this.masterDataAttribute.setAttributeCode(ATTRIBUTE_CODE);
    this.masterDataAttribute.setAttributeName(ATTRIBUTE_NAME);

    this.masterDataItemAttributeValue = new MasterDataItemAttributeValue();

    this.masterDataItemAttributeValue.setAttributeValue(ATTRIBUTE_VALUE);
    this.masterDataItemAttributeValue.setMasterDataAttribute(this.masterDataAttribute);

    this.masterDataItemAttributeValueList = new ArrayList<>();
    this.masterDataItemAttributeValueList.add(this.masterDataItemAttributeValue);

    this.masterDataItem = new MasterDataItem();

    this.masterDataItem.setMasterDataItemAttributeValues(this.masterDataItemAttributeValueList);

    this.pristineDataItem = new PristineDataItem();

    this.pristineDataItem.setPristineId(PRISTINE_ID);

    this.comboItemVO = new ComboItemVO();

    this.comboItemVO.setProductName(PRODUCT_NAME);
    this.comboItemVO.setItemCode(ITEM_CODE);
    this.comboItemVO.setItemSku(ITEM_SKU);
    this.comboItemVO.setQuantity(QUANTITY);
    this.comboItemVO.setDiscountPercentage(10.0);
    this.comboItemVO.setProductCode(PRODUCT_CODE);
    this.comboItemVO.setProductSku(PRODUCT_SKU);

    this.comboItemVOList = new ArrayList<>();
    this.comboItemVOList.add(this.comboItemVO);

    this.comboVO = new ComboVO();
    this.comboVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    this.comboVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    this.comboVO.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    this.comboVO.setComboItems(comboItemVOList);

    this.comboVOList = new ArrayList<>();
    this.comboVOList.add(comboVO);

    promoBundlingByItemSkuAndItemCodesResponseVO = new PromoBundlingByItemSkuAndItemCodesResponseVO();
    promoBundlingByItemSkuAndItemCodesResponseVO.setPromoBundling(promoBundlingDetailResponseVO);
    promoBundlingByItemSkuAndItemCodesResponseVO.setTotalComboRule(1);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.channelService);
    verifyNoMoreInteractions(this.itemPriceService);
    verifyNoMoreInteractions(this.itemRepository);
    verifyNoMoreInteractions(this.promotionOutbound);
    verifyNoMoreInteractions(this.itemService);
  }

  @Test
  public void getPromoBundlingByPristineIdOrItemSkuOrItemCode_successWithPristineId()
      throws Exception {
    activeComboRequestVO.setPristineId(PRISTINE_ID);
    itemCodes.add(ITEM_CODE);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemCodes);

    itemSkus.add("item-sku");

    List<Item> items = new ArrayList<>();
    this.item.getMasterDataItem().setGeneratedItemName(PRODUCT_NAME);
    items.add(item);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(Arrays.asList(item));

    when(this.itemService.getItemCodesByPristineId(STORE_ID, PRISTINE_ID)).thenReturn(itemCodes);
    when(this.promotionOutbound
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes))
        .thenReturn(activePromoBundlingResponseVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO))
        .thenReturn(comboVO);
    when(this.productService.getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false))
    .thenReturn(Arrays.asList(productAndItemsVO));
    when(this.objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
            .thenReturn(comboItemVOList);
    when(this.itemPriceService.getFinalPrice(price,
        DISCOUNT_PERCENTAGE)).thenReturn(85000.0);

    comboResponseVO = promoBundlingServiceImpl
        .getActiveCombos(STORE_ID, CHANNEL_WEB, CLIENT_ID,
            REQUEST_ID, USERNAME, activeComboRequestVO, PAGE, SIZE, SORT_BY, SORT_TYPE);

    verify(this.itemService).getItemCodesByPristineId(STORE_ID, PRISTINE_ID);
    verify(this.promotionOutbound)
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO);
    verify(this.objectConverterService)
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());
    verify(this.objectConverterService).convertItemAndProductToComboItemVO(comboItemVO, item, product);
    verify(this.objectConverterService).convertProductToComboItemVO(comboItemVO, product);
    verify(this.
        productService)
        .getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false);
    verify(this.itemPriceService).getFinalPrice(price, DISCOUNT_PERCENTAGE);

    assertNotNull(comboItemVOList);
    assertEquals(1, comboItemVOList.size());
  }

  @Test
  public void getPromoBundlingByPristineIdOrItemSkuOrItemCode_successWithItemCodes()
      throws Exception {
    activeComboRequestVO.setItemCode(ITEM_CODE);
    itemCodes.add(ITEM_CODE);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemCodes);

    itemSkus.add("item-sku");

    List<Item> items = new ArrayList<>();
    this.item.getMasterDataItem().setGeneratedItemName(PRODUCT_NAME);
    items.add(item);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(Arrays.asList(item));

    when(this.promotionOutbound
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes))
        .thenReturn(activePromoBundlingResponseVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO))
        .thenReturn(comboVO);
    when(this.productService.getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false))
        .thenReturn(Arrays.asList(productAndItemsVO));
    when(this.objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
        .thenReturn(comboItemVOList);
    when(this.itemPriceService.getFinalPrice(price, DISCOUNT_PERCENTAGE)).thenReturn(85000.0);

    comboResponseVO = promoBundlingServiceImpl
        .getActiveCombos(STORE_ID, CHANNEL_WEB, CLIENT_ID,
            REQUEST_ID, USERNAME, activeComboRequestVO, PAGE, SIZE, SORT_BY, SORT_TYPE);

    verify(this.promotionOutbound)
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO);
    verify(this.objectConverterService)
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());
    verify(this.objectConverterService).convertItemAndProductToComboItemVO(comboItemVO, item, product);
    verify(this.objectConverterService).convertProductToComboItemVO(comboItemVO, product);
    verify(this.
        productService)
        .getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false
        );
    verify(this.itemPriceService).getFinalPrice(price, DISCOUNT_PERCENTAGE);

    assertNotNull(comboItemVOList);
    assertEquals(1, comboItemVOList.size());
  }

  @Test
  public void getPromoBundlingByPristineIdOrItemSkuOrItemCode_successWithItemSkus()
      throws Exception {
    activeComboRequestVO.setItemSku(ITEM_SKU);
    itemSkus.add(ITEM_SKU);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemSkus);

    List<Item> items = new ArrayList<>();
    this.item.getMasterDataItem().setGeneratedItemName(PRODUCT_NAME);
    items.add(item);
    this.item.setPrice(prices);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(items);

    when(this.itemService.getItemCodesByPristineId(STORE_ID, PRISTINE_ID)).thenReturn(itemCodes);
    when(this.itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false))
        .thenReturn(this.item);
    when(this.promotionOutbound
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes)).thenReturn(activePromoBundlingResponseVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO))
        .thenReturn(comboVO);
    when(this.productService.getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false))
        .thenReturn(Arrays.asList(productAndItemsVO));
    when(this.objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
        .thenReturn(comboItemVOList);
    when(this.itemPriceService.getFinalPrice(price, DISCOUNT_PERCENTAGE)).thenReturn(85000.0);

    comboResponseVO = promoBundlingServiceImpl
        .getActiveCombos(STORE_ID, CHANNEL_WEB, CLIENT_ID,
            REQUEST_ID, USERNAME, activeComboRequestVO, PAGE, SIZE, SORT_BY, SORT_TYPE);

    verify(this.itemService).getItemCodesByPristineId(STORE_ID, PRISTINE_ID);
    verify(this.itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false);
    verify(this.promotionOutbound)
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO);
    verify(this.objectConverterService)
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());
    verify(this.objectConverterService).convertItemAndProductToComboItemVO(comboItemVO, item, product);
    verify(this.objectConverterService).convertProductToComboItemVO(comboItemVO, product);
    verify(this.
        productService)
        .getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false);
    verify(this.itemPriceService).getFinalPrice(price, DISCOUNT_PERCENTAGE);

    assertNotNull(comboItemVOList);
    assertEquals(1, comboItemVOList.size());
  }

  @Test
  public void getPromoBundlingByPristineIdOrItemSkuOrItemCode_successWithItemSkusAndSynchronizedFalse()
      throws Exception {
    activeComboRequestVO.setItemSku(ITEM_SKU);
    itemSkus.add(ITEM_SKU);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemSkus);

    List<Item> items = new ArrayList<>();
    this.item.getMasterDataItem().setGeneratedItemName(PRODUCT_NAME);
    this.item.setSynchronized(false);
    items.add(item);
    this.item.setPrice(prices);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(items);

    when(this.itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false))
        .thenReturn(this.item);
    when(this.promotionOutbound
        .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.COMBO,
            itemSkus)).thenReturn(activePromoBundlingResponseVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO))
        .thenReturn(comboVO);
    when(this.productService.getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false))
        .thenReturn(Arrays.asList(productAndItemsVO));
    when(this.objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
        .thenReturn(comboItemVOList);
    when(this.itemPriceService.getFinalPrice(price, DISCOUNT_PERCENTAGE)).thenReturn(85000.0);

    comboResponseVO = promoBundlingServiceImpl
        .getActiveCombos(STORE_ID, CHANNEL_WEB, CLIENT_ID,
            REQUEST_ID, USERNAME, activeComboRequestVO, PAGE, SIZE, SORT_BY, SORT_TYPE);

    verify(this.itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false);
    verify(this.promotionOutbound)
        .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.COMBO,
            itemSkus);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO);
    verify(this.objectConverterService)
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());
    verify(this.objectConverterService).convertItemAndProductToComboItemVO(comboItemVO, item, product);
    verify(this.objectConverterService).convertProductToComboItemVO(comboItemVO, product);
    verify(this.
        productService)
        .getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false);
    verify(this.itemPriceService).getFinalPrice(price, DISCOUNT_PERCENTAGE);

    assertNotNull(comboItemVOList);
    assertEquals(1, comboItemVOList.size());
  }

  @Test
  public void getPromoBundlingByPristineIdOrItemSkuOrItemCode_successWithItemSkusAndHaveItemCode()
      throws Exception {
    activeComboRequestVO.setItemSku(ITEM_SKU);
    itemSkus.add(ITEM_SKU);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemSkus);

    List<Item> items = new ArrayList<>();
    this.item.getMasterDataItem().setGeneratedItemName(PRODUCT_NAME);
    this.item.setPristineDataItem(null);
    this.item.setItemCode(ITEM_CODE);
    items.add(item);
    this.item.setPrice(prices);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(items);

    itemCodes.add(ITEM_CODE);

    when(this.itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false))
        .thenReturn(this.item);
    when(this.promotionOutbound
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes)).thenReturn(activePromoBundlingResponseVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO))
        .thenReturn(comboVO);
    when(this.productService.getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false))
        .thenReturn(Arrays.asList(productAndItemsVO));
    when(this.objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
        .thenReturn(comboItemVOList);
    when(this.itemPriceService.getFinalPrice(price, DISCOUNT_PERCENTAGE)).thenReturn(85000.0);

    comboResponseVO = promoBundlingServiceImpl
        .getActiveCombos(STORE_ID, CHANNEL_WEB, CLIENT_ID,
            REQUEST_ID, USERNAME, activeComboRequestVO, PAGE, SIZE, SORT_BY, SORT_TYPE);

    verify(this.itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false);
    verify(this.promotionOutbound)
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO);
    verify(this.objectConverterService)
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());
    verify(this.objectConverterService).convertItemAndProductToComboItemVO(comboItemVO, item, product);
    verify(this.objectConverterService).convertProductToComboItemVO(comboItemVO, product);
    verify(this.
        productService)
        .getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false);
    verify(this.itemPriceService).getFinalPrice(price, DISCOUNT_PERCENTAGE);

    assertNotNull(comboItemVOList);
    assertEquals(1, comboItemVOList.size());
  }

  @Test
  public void getPromoBundlingByPristineIdOrItemSkuOrItemCode_throwExceptionWhenConvertComboVO()
      throws Exception {
    activeComboRequestVO.setItemSku(ITEM_SKU);
    itemSkus.add(ITEM_SKU);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemSkus);

    List<Item> items = new ArrayList<>();
    this.item.getMasterDataItem().setGeneratedItemName(PRODUCT_NAME);
    items.add(item);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(Arrays.asList(item));

    when(this.itemService.getItemCodesByPristineId(STORE_ID, PRISTINE_ID)).thenReturn(itemCodes);
    when(this.itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false))
        .thenReturn(this.item);
    when(this.promotionOutbound
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes)).thenReturn(activePromoBundlingResponseVO);
    when(objectConverterService
        .convertPromoBundlingDetailResponseToPromoBundlingDetailVo(promoBundlingDetailResponses))
        .thenReturn(promoBundlingDetailResponseVOs);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO))
        .thenReturn(comboVO);
    when(this.objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
        .thenThrow(new ApplicationRuntimeException());

    comboResponseVO = promoBundlingServiceImpl
        .getActiveCombos(STORE_ID, CHANNEL_WEB, CLIENT_ID,
            REQUEST_ID, USERNAME, activeComboRequestVO, PAGE, SIZE, SORT_BY, SORT_TYPE);

    verify(this.itemService).getItemCodesByPristineId(STORE_ID, PRISTINE_ID);
    verify(this.itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false);
    verify(this.promotionOutbound)
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO);
    verify(this.objectConverterService)
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());
  }

  @Test
  public void getPromoBundlingByPristineIdOrItemSkuOrItemCode_throwExceptionWhenConvertComboItem()
      throws Exception {
    activeComboRequestVO.setItemSku(ITEM_SKU);
    itemSkus.add(ITEM_SKU);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemSkus);

    List<Item> items = new ArrayList<>();
    this.item.getMasterDataItem().setGeneratedItemName(PRODUCT_NAME);
    items.add(item);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(Arrays.asList(item));

    when(this.itemService.getItemCodesByPristineId(STORE_ID, PRISTINE_ID)).thenReturn(itemCodes);
    when(this.itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false))
        .thenReturn(this.item);
    when(this.promotionOutbound
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes)).thenReturn(activePromoBundlingResponseVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO))
        .thenReturn(comboVO);
    when(this.productService.getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false))
        .thenReturn(Arrays.asList(productAndItemsVO));
    when(this.objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
        .thenReturn(comboItemVOList);
    when(this.itemPriceService.getFinalPrice(price, DISCOUNT_PERCENTAGE)).thenThrow(new ApplicationRuntimeException());

    comboResponseVO = promoBundlingServiceImpl
        .getActiveCombos(STORE_ID, CHANNEL_WEB, CLIENT_ID,
            REQUEST_ID, USERNAME, activeComboRequestVO, PAGE, SIZE, SORT_BY, SORT_TYPE);

    verify(this.itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false);
    verify(this.itemService).getItemCodesByPristineId(STORE_ID, PRISTINE_ID);
    verify(this.promotionOutbound)
        .getActiveCombosByItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            itemCodes);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO);
    verify(this.objectConverterService)
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());
    verify(this.objectConverterService).convertItemAndProductToComboItemVO(comboItemVO, item, product);
    verify(this.objectConverterService).convertProductToComboItemVO(comboItemVO, product);
    verify(this.
        productService)
        .getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false);
    verify(this.itemPriceService).getFinalPrice(price, DISCOUNT_PERCENTAGE);

    assertNotNull(comboItemVOList);
    assertEquals(1, comboItemVOList.size());
  }

  @Test
  public void getWholesaleByItemSku_success() throws Exception{

    WholesaleRuleVO wholesaleRuleVO = new WholesaleRuleVO();
    wholesaleRuleVO.setMinQuantity(2);
    wholesaleRuleVO.setMaxQuantity(4);
    wholesaleRuleVO.setDiscountPercentage(40);
    wholesaleRuleVO.setFinalPrice(30000.0);

    List<WholesaleRuleVO> wholesaleRuleVOList = new ArrayList<>();
    wholesaleRuleVOList.add(wholesaleRuleVO);

    WholesaleVO wholesaleVO = new WholesaleVO();
    wholesaleVO.setItemSku(ITEM_SKU);
    wholesaleVO.setItemCode(ITEM_CODE);
    wholesaleVO.setListPrice(50000.0);
    wholesaleVO.setWholesaleRules(wholesaleRuleVOList);

    PromoBundlingDetailResponseVO promoBundlingDetailResponseWholesaleVO = new PromoBundlingDetailResponseVO();
    promoBundlingDetailResponseWholesaleVO.setItemSku(ITEM_SKU);
    promoBundlingDetailResponseWholesaleVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    promoBundlingDetailResponseWholesaleVO.setPromoBundlingType(PROMO_BUNDLING_TYPE_WHOLESALE);
    promoBundlingDetailResponseWholesaleVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    promoBundlingDetailResponseWholesaleVO.setWholesaleRules(wholesaleRuleVOList);

    List<PromoBundlingDetailResponseVO> promoBundlingDetailResponsesWholesaleVO = new ArrayList<>();
    promoBundlingDetailResponsesWholesaleVO.add(promoBundlingDetailResponseWholesaleVO);

    WholesaleRule wholesaleRule = new WholesaleRule();
    wholesaleRule.setMinQuantity(2);
    wholesaleRule.setMaxQuantity(4);
    wholesaleRule.setDiscountPercentage(40);

    List<WholesaleRule> wholesaleRuleList = new ArrayList<>();
    wholesaleRuleList.add(wholesaleRule);

    PromoBundlingDetailResponse promoBundlingDetailResponseWholesale = new PromoBundlingDetailResponse();
    promoBundlingDetailResponseWholesale.setItemSku(ITEM_SKU);
    promoBundlingDetailResponseWholesale.setPromoBundlingId(PROMO_BUNDLING_ID);
    promoBundlingDetailResponseWholesale.setPromoBundlingType(PROMO_BUNDLING_TYPE_WHOLESALE);
    promoBundlingDetailResponseWholesale.setPromoBundlingName(PROMO_BUNDLING_NAME);
    promoBundlingDetailResponseWholesale.setWholesaleRules(wholesaleRuleList);

    List<PromoBundlingDetailResponse> promoBundlingDetailResponsesWholesale = new ArrayList<>();
    promoBundlingDetailResponsesWholesale.add(promoBundlingDetailResponseWholesale);

    PromoBundlingByItemSkuAndItemCodesResponseVO responseVO =
        new PromoBundlingByItemSkuAndItemCodesResponseVO();

    promoBundlingDetailResponseVO.setWholesaleRules(Arrays.asList(wholesaleRuleVO));

    responseVO.setPromoBundling(promoBundlingDetailResponseVO);
    responseVO.setTotalWholesaleRule(1);

    itemCodes.add(ITEM_CODE);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemCodes);

    Set<Price> prices = new HashSet<>();
    prices.add(price);
    item.setPrice(prices);
    when(this.itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null, false, false))
        .thenReturn(item);
    when(this.itemService.getItemCodesByPristine(STORE_ID, item.getPristineDataItem()))
        .thenReturn(itemCodes);
    when(this.promotionOutbound.getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.WHOLESALE.name(), ITEM_SKU, itemCodes, false)).thenReturn(responseVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseToWholesaleVO(responseVO, item)).thenReturn(wholesaleVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseToWholesaleRuleVO(responseVO.getPromoBundling()))
            .thenReturn(wholesaleRuleVOList);
    when(this.itemPriceService.getFinalPrice(price, wholesaleRuleVO.getDiscountPercentage())).thenReturn(30000.0);

    WholesaleVO result = this.promoBundlingServiceImpl.getWholesaleByItemSku(STORE_ID, CHANNEL_WEB,
        CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false);

    verify(this.itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null, false, false);
    verify(this.itemService).getItemCodesByPristine(STORE_ID, item.getPristineDataItem());
    verify(this.promotionOutbound).getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.WHOLESALE.name(), ITEM_SKU, itemCodes, false);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseToWholesaleVO(responseVO, item);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseToWholesaleRuleVO(responseVO.getPromoBundling());
    verify(this.itemPriceService).getFinalPrice(price, wholesaleRuleVO.getDiscountPercentage());

    assertNotNull(result);
    assertEquals(wholesaleVO, result);
  }

  @Test
  public void getWholesaleByItemSku_successWithIsPristineTrue() throws Exception{
    isPristine = true;

    WholesaleRuleVO wholesaleRuleVO = new WholesaleRuleVO();
    wholesaleRuleVO.setMinQuantity(2);
    wholesaleRuleVO.setMaxQuantity(4);
    wholesaleRuleVO.setDiscountPercentage(40);
    wholesaleRuleVO.setFinalPrice(30000.0);

    List<WholesaleRuleVO> wholesaleRuleVOList = new ArrayList<>();
    wholesaleRuleVOList.add(wholesaleRuleVO);

    WholesaleVO wholesaleVO = new WholesaleVO();
    wholesaleVO.setItemSku(ITEM_SKU);
    wholesaleVO.setItemCode(ITEM_CODE);
    wholesaleVO.setListPrice(50000.0);
    wholesaleVO.setWholesaleRules(wholesaleRuleVOList);

    PromoBundlingDetailResponseVO promoBundlingDetailResponseWholesaleVO = new PromoBundlingDetailResponseVO();
    promoBundlingDetailResponseWholesaleVO.setItemSku(ITEM_SKU);
    promoBundlingDetailResponseWholesaleVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    promoBundlingDetailResponseWholesaleVO.setPromoBundlingType(PROMO_BUNDLING_TYPE_WHOLESALE);
    promoBundlingDetailResponseWholesaleVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    promoBundlingDetailResponseWholesaleVO.setWholesaleRules(wholesaleRuleVOList);

    List<PromoBundlingDetailResponseVO> promoBundlingDetailResponsesWholesaleVO = new ArrayList<>();
    promoBundlingDetailResponsesWholesaleVO.add(promoBundlingDetailResponseWholesaleVO);

    WholesaleRule wholesaleRule = new WholesaleRule();
    wholesaleRule.setMinQuantity(2);
    wholesaleRule.setMaxQuantity(4);
    wholesaleRule.setDiscountPercentage(40);

    List<WholesaleRule> wholesaleRuleList = new ArrayList<>();
    wholesaleRuleList.add(wholesaleRule);

    PromoBundlingDetailResponse promoBundlingDetailResponseWholesale = new PromoBundlingDetailResponse();
    promoBundlingDetailResponseWholesale.setItemSku(ITEM_SKU);
    promoBundlingDetailResponseWholesale.setPromoBundlingId(PROMO_BUNDLING_ID);
    promoBundlingDetailResponseWholesale.setPromoBundlingType(PROMO_BUNDLING_TYPE_WHOLESALE);
    promoBundlingDetailResponseWholesale.setPromoBundlingName(PROMO_BUNDLING_NAME);
    promoBundlingDetailResponseWholesale.setWholesaleRules(wholesaleRuleList);

    List<PromoBundlingDetailResponse> promoBundlingDetailResponsesWholesale = new ArrayList<>();
    promoBundlingDetailResponsesWholesale.add(promoBundlingDetailResponseWholesale);

    PromoBundlingByItemSkuAndItemCodesResponseVO responseVO =
        new PromoBundlingByItemSkuAndItemCodesResponseVO();

    promoBundlingDetailResponseVO.setWholesaleRules(Arrays.asList(wholesaleRuleVO));

    responseVO.setPromoBundling(promoBundlingDetailResponseVO);
    responseVO.setTotalWholesaleRule(1);

    itemCodes.add(ITEM_CODE);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemCodes);

    Set<Price> prices = new HashSet<>();
    prices.add(price);
    item.setPrice(prices);
    when(this.itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null, false, false))
        .thenReturn(item);
    when(this.itemService.getItemCodesByPristine(STORE_ID, item.getPristineDataItem()))
        .thenReturn(itemCodes);
    when(this.promotionOutbound.getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.WHOLESALE.name(), ITEM_SKU, itemCodes, false)).thenReturn(responseVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseToWholesaleVO(responseVO, item)).thenReturn(wholesaleVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseToWholesaleRuleVO(responseVO.getPromoBundling()))
        .thenReturn(wholesaleRuleVOList);
    when(this.itemPriceService.getFinalPrice(price, wholesaleRuleVO.getDiscountPercentage())).thenReturn(30000.0);

    WholesaleVO result = this.promoBundlingServiceImpl.getWholesaleByItemSku(STORE_ID, CHANNEL_WEB,
        CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false);

    verify(this.itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null, false, false);
    verify(this.itemService).getItemCodesByPristine(STORE_ID, item.getPristineDataItem());
    verify(this.promotionOutbound).getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.WHOLESALE.name(), ITEM_SKU, itemCodes, false);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseToWholesaleVO(responseVO, item);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseToWholesaleRuleVO(responseVO.getPromoBundling());
    verify(this.itemPriceService).getFinalPrice(price, wholesaleRuleVO.getDiscountPercentage());

    assertNotNull(result);
    assertEquals(wholesaleVO, result);
  }

  @Test
  public void getWholesaleByItemSku_successWithIsPristineFalse() throws Exception {
    isPristine = false;

    WholesaleRuleVO wholesaleRuleVO = new WholesaleRuleVO();
    wholesaleRuleVO.setMinQuantity(2);
    wholesaleRuleVO.setMaxQuantity(4);
    wholesaleRuleVO.setDiscountPercentage(40);
    wholesaleRuleVO.setFinalPrice(30000.0);

    List<WholesaleRuleVO> wholesaleRuleVOList = new ArrayList<>();
    wholesaleRuleVOList.add(wholesaleRuleVO);

    WholesaleVO wholesaleVO = new WholesaleVO();
    wholesaleVO.setItemSku(ITEM_SKU);
    wholesaleVO.setItemCode(ITEM_CODE);
    wholesaleVO.setListPrice(50000.0);
    wholesaleVO.setWholesaleRules(wholesaleRuleVOList);

    PromoBundlingDetailResponseVO promoBundlingDetailResponseWholesaleVO = new PromoBundlingDetailResponseVO();
    promoBundlingDetailResponseWholesaleVO.setItemSku(ITEM_SKU);
    promoBundlingDetailResponseWholesaleVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    promoBundlingDetailResponseWholesaleVO.setPromoBundlingType(PROMO_BUNDLING_TYPE_WHOLESALE);
    promoBundlingDetailResponseWholesaleVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    promoBundlingDetailResponseWholesaleVO.setWholesaleRules(wholesaleRuleVOList);

    List<PromoBundlingDetailResponseVO> promoBundlingDetailResponsesWholesaleVO = new ArrayList<>();
    promoBundlingDetailResponsesWholesaleVO.add(promoBundlingDetailResponseWholesaleVO);

    WholesaleRule wholesaleRule = new WholesaleRule();
    wholesaleRule.setMinQuantity(2);
    wholesaleRule.setMaxQuantity(4);
    wholesaleRule.setDiscountPercentage(40);

    List<WholesaleRule> wholesaleRuleList = new ArrayList<>();
    wholesaleRuleList.add(wholesaleRule);

    PromoBundlingDetailResponse promoBundlingDetailResponseWholesale = new PromoBundlingDetailResponse();
    promoBundlingDetailResponseWholesale.setItemSku(ITEM_SKU);
    promoBundlingDetailResponseWholesale.setPromoBundlingId(PROMO_BUNDLING_ID);
    promoBundlingDetailResponseWholesale.setPromoBundlingType(PROMO_BUNDLING_TYPE_WHOLESALE);
    promoBundlingDetailResponseWholesale.setPromoBundlingName(PROMO_BUNDLING_NAME);
    promoBundlingDetailResponseWholesale.setWholesaleRules(wholesaleRuleList);

    List<PromoBundlingDetailResponse> promoBundlingDetailResponsesWholesale = new ArrayList<>();
    promoBundlingDetailResponsesWholesale.add(promoBundlingDetailResponseWholesale);

    PromoBundlingByItemSkuAndItemCodesResponseVO responseVO =
        new PromoBundlingByItemSkuAndItemCodesResponseVO();

    promoBundlingDetailResponseVO.setWholesaleRules(Arrays.asList(wholesaleRuleVO));

    responseVO.setPromoBundling(promoBundlingDetailResponseVO);
    responseVO.setTotalWholesaleRule(1);

    itemCodes.add(ITEM_CODE);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemCodes);

    Set<Price> prices = new HashSet<>();
    prices.add(price);
    item.setPrice(prices);
    when(this.itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null, false, false))
        .thenReturn(item);
    when(this.promotionOutbound
        .getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam, PromoBundlingType.WHOLESALE.name(), ITEM_SKU, itemCodes,
            false)).thenReturn(responseVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseToWholesaleVO(responseVO, item)).thenReturn(wholesaleVO);
    when(this.objectConverterService
        .convertPromoBundlingDetailResponseToWholesaleRuleVO(responseVO.getPromoBundling()))
        .thenReturn(wholesaleRuleVOList);
    when(this.itemPriceService.getFinalPrice(price, wholesaleRuleVO.getDiscountPercentage())).thenReturn(30000.0);

    WholesaleVO result = this.promoBundlingServiceImpl
        .getWholesaleByItemSku(STORE_ID, CHANNEL_WEB, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU,
            isPristine, false);

    verify(this.itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null, false, false);
    verify(this.promotionOutbound)
        .getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam, PromoBundlingType.WHOLESALE.name(), ITEM_SKU, itemCodes,
            false);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseToWholesaleVO(responseVO, item);
    verify(this.objectConverterService)
        .convertPromoBundlingDetailResponseToWholesaleRuleVO(responseVO.getPromoBundling());
    verify(this.itemPriceService).getFinalPrice(price, wholesaleRuleVO.getDiscountPercentage());

    assertNotNull(result);
    assertEquals(wholesaleVO, result);
  }

  @Test
  public void getComboDetailByItemSkuTest_Success() throws Exception{
    ComboItemVO comboItemVO = new ComboItemVO();
    comboItemVO.setProductName(PRODUCT_NAME);
    comboItemVO.setQuantity(1);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setProductCode(PRODUCT_CODE);
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setProductSku(PRODUCT_SKU);
    comboItemVO.setItemCode(ITEM_CODE);

    ComboDetailVo comboDetailVO = new ComboDetailVo();
    comboDetailVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    comboDetailVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    comboDetailVO.setPromoBundlingType("COMBO");
    comboDetailVO.setComboItems(Arrays.asList(comboItemVO));
    comboDetailVO.setTotal(1);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(Arrays.asList(item));
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    productAndItemsVOs.add(productAndItemsVO);

    itemSkus = new HashSet<>();
    itemSkus.add("item-sku");

    mandatoryRequestParam.setUsername(USERNAME);
    mandatoryRequestParam.setAuthenticator(USERNAME);
    when(itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false)).thenReturn(item);
    when(itemService.getItemCodesByPristine(STORE_ID, pristineDataItem)).thenReturn(itemCodes);
    when(promotionOutbound.getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.COMBO.name(), ITEM_SKU, itemCodes, false))
            .thenReturn(promoBundlingByItemSkuAndItemCodesResponseVO);
    when(objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
            .thenReturn(comboItemVOList);
    when(
        productService.getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false))
            .thenReturn(productAndItemsVOs);

    ComboDetailVo result = promoBundlingServiceImpl
        .getComboDetailByItemSku(STORE_ID, CHANNEL_WEB, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU,
            true, false);

    verify(objectConverterService)
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());
    verify(promotionOutbound).getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.COMBO.name(), ITEM_SKU, itemCodes, false);
    verify(itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false);
    verify(itemService).getItemCodesByPristine(STORE_ID, pristineDataItem);
    verify(productService).getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false);
    verify(objectConverterService).convertItemAndProductToComboItemVO(comboItemVO, item, product);
    verify(objectConverterService).convertProductToComboItemVO(comboItemVO, product);
    verify(itemPriceService).getFinalPrice(price,
        DISCOUNT_PERCENTAGE);

    assertNotNull(result);
    assertEquals(comboDetailVO, result);
  }

  @Test
  public void getComboDetailByItemSkuTest_SuccessWithIsPristineTrue() throws Exception{
    isPristine = true;

    ComboItemVO comboItemVO = new ComboItemVO();
    comboItemVO.setProductName(PRODUCT_NAME);
    comboItemVO.setQuantity(1);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setProductCode(PRODUCT_CODE);
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setProductSku(PRODUCT_SKU);
    comboItemVO.setItemCode(ITEM_CODE);

    ComboDetailVo comboDetailVO = new ComboDetailVo();
    comboDetailVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    comboDetailVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    comboDetailVO.setPromoBundlingType("COMBO");
    comboDetailVO.setComboItems(Arrays.asList(comboItemVO));
    comboDetailVO.setTotal(1);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(Arrays.asList(item));
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    productAndItemsVOs.add(productAndItemsVO);

    itemSkus = new HashSet<>();
    itemSkus.add("item-sku");

    mandatoryRequestParam.setUsername(USERNAME);
    mandatoryRequestParam.setAuthenticator(USERNAME);
    when(itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false)).thenReturn(item);
    when(itemService.getItemCodesByPristine(STORE_ID, pristineDataItem)).thenReturn(itemCodes);
    when(promotionOutbound.getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.COMBO.name(), ITEM_SKU, itemCodes, false))
        .thenReturn(promoBundlingByItemSkuAndItemCodesResponseVO);
    when(objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
        .thenReturn(comboItemVOList);
    when(
        productService.getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false))
        .thenReturn(productAndItemsVOs);

    ComboDetailVo result = promoBundlingServiceImpl.getComboDetailByItemSku(STORE_ID, CHANNEL_WEB, CLIENT_ID, REQUEST_ID,
        USERNAME, ITEM_SKU, true, false);

    verify(objectConverterService)
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());
    verify(promotionOutbound).getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.COMBO.name(), ITEM_SKU, itemCodes, false);
    verify(itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false);
    verify(itemService).getItemCodesByPristine(STORE_ID, pristineDataItem);
    verify(productService).getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false);
    verify(objectConverterService).convertItemAndProductToComboItemVO(comboItemVO, item, product);
    verify(objectConverterService).convertProductToComboItemVO(comboItemVO, product);
    verify(itemPriceService).getFinalPrice(price,
        DISCOUNT_PERCENTAGE);

    assertNotNull(result);
    assertEquals(comboDetailVO, result);
  }

  @Test
  public void getComboDetailByItemSkuTest_SuccessWithIsPristineFalse() throws Exception{
    isPristine = false;
    itemCodes.add(ITEM_CODE);

    ComboItemVO comboItemVO = new ComboItemVO();
    comboItemVO.setProductName(PRODUCT_NAME);
    comboItemVO.setQuantity(1);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setProductCode(PRODUCT_CODE);
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setProductSku(PRODUCT_SKU);
    comboItemVO.setItemCode(ITEM_CODE);

    ComboDetailVo comboDetailVO = new ComboDetailVo();
    comboDetailVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    comboDetailVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    comboDetailVO.setPromoBundlingType("COMBO");
    comboDetailVO.setComboItems(Arrays.asList(comboItemVO));
    comboDetailVO.setTotal(1);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(Arrays.asList(item));
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    productAndItemsVOs.add(productAndItemsVO);

    itemSkus = new HashSet<>();
    itemSkus.add("item-sku");

    mandatoryRequestParam.setUsername(USERNAME);
    mandatoryRequestParam.setAuthenticator(USERNAME);
    when(itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false)).thenReturn(item);
    when(promotionOutbound.getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.COMBO.name(), ITEM_SKU, itemCodes, false))
        .thenReturn(promoBundlingByItemSkuAndItemCodesResponseVO);
    when(objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
        .thenReturn(comboItemVOList);
    when(
        productService.getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false))
        .thenReturn(productAndItemsVOs);

    ComboDetailVo result = promoBundlingServiceImpl.getComboDetailByItemSku(STORE_ID, CHANNEL_WEB, CLIENT_ID, REQUEST_ID,
        USERNAME, ITEM_SKU, isPristine, false);

    verify(objectConverterService)
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());
    verify(promotionOutbound).getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.COMBO.name(), ITEM_SKU, itemCodes, false);
    verify(itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false);
    verify(productService).getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUEST_ID, USERNAME, itemSkus, true, false, false, false);
    verify(objectConverterService).convertItemAndProductToComboItemVO(comboItemVO, item, product);
    verify(objectConverterService).convertProductToComboItemVO(comboItemVO, product);
    verify(itemPriceService).getFinalPrice(price, DISCOUNT_PERCENTAGE);

    assertNotNull(result);
    assertEquals(comboDetailVO, result);
  }

  @Test
  public void getComboDetailByItemSkuTest_ReturnNull() throws Exception{

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(Arrays.asList(item));
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    productAndItemsVOs.add(productAndItemsVO);

    itemSkus = new HashSet<>();
    itemSkus.add("item-sku");

    mandatoryRequestParam.setUsername(USERNAME);
    mandatoryRequestParam.setAuthenticator(USERNAME);
    when(itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false)).thenReturn(item);
    when(itemService.getItemCodesByPristine(STORE_ID, pristineDataItem)).thenReturn(itemCodes);
    promoBundlingByItemSkuAndItemCodesResponseVO.getPromoBundling().setComboRules(new ArrayList<>());
    when(promotionOutbound.getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.COMBO.name(), ITEM_SKU, itemCodes, false))
        .thenReturn(promoBundlingByItemSkuAndItemCodesResponseVO);
    when(objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules()))
        .thenReturn(new ArrayList<ComboItemVO>());

    ComboDetailVo response = promoBundlingServiceImpl.getComboDetailByItemSku(STORE_ID, CHANNEL_WEB, CLIENT_ID, REQUEST_ID,
        USERNAME, ITEM_SKU, true, false);

    verify(promotionOutbound).getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam,
        PromoBundlingType.COMBO.name(), ITEM_SKU, itemCodes, false);
    verify(itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false, false, false, null, false, false);
    verify(itemService).getItemCodesByPristine(STORE_ID, pristineDataItem);

    assertEquals(response.getComboItems(), new ArrayList<>());
  }

  @Test
  public void getByPromoBundlingIdsTest() throws Exception {
    mandatoryRequestParam.setUsername(USERNAME);
    mandatoryRequestParam.setAuthenticator(USERNAME);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    Set<String> promoBundlingIds = new HashSet<>();
    promoBundlingIds.add(PromoBundlingType.COMBO.name());
    Mockito.when(
        promotionOutbound.getPromoBundlingDetailByPromoBundlingIds(mandatoryRequestParam, promoBundlingIds, itemSkus))
        .thenReturn(Arrays.asList(new PromoBundlingDetailResponseVO()));
    List<PromoBundlingDetailResponseVO> response =
        promoBundlingServiceImpl.getByPromoBundlingIds(mandatoryRequestParam, promoBundlingIds, itemSkus);
    verify(promotionOutbound)
        .getPromoBundlingDetailByPromoBundlingIds(mandatoryRequestParam, promoBundlingIds, itemSkus);
    assertEquals(1, response.size());
  }

  @Test
  public void getByPromoBundlingIdsWithEmptyPromoBundlingListTest() throws Exception {
    mandatoryRequestParam.setUsername(USERNAME);
    mandatoryRequestParam.setAuthenticator(USERNAME);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<PromoBundlingDetailResponseVO> response =
        promoBundlingServiceImpl.getByPromoBundlingIds(mandatoryRequestParam, null, itemSkus);
    assertEquals(0, response.size());
  }

  @Test
  public void getWholesaleRulesTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, null);

    when(productPricingOutbound.findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCode(
        Mockito.any(MandatoryRequestParam.class), Mockito.eq(PromoBundlingType.WHOLESALE.name()),
        Mockito.anySet())).thenReturn(getPromoBundlingSkuDetailResponses());
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.MINIMUM_PRICE, "10.0", SystemParameterNames.MINIMUM_PRICE));
    when(itemPriceService.getFinalPriceWithMinimumPriceParameter(Mockito.any(Price.class), Mockito.eq(10.0),
        Mockito.eq(10.0))).thenReturn(10.0);

    Map<String, List<WholesaleRuleVO>>
        wholesaleRuleVoMap = promoBundlingServiceImpl.getWholesaleRules(mandatoryRequestParam, getItemPickupPoints());

    verify(productPricingOutbound).findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCode(
        Mockito.any(MandatoryRequestParam.class), Mockito.eq(PromoBundlingType.WHOLESALE.name()),
        Mockito.anySet());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPriceService).getFinalPriceWithMinimumPriceParameter(Mockito.any(Price.class), Mockito.eq(10.0),
        Mockito.eq(10.0));

    Assertions.assertEquals(1,
        wholesaleRuleVoMap.get(CommonUtil.generatePickupPointKey(ITEM_SKU, PICKUP_POINT_CODE)).size());
    Assertions.assertEquals(0,
        wholesaleRuleVoMap.get(CommonUtil.generatePickupPointKey(ITEM_SKU2, PICKUP_POINT_CODE_2)).size());
    Assertions.assertEquals(0,
        wholesaleRuleVoMap.get(CommonUtil.generatePickupPointKey(ITEM_SKU_3, PICKUP_POINT_CODE_3)).size());
  }

  @Test
  public void getWholesaleRulesNoWSItemTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, null);

    Map<String, List<WholesaleRuleVO>>
        wholesaleRuleVoMap = promoBundlingServiceImpl.getWholesaleRules(mandatoryRequestParam, new ArrayList<>());

    Assertions.assertEquals(0, wholesaleRuleVoMap.size());
  }

  private List<ItemPickupPoint> getItemPickupPoints() {
    Set<String> activePromoBundlingSet1 = ImmutableSet.of(Constants.WHOLESALE);
    Set<String> activePromoBundlingSet2 = ImmutableSet.of(Constants.WHOLESALE_PRICE);
    Set<String> activePromoBundlingSet3 = new HashSet<>();
    Set<String> activePromoBundlingSet4 = ImmutableSet.of(Constants.COMBO);

    ItemPickupPoint itemPickupPoint1 = ItemPickupPoint.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
        .activePromoBundlings(activePromoBundlingSet1).price(new HashSet<>()).build();
    ItemPickupPoint itemPickupPoint2 = ItemPickupPoint.builder().itemSku(ITEM_SKU2).pickupPointCode(PICKUP_POINT_CODE_2)
        .activePromoBundlings(activePromoBundlingSet2).build();
    ItemPickupPoint itemPickupPoint3 =
        ItemPickupPoint.builder().itemSku(ITEM_SKU_3).pickupPointCode(PICKUP_POINT_CODE_3)
            .activePromoBundlings(activePromoBundlingSet3).build();
    ItemPickupPoint itemPickupPoint4 =
        ItemPickupPoint.builder().itemSku(ITEM_SKU_3).pickupPointCode(PICKUP_POINT_CODE_3)
            .activePromoBundlings(activePromoBundlingSet4).build();

    return Arrays.asList(itemPickupPoint1, itemPickupPoint2, itemPickupPoint3, itemPickupPoint4);
  }

  private List<PromoBundlingSkuDetailResponse> getPromoBundlingSkuDetailResponses() {
    com.gdn.partners.product.pricing.web.model.promo.bundling.response.WholesaleRule wholesaleRule =
        new com.gdn.partners.product.pricing.web.model.promo.bundling.response.WholesaleRule(10, 10, 10.0, 0L);
    PromoBundlingSkuDetailResponse promoBundlingSkuDetailResponse = new PromoBundlingSkuDetailResponse();
    promoBundlingSkuDetailResponse.setItemSku(ITEM_SKU);
    promoBundlingSkuDetailResponse.setPickupPointCode(PICKUP_POINT_CODE);
    promoBundlingSkuDetailResponse.setWholesaleRules(Arrays.asList(wholesaleRule));
    return Arrays.asList(promoBundlingSkuDetailResponse);
  }
}
