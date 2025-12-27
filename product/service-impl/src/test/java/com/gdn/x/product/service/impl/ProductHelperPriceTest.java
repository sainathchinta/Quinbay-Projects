package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.outbound.api.feign.PCBFeign;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.util.FormulaUtil;

;


public class ProductHelperPriceTest {

  private static final String STORE_ID = "store-id";

  private static final String PRODUCT_SKU = "product-sku";

  private static final double DISCOUNT_PRICE1 = 1000;

  private static final double DISCOUNT_PRICE2 = 2000;

  private static final String CHANNEL_WEB = ChannelName.DESKTOP_WEB.toString();

  private static final double OFFER_PRICE = 5000;

  private static final String CHANNEL_DEFAULT = ChannelName.DEFAULT.toString();

  private static final String CHANNEL_MOBILE = ChannelName.MOBILE_WEB.toString();

  private static final long AN_HOUR_IN_MILLISECOND = 3600000;

  private static final String ITEM_CODE = "item-code";

  private static final String MERCHANT_SKU = "merchantSku";

  private static final int SIZE_OF_PRODUCT_ATTRIBUTES = 0;

  private static final String MERCHANT_CODE = "BLI-01006";

  @InjectMocks
  private ProductHelperServiceImpl productHelperServiceImpl;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private CachedService cachedService;

  @Mock
  private ProductService productService;

  @Mock
  private ChannelService channelService;

  @Mock
  private FormulaUtil formulaUtil;

  private HashSet<Price> pricesWithDefaultChannel;

  private DiscountPrice priceADiscountPrice1;

  private DiscountPrice priceADiscountPrice2;

  private Price priceWithChannelWeb;

  private Date startDateTrue;

  private Date endDateTrue;

  private Date currDate;

  private Date endDateFalse;

  private Date startDateFalse;

  private Price priceWithChannelDefault;

  private Item item;

  private ArrayList<DiscountPrice> discountPriceList;

  private Set<ItemViewConfig> itemViewConfigs;

  private Item itemRequestVOWithDefaultChannel;

  private Item itemRequestVOWithoutDefaultChannel;

  private Set<Price> pricesWithoutDefaultChannel;

  private ArrayList<Item> itemRequestVOListWithoutDefaultChannel;

  private ArrayList<Item> itemRequestVOListWithDefaultChannel;

  private List<Item> listOfItems;

  @Test
  public void containDefaultChannelPriceForListOfItemRequestVOTestFalse() {
    boolean result =
        this.productHelperServiceImpl
            .containDefaultChannelPriceForListOfItemRequestVO(this.itemRequestVOListWithoutDefaultChannel);
    Assertions.assertEquals(result, (false));
    verify(this.channelService).getDefaultChannel();
  }

  @Test
  public void containDefaultChannelPriceForListOfItemRequestVOTestTrue() {
    boolean result =
        this.productHelperServiceImpl
            .containDefaultChannelPriceForListOfItemRequestVO(this.itemRequestVOListWithDefaultChannel);
    Assertions.assertEquals(result, (true));
    verify(this.channelService).getDefaultChannel();
  }

  @Test
  public void containDefaultChannelPriceTestFalse() {
    boolean result =
        this.productHelperServiceImpl
            .containDefaultChannelPrice(this.itemRequestVOWithoutDefaultChannel);
    Assertions.assertEquals(result, (false));
    verify(this.channelService).getDefaultChannel();
  }

  @Test
  public void containDefaultChannelPriceTestTrue() {
    boolean result =
        this.productHelperServiceImpl
            .containDefaultChannelPrice(this.itemRequestVOWithDefaultChannel);
    Assertions.assertEquals(result, (true));
    verify(this.channelService).getDefaultChannel();
  }

  @Test
  public void getRelevantItemPriceTest() throws Exception {
    double result =
        this.productHelperServiceImpl
            .getRelevantItemPrice(this.pricesWithDefaultChannel, ProductHelperPriceTest.CHANNEL_WEB)
            .getListOfDiscountPrices().get(0).getDiscountPrice();

    Assertions.assertEquals(result, (this.priceADiscountPrice1.getDiscountPrice()));
  }

  @Test
  public void getRelevantItemPriceTestWithChannelFoundAndIsNotOnSaleDateAnymore() throws Exception {
    this.discountPriceList = new ArrayList<DiscountPrice>();
    this.discountPriceList.add(this.priceADiscountPrice2);
    this.priceWithChannelWeb.setListOfDiscountPrices(this.discountPriceList);

    double result =
        this.productHelperServiceImpl.getRelevantItemPrice(this.pricesWithDefaultChannel,
            ProductHelperPriceTest.CHANNEL_WEB).getOfferPrice();

    Assertions.assertEquals(result, (this.priceWithChannelWeb.getOfferPrice()));
  }

  @Test
  public void getRelevantItemPriceTestWithChannelFoundAndIsNotYetOnSaleDate() throws Exception {
    this.priceADiscountPrice2.setStartDateTime(this.startDateFalse);
    this.discountPriceList = new ArrayList<DiscountPrice>();
    this.discountPriceList.add(this.priceADiscountPrice2);

    this.priceWithChannelWeb.setListOfDiscountPrices(this.discountPriceList);

    double result =
        this.productHelperServiceImpl.getRelevantItemPrice(this.pricesWithDefaultChannel,
            ProductHelperPriceTest.CHANNEL_WEB).getOfferPrice();

    Assertions.assertEquals(result, (this.priceWithChannelWeb.getOfferPrice()));
  }

  @Test
  public void getRelevantItemPriceTestWithChannelNotFoundAndHasDefaultPriceAndHasNoSalePrice()
      throws Exception {
    this.priceWithChannelDefault.setListOfDiscountPrices(null);

    double result =
        this.productHelperServiceImpl.getRelevantItemPrice(this.pricesWithDefaultChannel,
            ProductHelperPriceTest.CHANNEL_MOBILE).getOfferPrice();

    Assertions.assertEquals(result, (this.priceWithChannelDefault.getOfferPrice()));
    verify(this.channelService).getDefaultChannel();
  }

  @Test
  public void getRelevantItemPriceTestWithChannelNotFoundAndHasDefaultPriceOnSalePrice()
      throws Exception {
    this.priceADiscountPrice2.setStartDateTime(this.startDateTrue);
    this.priceADiscountPrice2.setEndDateTime(this.endDateTrue);
    this.productHelperServiceImpl.getRelevantItemPrice(this.pricesWithDefaultChannel,
        ProductHelperPriceTest.CHANNEL_MOBILE);
    verify(this.channelService).getDefaultChannel();
  }

  @Test
  public void getRelevantItemPriceTestWithChannelNotFoundAndHasNoDefaultPrice() throws Exception {
    this.pricesWithDefaultChannel.remove(this.priceWithChannelDefault);

    try {
      this.productHelperServiceImpl.getRelevantItemPrice(this.pricesWithDefaultChannel,
          ProductHelperPriceTest.CHANNEL_MOBILE);
    } catch (Exception e) {
      assertNotNull(e.getMessage());
      verify(this.channelService).getDefaultChannel();
    }
  }

  @Test
  public void getRelevantItemPriceTestWithNullChannel() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.getRelevantItemPrice(this.pricesWithDefaultChannel, null));
  }

  @Test
  public void setItemDetailTest() {
    when(
        this.formulaUtil.appendWithSerial(ProductHelperPriceTest.PRODUCT_SKU,
            ProductHelperPriceTest.SIZE_OF_PRODUCT_ATTRIBUTES + 1, 5)).thenReturn(
        ProductHelperPriceTest.ITEM_CODE);
    this.productHelperServiceImpl.setItemDetail(ProductHelperPriceTest.STORE_ID,
        ProductHelperPriceTest.PRODUCT_SKU, ProductHelperPriceTest.MERCHANT_CODE,
        ProductHelperPriceTest.SIZE_OF_PRODUCT_ATTRIBUTES,
        this.itemRequestVOWithDefaultChannel);
    verify(this.formulaUtil).appendWithSerial(ProductHelperPriceTest.PRODUCT_SKU,
        ProductHelperPriceTest.SIZE_OF_PRODUCT_ATTRIBUTES + 1, 5);
  }

  @Test
  public void setItemDetailTestWithNullItemRequestVO() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.setItemDetail(ProductHelperPriceTest.STORE_ID,
        ProductHelperPriceTest.PRODUCT_SKU, ProductHelperPriceTest.MERCHANT_CODE,
        ProductHelperPriceTest.SIZE_OF_PRODUCT_ATTRIBUTES, null));
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    when(this.channelService.getDefaultChannel()).thenReturn(ChannelName.DEFAULT.toString());
    this.currDate = new Date();
    this.startDateTrue =
        new Date(this.currDate.getTime() - ProductHelperPriceTest.AN_HOUR_IN_MILLISECOND);
    this.endDateTrue =
        new Date(this.currDate.getTime() + ProductHelperPriceTest.AN_HOUR_IN_MILLISECOND);

    this.priceADiscountPrice1 = new DiscountPrice();
    this.priceADiscountPrice1.setStartDateTime(this.startDateTrue);
    this.priceADiscountPrice1.setEndDateTime(this.endDateTrue);
    this.priceADiscountPrice1.setDiscountPrice(ProductHelperPriceTest.DISCOUNT_PRICE1);

    this.startDateFalse =
        new Date(this.currDate.getTime() + ProductHelperPriceTest.AN_HOUR_IN_MILLISECOND);

    this.endDateFalse =
        new Date(this.currDate.getTime() - ProductHelperPriceTest.AN_HOUR_IN_MILLISECOND);

    this.priceADiscountPrice2 = new DiscountPrice();
    this.priceADiscountPrice2.setStartDateTime(this.startDateTrue);
    this.priceADiscountPrice2.setEndDateTime(this.endDateFalse);
    this.priceADiscountPrice2.setDiscountPrice(ProductHelperPriceTest.DISCOUNT_PRICE2);

    this.discountPriceList = new ArrayList<DiscountPrice>();
    this.discountPriceList.add(this.priceADiscountPrice1);
    this.discountPriceList.add(this.priceADiscountPrice2);

    this.priceWithChannelWeb = new Price();
    this.priceWithChannelWeb.setChannel(ProductHelperPriceTest.CHANNEL_WEB);
    this.priceWithChannelWeb.setOfferPrice(ProductHelperPriceTest.OFFER_PRICE);
    this.priceWithChannelWeb.setListOfDiscountPrices(this.discountPriceList);

    this.priceWithChannelDefault = new Price();
    this.priceWithChannelDefault.setChannel(ProductHelperPriceTest.CHANNEL_DEFAULT);
    this.priceWithChannelDefault.setOfferPrice(ProductHelperPriceTest.OFFER_PRICE);
    this.priceWithChannelDefault.setListOfDiscountPrices(this.discountPriceList);

    this.pricesWithDefaultChannel = new HashSet<Price>();
    this.pricesWithDefaultChannel.add(this.priceWithChannelDefault);
    this.pricesWithDefaultChannel.add(this.priceWithChannelWeb);

    this.pricesWithoutDefaultChannel = new HashSet<Price>();
    this.pricesWithoutDefaultChannel.add(this.priceWithChannelWeb);

    this.itemRequestVOWithDefaultChannel = new Item();
    this.itemRequestVOWithDefaultChannel.setPrice(this.pricesWithDefaultChannel);
    this.itemRequestVOWithDefaultChannel.setItemCode(ProductHelperPriceTest.ITEM_CODE);
    this.itemRequestVOWithDefaultChannel.setMerchantSku(ProductHelperPriceTest.MERCHANT_SKU);
    this.itemRequestVOWithDefaultChannel.setItemViewConfigs(this.itemViewConfigs);

    this.itemRequestVOWithoutDefaultChannel = new Item();
    this.itemRequestVOWithoutDefaultChannel.setPrice(this.pricesWithoutDefaultChannel);

    this.itemRequestVOListWithDefaultChannel = new ArrayList<Item>();
    this.itemRequestVOListWithDefaultChannel.add(this.itemRequestVOWithDefaultChannel);

    this.itemRequestVOListWithoutDefaultChannel = new ArrayList<Item>();
    this.itemRequestVOListWithoutDefaultChannel.add(this.itemRequestVOWithoutDefaultChannel);

    this.item = new Item();
    this.listOfItems = new ArrayList<Item>();
    this.listOfItems.add(this.item);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.cachedService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.pcbFeign);
    verifyNoMoreInteractions(this.channelService);
    verifyNoMoreInteractions(this.formulaUtil);
  }

}
