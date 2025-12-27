package com.gdn.partners.pbp.converter;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.entity.eventstore.ProductItemEventStore;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemViewConfig;
import com.gdn.x.product.domain.event.model.MasterDataItem;
import com.gdn.x.product.domain.event.model.Price;
import com.gdn.x.product.enums.ChannelName;

public class ProductItemEventStoreModelConverterBeanTest {

  @InjectMocks
  private ProductItemEventStoreModelConverterBean productItemEventStoreModelConverterBean;

  private static final boolean SYNCHRONIZED = false;
  private static final boolean MARK_FOR_DELETE = false;
  private static final boolean ARCHIVED = false;
  private static final boolean O2O_CHANNEL_ACTIVE = false;
  private static final boolean BUYABLE = false;
  private static final boolean DISCOVERABLE = false;
  private static final double LIST_PRICE = 1000;
  private static final double OFFER_PRICE = 1000;
  private static final String EVENT_NAME = "eventName";
  private static final String GENERATED_ITEM_NAME = "generatedItemName";
  private static final String SKU_CODE = "skuCode";

  private Date curDate;
  private MasterDataItem masterDataItem;
  private Price price;
  private Set<Price> priceSet;
  private ItemViewConfig itemViewConfig;
  private Set<ItemViewConfig> itemViewConfigSet;

  private ItemChange itemChange;
  private ProductItemEventStore productItemEventStore;

  @BeforeEach
  public void _setup() {
    MockitoAnnotations.initMocks(this);

    curDate = new Date();

    masterDataItem = new MasterDataItem();
    masterDataItem.setGeneratedItemName(GENERATED_ITEM_NAME);
    masterDataItem.setSkuCode(SKU_CODE);

    price = new Price();
    price.setChannel(ChannelName.DEFAULT.name());
    price.setListPrice(LIST_PRICE);
    price.setOfferPrice(OFFER_PRICE);
    priceSet = new HashSet<>();
    priceSet.add(price);

    itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(ChannelName.DEFAULT.name());
    itemViewConfig.setBuyable(BUYABLE);
    itemViewConfig.setDiscoverable(DISCOVERABLE);
    itemViewConfigSet = new HashSet<>();
    itemViewConfigSet.add(itemViewConfig);

    itemChange = new ItemChange();
    itemChange.setMasterDataItem(masterDataItem);
    itemChange.setPrice(priceSet);
    itemChange.setItemViewConfigs(itemViewConfigSet);

    productItemEventStore = new ProductItemEventStore();
    productItemEventStore.setEventTimestamp(curDate);
    productItemEventStore.setSynchronized(SYNCHRONIZED);
    productItemEventStore.setMarkForDelete(MARK_FOR_DELETE);
    productItemEventStore.setArchived(ARCHIVED);
    productItemEventStore.setO2oChannelActive(O2O_CHANNEL_ACTIVE);
    productItemEventStore.setListPrice(LIST_PRICE);
    productItemEventStore.setOfferPrice(OFFER_PRICE);
    productItemEventStore.setBuyable(BUYABLE);
    productItemEventStore.setDiscoverable(DISCOVERABLE);
  }

  @AfterEach
  public void _teardown() {}

  @Test
  public void testConvertToProductItemEventStore() {
    ProductItemEventStore result = productItemEventStoreModelConverterBean
        .convertToProductItemEventStore(itemChange, EVENT_NAME);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(GENERATED_ITEM_NAME, result.getItemName());
    Assertions.assertEquals(SKU_CODE, result.getSkuCode());
    Assertions.assertEquals(Double.valueOf(LIST_PRICE), result.getListPrice());
    Assertions.assertEquals(Double.valueOf(OFFER_PRICE), result.getOfferPrice());
    Assertions.assertEquals(BUYABLE, result.isBuyable());
    Assertions.assertEquals(DISCOVERABLE, result.isDiscoverable());
  }

  @Test
  public void testConvertToProductItemEventStore2_MasterDataItem_null() {
    itemChange.setMasterDataItem(null);

    ProductItemEventStore result = productItemEventStoreModelConverterBean
        .convertToProductItemEventStore(itemChange, EVENT_NAME);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(null, result.getItemName());
    Assertions.assertEquals(null, result.getSkuCode());
  }

  @Test
  public void testConvertToProductItemEventStore2_priceSet_empty() {
    priceSet.clear();

    ProductItemEventStore result = productItemEventStoreModelConverterBean
        .convertToProductItemEventStore(itemChange, EVENT_NAME);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(null, result.getListPrice());
    Assertions.assertEquals(null, result.getOfferPrice());
  }

  @Test
  public void testConvertToProductItemEventStore2_itemViewConfigSet_empty() {
    itemViewConfigSet.clear();

    ProductItemEventStore result = productItemEventStoreModelConverterBean
        .convertToProductItemEventStore(itemChange, EVENT_NAME);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(null, result.isBuyable());
    Assertions.assertEquals(null, result.isDiscoverable());
  }

  @Test
  public void testConvertToItemChange() {
    ItemChange result =
        productItemEventStoreModelConverterBean.convertToItemChange(productItemEventStore);
    Assertions.assertNotNull(result);
  }

}
