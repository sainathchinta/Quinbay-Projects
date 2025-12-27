package com.gdn.mta.product.converter.offlineitem;

import static org.hamcrest.MatcherAssert.assertThat;

import java.util.ArrayList;
import java.util.List;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;

public class OfflineItemRequestConverterBeanTest {

  private static final String ITEM_SKU = "itemSku";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String FILE_NAME = "file-name";
  private static final double LIST_PRICE = 200000D;
  private static final double OFFER_PRICE = 100000D;
  private static final int STOCK = 1;

  private OfflineItemRequestConverterBean converter = new OfflineItemRequestConverterBean();
  private List<DeleteOfflineItemRequest> deleteOfflineItemRequests;
  private DeleteOfflineItemRequest deleteOfflineItemRequest;
  private UpsertOfflineItemRequest upsertOfflineItemRequest;
  private List<UpsertOfflineItemRequest> upsertOfflineItemRequests;
  private UpsertOfflineItem upsertOfflineItem;
  private List<UpsertOfflineItem> upsertOfflineItems;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    this.deleteOfflineItemRequests = new ArrayList<>();
    this.deleteOfflineItemRequest = new DeleteOfflineItemRequest();
    this.deleteOfflineItemRequest.setItemSku(ITEM_SKU);
    this.deleteOfflineItemRequest.setMerchantSku(MERCHANT_SKU);
    this.deleteOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    this.deleteOfflineItemRequests.add(this.deleteOfflineItemRequest);

    this.upsertOfflineItemRequest = new com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest();
    this.upsertOfflineItemRequest.setItemSku(ITEM_SKU);
    this.upsertOfflineItemRequest.setListPrice(LIST_PRICE);
    this.upsertOfflineItemRequest.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItemRequest.setStock(STOCK);
    this.upsertOfflineItemRequest.setFileName(FILE_NAME);

    this.upsertOfflineItemRequests = new ArrayList<>();
    this.upsertOfflineItemRequests.add(this.upsertOfflineItemRequest);

    this.upsertOfflineItem = new UpsertOfflineItem();
    this.upsertOfflineItem.setItemSku(ITEM_SKU);
    this.upsertOfflineItem.setListPrice(LIST_PRICE);
    this.upsertOfflineItem.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItem.setFileName(FILE_NAME);
    this.upsertOfflineItem.setStock(STOCK);
    this.upsertOfflineItem.setCncActive(true);

    this.upsertOfflineItems = new ArrayList<>();
    this.upsertOfflineItems.add(this.upsertOfflineItem);
  }

  @Test
  public void testConvertToDeleteOfflineItem() {
    List<DeleteOfflineItem> result =
        this.converter.convertToDeleteOfflineItem(this.deleteOfflineItemRequests);
    Assertions.assertNotNull(result);
    assertThat(result.size(), Matchers.equalTo(1));
    Assertions.assertEquals(ITEM_SKU, result.get(0).getItemSku());
    Assertions.assertEquals(MERCHANT_SKU, result.get(0).getMerchantSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, result.get(0).getPickupPointCode());
  }

  @Test
  public void convertToUpsertOfflineItemsTest() {
    List<UpsertOfflineItem> result = this.converter
        .convertToUpsertOfflineItems(this.upsertOfflineItemRequests);
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertEquals(result, this.upsertOfflineItems);
  }
}
