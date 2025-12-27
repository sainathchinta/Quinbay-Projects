package com.gdn.mta.product.converter.offlineitem;

import com.gdn.partners.pbp.dto.offlineitem.OfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemResponseDetailResponse;
import com.gdn.partners.pbp.model.offlineitem.OfflineItemDetail;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class OfflineItemResponseConverterBeanTest {

  private final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";
  private final String PICKUP_POINT_NAME = "PICKUP_POINT_NAME";
  private final String ITEM_SKU = "ITEM_SKU";
  private final String MERCHANT_SKU = "MERCHANT_SKU";
  private final Integer STOCK = 11;
  private final Double PRICE = 111.111;

  private List<OfflineItemDetail> offlineItemDetails;
  private OfflineItemResponseConverterBean converter;
  private OfflineItemDetail offlineItemDetail;
  private List<OfflineItemResponseDetail> offlineItemResponseDetails;
  private OfflineItemResponseDetail offlineItemResponseDetail;

  @Test
  public void convertOfflineItemDetailResponsesTest() {
    this.offlineItemDetails = Arrays.asList(offlineItemDetail);

    List<OfflineItemDetailResponse> responses =
        this.converter.convertOfflineItemDetailResponses(offlineItemDetails);
    OfflineItemDetailResponse response = responses.get(0);

    Assertions.assertEquals(PICKUP_POINT_CODE, response.getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getPickupPointName());
    Assertions.assertEquals(true, response.isActive());
    Assertions.assertEquals(STOCK, response.getStock());
    Assertions.assertEquals(PRICE, response.getPrice());
  }

  @Test
  public void convertOfflineItemDetailResponsesTestWithEmptyListRequest() {
    this.offlineItemDetails = new ArrayList<>();
    List<OfflineItemDetailResponse> responses =
        this.converter.convertOfflineItemDetailResponses(offlineItemDetails);
    Assertions.assertEquals(responses.size(), 0);
  }

  @Test
  public void convertOfflineItemResponseDetailResponsesTest() {
    List<OfflineItemResponseDetailResponse> responses =
        this.converter.convertOfflineItemResponseDetailResponses(this.offlineItemResponseDetails);
    OfflineItemResponseDetailResponse response = responses.get(0);

    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(MERCHANT_SKU, response.getMerchantSku());
  }

  @Test
  public void convertOfflineItemResponseDetailResponsesWithEmptyListRequest() {
    List<OfflineItemResponseDetailResponse> responses =
        this.converter.convertOfflineItemResponseDetailResponses(Collections.emptyList());
    Assertions.assertEquals(0, responses.size());
  }

  @BeforeEach
  public void setUp() {
    this.converter = new OfflineItemResponseConverterBean();

    this.offlineItemDetail = new OfflineItemDetail();
    this.offlineItemDetail.setPickupPointCode(PICKUP_POINT_CODE);
    this.offlineItemDetail.setPickupPointName(PICKUP_POINT_NAME);
    this.offlineItemDetail.setActive(true);
    this.offlineItemDetail.setStock(STOCK);
    this.offlineItemDetail.setPrice(PRICE);

    this.offlineItemResponseDetail = new OfflineItemResponseDetail();
    this.offlineItemResponseDetail.setItemSku(ITEM_SKU);
    this.offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU);

    this.offlineItemResponseDetails = Arrays.asList(this.offlineItemResponseDetail);
  }
}
