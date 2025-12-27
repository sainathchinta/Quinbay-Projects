package com.gdn.partners.pbp.service.productlevel3;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.partners.pbp.outbound.sellerLogistics.SellerLogisticsOutbound;
import com.gdn.seller.logistics.web.model.request.LogisticProductSkuLevel;
import com.gdn.seller.logistics.web.model.request.SaveSkuLogisticProductRequest;
import com.gdn.seller.logistics.web.model.response.GetSkuLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.SaveSkuLogisticProductResponse;

public class ProductLevel3LogisticsServiceBeanTest {

  private static final String ITEM_SKU = "itemSku";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String MERCHANT_DELIVERY_TYPE = "merchantDeliveryType";

  @InjectMocks
  private ProductLevel3LogisticsServiceBean productLevel3LogisticsServiceBean;

  @Mock
  private SellerLogisticsOutbound sellerLogisticsOutbound;

  @Mock
  private ProductLevel3Converter productLevel3Converter;

  private GetSkuLogisticProductResponse getSkuLogisticProductResponse;
  private List<GetSkuLogisticProductResponse> getSkuLogisticProductResponseList;
  private List<String> itemSkus;
  private List<ProductLevel3Logistics> logistics;
  private SaveSkuLogisticProductRequest saveSkuLogisticProductRequest;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    itemSkus = new ArrayList<>();
    itemSkus.add(ITEM_SKU);
    logistics = new ArrayList<>();
    logistics.add(new ProductLevel3Logistics());
    saveSkuLogisticProductRequest = SaveSkuLogisticProductRequest.builder()
        .itemSkus(Arrays.asList(ITEM_SKU)).merchantCode(MERCHANT_CODE)
        .logisticProductCodes(Arrays.asList(new LogisticProductSkuLevel())).build();
    getSkuLogisticProductResponseList = new ArrayList<>();
    getSkuLogisticProductResponse = new GetSkuLogisticProductResponse();
    getSkuLogisticProductResponseList.add(getSkuLogisticProductResponse);
    when(this.sellerLogisticsOutbound.getSkuLogistics(ITEM_SKU, MERCHANT_CODE,
        MERCHANT_DELIVERY_TYPE)).thenReturn(getSkuLogisticProductResponseList);
    when(this.productLevel3Converter
        .convertLogisticDetailsToItemLevel3Logistics(getSkuLogisticProductResponseList))
            .thenReturn(new ArrayList<>());
    when(this.sellerLogisticsOutbound.saveSkuLogistics(saveSkuLogisticProductRequest, true))
        .thenReturn(new SaveSkuLogisticProductResponse());
  }

  @Test
  public void findLogisticsByItemSku() throws Exception {
    productLevel3LogisticsServiceBean.findLogisticsByItemSku(ITEM_SKU, MERCHANT_CODE,
        MERCHANT_DELIVERY_TYPE);
    verify(sellerLogisticsOutbound).getSkuLogistics(ITEM_SKU, MERCHANT_CODE,
        MERCHANT_DELIVERY_TYPE);
    verify(productLevel3Converter)
        .convertLogisticDetailsToItemLevel3Logistics(getSkuLogisticProductResponseList);
  }

  @Test
  public void findLogisticsByItemSku_emptyList() throws Exception {
    when(this.sellerLogisticsOutbound.getSkuLogistics(ITEM_SKU, MERCHANT_CODE,
        MERCHANT_DELIVERY_TYPE)).thenReturn(null);
    productLevel3LogisticsServiceBean.findLogisticsByItemSku(ITEM_SKU, MERCHANT_CODE,
        MERCHANT_DELIVERY_TYPE);
    verify(sellerLogisticsOutbound).getSkuLogistics(ITEM_SKU, MERCHANT_CODE,
        MERCHANT_DELIVERY_TYPE);
  }

  @Test
  public void findLogisticsByItemSku_exception() throws Exception {
    when(this.sellerLogisticsOutbound.getSkuLogistics(ITEM_SKU, MERCHANT_CODE,
        MERCHANT_DELIVERY_TYPE)).thenThrow(new Exception());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productLevel3LogisticsServiceBean.findLogisticsByItemSku(ITEM_SKU, MERCHANT_CODE,
            MERCHANT_DELIVERY_TYPE);
      });
    } finally {
      verify(sellerLogisticsOutbound).getSkuLogistics(ITEM_SKU, MERCHANT_CODE,
          MERCHANT_DELIVERY_TYPE);
    }
  }

  @Test
  public void saveLogisticsByItemSku() throws Exception {
    itemSkus.add(ITEM_SKU);
    productLevel3LogisticsServiceBean.saveLogisticsByItemSku(itemSkus, MERCHANT_CODE, logistics,
        true);
    verify(sellerLogisticsOutbound).saveSkuLogistics(saveSkuLogisticProductRequest, true);
  }

  @Test
  public void saveLogisticsByItemSku_emptyLogistics() throws Exception {
    productLevel3LogisticsServiceBean.saveLogisticsByItemSku(itemSkus, MERCHANT_CODE,
        new ArrayList<>(), true);
  }

  @Test
  public void saveLogisticsByItemSku_Exception() throws Exception {
    when(this.sellerLogisticsOutbound.saveSkuLogistics(saveSkuLogisticProductRequest, true))
        .thenThrow(new Exception());
    productLevel3LogisticsServiceBean.saveLogisticsByItemSku(itemSkus, MERCHANT_CODE, logistics,
        true);
    verify(sellerLogisticsOutbound).saveSkuLogistics(saveSkuLogisticProductRequest, true);
  }

  @Test
  public void saveLogisticsByEmptyItemSkuList() throws Exception {
    productLevel3LogisticsServiceBean.saveLogisticsByItemSku(new ArrayList<>(), MERCHANT_CODE,
      logistics, true);
    verify(sellerLogisticsOutbound, times(0)).saveSkuLogistics(saveSkuLogisticProductRequest, true);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    verifyNoMoreInteractions(this.sellerLogisticsOutbound, this.productLevel3Converter);
  }
}
