package com.gdn.mta.product.converter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import com.gda.mta.product.dto.ProductLevel3SummaryDetailsRequest;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.ProductLevel3SummaryMinifiedRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilterDetails;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemSearchRequest;

public class ProductLevel3RequestConverterBeanTest {

  private static final String GDN_ITEM_SKU = "TOS-16000-78568-00001";

  private ProductLevel3RequestConverterBean converter = new ProductLevel3RequestConverterBean();

  @Test
  public void testConvertToProductLevel3SummaryFilter() {
    ProductLevel3SummaryRequest summaryRequest = new ProductLevel3SummaryRequest();
    summaryRequest.setGdnSku(GDN_ITEM_SKU);
    summaryRequest.setPickupPointCodes(Collections.singletonList(GDN_ITEM_SKU));
    summaryRequest.setPromoTypes(Collections.singletonList(GDN_ITEM_SKU));
    ProductLevel3SummaryFilter filter = converter
      .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(summaryRequest);

    Assertions.assertEquals(GDN_ITEM_SKU, filter.getGdnSku());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(filter.getItemSkus()));
    Assertions.assertTrue(filter.getItemSkus().contains(GDN_ITEM_SKU));
    Assertions.assertTrue(filter.getPromoTypes().contains(GDN_ITEM_SKU));
    Assertions.assertTrue(filter.getPickupPointCodes().contains(GDN_ITEM_SKU));
  }

  @Test
  public void testConvertToProductLevel3SummaryFilter_withItemSKUs() {
    ProductLevel3SummaryRequest summaryRequest = new ProductLevel3SummaryRequest();
    summaryRequest.setIgnoreArchive(true);
    summaryRequest.setItemSkus(Arrays.asList(GDN_ITEM_SKU));
    ProductLevel3SummaryFilter filter = converter
      .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(summaryRequest);

    Assertions.assertTrue(CollectionUtils.isNotEmpty(filter.getItemSkus()));
    Assertions.assertTrue(filter.getItemSkus().contains(GDN_ITEM_SKU));
    Assertions.assertNull(filter.getArchived());
  }

  @Test
  public void testConvertToProductLevel3SummaryFilter_withItemSKUsAndIndividual() {
    ProductLevel3SummaryRequest summaryRequest = new ProductLevel3SummaryRequest();
    summaryRequest.setGdnSku("gdnSKU");
    summaryRequest.setItemSkus(new ArrayList<>(Arrays.asList(GDN_ITEM_SKU)));
    ProductLevel3SummaryFilter filter = converter
      .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(summaryRequest);

    Assertions.assertEquals("gdnSKU", filter.getGdnSku());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(filter.getItemSkus()));
    Assertions.assertTrue(filter.getItemSkus().contains(GDN_ITEM_SKU));
    Assertions.assertTrue(filter.getItemSkus().contains("gdnSKU"));
  }

  @Test
  public void testConvertToProductLevel3SummaryFilter_NullSource() {
    converter.convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(null);
  }

  @Test
  public void testConvertToProductLevel3SummaryMinifiedFilter() {
    converter
        .convertProductLevel3SummaryMinifiedRequestToProductLevel3SummaryFilter(new ProductLevel3SummaryMinifiedRequest());
  }

  @Test
  public void testConvertToProductLevel3SummaryMinifiedFilter_NullSource() {
    converter.convertProductLevel3SummaryMinifiedRequestToProductLevel3SummaryFilter(null);
  }

  @Test
  public void testConvertProductLevel3ItemSearchRequestToProductLevel3ItemSearch_sourceNull() {
    converter.convertProductLevel3ItemSearchRequestToProductLevel3ItemSearch(null);
  }

  @Test
  public void testConvertProductLevel3ItemSearchRequestToProductLevel3ItemSearch_sourceNotNull() {
    converter.convertProductLevel3ItemSearchRequestToProductLevel3ItemSearch(
        new ProductLevel3ItemSearchRequest());
  }

  @Test
  public void convertProductLevel3SummaryRequestToProductLevel3SummaryDetailsFilterTest() {
    ProductLevel3SummaryDetailsRequest summaryRequest = new ProductLevel3SummaryDetailsRequest();
    summaryRequest.setBusinessPartnerCode("BP-Code");
    summaryRequest.setProductSku("PRO-12002");
    summaryRequest.setItemSku("PRO-12002-00001");
    ProductLevel3SummaryFilterDetails filter =
        converter.convertProductLevel3SummaryRequestToProductLevel3SummaryDetailsFilter(summaryRequest);
    Assertions.assertEquals("BP-Code", filter.getBusinessPartnerCode());
    Assertions.assertEquals("PRO-12002", filter.getProductSku());
    Assertions.assertEquals("PRO-12002-00001", filter.getItemSku());
    Assertions.assertTrue(StringUtils.isNotEmpty(filter.getBusinessPartnerCode()));
  }
}
