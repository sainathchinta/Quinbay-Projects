package com.gdn.mta.product.converter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gda.mta.product.dto.ProductLevel3SummaryDetailsResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetails;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetailsImage;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;


public class ProductLevel3ResponseConverterBeanTest {

  private static final String ITEM_SKU = "TOQ-15632-00234-00001";
  private static final String PROMO_BUNDLING = "PROMO_BUNDLING";

  @InjectMocks
  private ProductLevel3ResponseConverterBean converter;

  @Mock
  private ApplicationProperties properties;

  private ProductLevel3Summary productLevel3Summary;

  private ProductLevel3SummaryMinified productLevel3SummaryMinified;

  private ProductLevel3Item productLevel3Item;

  private ProductLevel3SummaryDetails productLevel3SummaryDetails;



  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    productLevel3Summary = new ProductLevel3Summary();
    productLevel3Summary.setItemSku(ITEM_SKU);
    productLevel3Summary.setProductScore(90.0);
    productLevel3SummaryMinified = new ProductLevel3SummaryMinified();
    List<ProductLevel3Price> prices = new ArrayList<>();
    ProductLevel3Price price = new ProductLevel3Price();
    prices.add(price);
    productLevel3Summary.setPrices(prices);
    productLevel3SummaryMinified.setPrices(prices);

    List<ProductLevel3ViewConfig> viewConfigs = new ArrayList<>();
    ProductLevel3ViewConfig viewConfig = new ProductLevel3ViewConfig();
    viewConfigs.add(viewConfig);
    productLevel3Summary.setViewConfigs(viewConfigs);
    productLevel3SummaryMinified.setViewConfigs(viewConfigs);

    List<ProductLevel3Image> images = new ArrayList<>();
    ProductLevel3Image image = new ProductLevel3Image();
    images.add(image);
    productLevel3Summary.setImages(images);
    productLevel3Summary.setPriceEditDisabled(true);
    productLevel3SummaryMinified.setImages(images);

    this.productLevel3Item = new ProductLevel3Item();

    Mockito.when(properties.getProductDetailPageUrlPrefix()).thenReturn("www.blibli.com/product-detail");

    List<ProductLevel3SummaryDetailsImage> itemImages = new ArrayList<>();
    ProductLevel3SummaryDetailsImage itemImage = new ProductLevel3SummaryDetailsImage();
    itemImages.add(itemImage);

    productLevel3SummaryDetails = new ProductLevel3SummaryDetails();
    productLevel3SummaryDetails.setItemSku(ITEM_SKU);
    productLevel3SummaryDetails.setProductScore(90.0);
    productLevel3SummaryDetails.setPrices(prices);
    productLevel3SummaryDetails.setViewConfigs(viewConfigs);
    productLevel3SummaryDetails.setImages(itemImages);
    productLevel3SummaryDetails.setPriceEditDisabled(true);
  }

  @Test
  public void convertProductLevel3SummaryToProductLevel3SummaryResponse_Test() {
    productLevel3Summary.setIsArchived(true);
    productLevel3Summary.setPreOrder(true);
    ProductLevel3SummaryResponse response =
        converter.convertProductLevel3SummaryToProductLevel3SummaryResponse(productLevel3Summary);

    Assertions.assertEquals("www.blibli.com/product-detail-TOQ.15632.00234.html", response.getProductDetailPageLink());
    Assertions.assertTrue(response.isPriceEditDisabled());
    Assertions.assertEquals(90.0, response.getProductScore(), 0);
    Assertions.assertTrue(response.getArchived());
    Assertions.assertTrue(response.isPreOrder());
  }

  @Test
  public void convertProductLevel3SummaryToProductLevel3SummaryResponsePromoTypesTest() {
    productLevel3Summary.setPromoTypes(Arrays.asList(PROMO_BUNDLING));
    productLevel3Summary.setForceReview(true);
    ProductLevel3SummaryResponse response = converter
        .convertProductLevel3SummaryToProductLevel3SummaryResponse(productLevel3Summary);

    Assertions.assertEquals("www.blibli.com/product-detail-TOQ.15632.00234.html", response.getProductDetailPageLink());
    Assertions.assertEquals(1, response.getPromoTypes().size());
    Assertions.assertEquals(PROMO_BUNDLING, response.getPromoTypes().get(0));
    Assertions.assertFalse(response.isEnableEdit());
  }

  @Test
  public void convertProductLevel3SummaryToProductLevel3SummaryResponse_whenItemSkuIsNull() {
    productLevel3Summary.setItemSku(null);
    ProductLevel3SummaryResponse response = converter
      .convertProductLevel3SummaryToProductLevel3SummaryResponse(productLevel3Summary);

    Assertions.assertEquals("www.blibli.com/product-detail-", response.getProductDetailPageLink());
  }

  @Test
  public void convertProductLevel3SummaryToProductLevel3SummaryMinifiedResponse_Test() {
    converter
        .convertProductLevel3SummaryToProductLevel3SummaryMinifiedResponse(productLevel3SummaryMinified);
  }

  @Test
  public void convertProductLevel3ItemToProductLevel3ItemSearchResponse_Test() {
    this.converter
        .convertProductLevel3ItemToProductLevel3ItemSearchResponse(this.productLevel3Item);
  }

  @Test
  public void convertProductLevel3SummaryDetailsToProductLevel3SummaryDetailsResponseTest() {
    ProductLevel3SummaryDetailsResponse response = converter
        .convertProductLevel3SummaryDetailsToProductLevel3SummaryDetailsResponse(productLevel3SummaryDetails);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(90.0, response.getProductScore(), 0.0);
    Assertions.assertTrue(response.isPriceEditDisabled());
  }
}
