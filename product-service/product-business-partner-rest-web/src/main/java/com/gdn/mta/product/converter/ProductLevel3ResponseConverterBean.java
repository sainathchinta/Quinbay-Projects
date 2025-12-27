package com.gdn.mta.product.converter;

import java.util.ArrayList;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gda.mta.product.dto.ProductLevel3ImageResponse;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryMinifiedResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetails;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemSearchResponse;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;

@Component
public class ProductLevel3ResponseConverterBean implements ProductLevel3ResponseConverter {

  private static final String DOT_SEPARATOR = ".";

  private static final String SKU_SEPARATOR = "-";

  @Autowired
  private ApplicationProperties properties;

  @Override
  public ProductLevel3SummaryResponse convertProductLevel3SummaryToProductLevel3SummaryResponse(
      ProductLevel3Summary source) {
    ProductLevel3SummaryResponse response = new ProductLevel3SummaryResponse();
    BeanUtils.copyProperties(source, response, new String[] {"prices", "viewConfigs", "images", "promoTypes"});
    response.setPrices(new ArrayList<ProductLevel3PriceResponse>());
    response.setViewConfigs(new ArrayList<ProductLevel3ViewConfigResponse>());
    response.setImages(new ArrayList<ProductLevel3ImageResponse>());
    response.setPromoTypes(source.getPromoTypes());
    for (ProductLevel3Price productLevel3Price : source.getPrices()) {
      ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
      BeanUtils.copyProperties(productLevel3Price, productLevel3PriceResponse);
      response.getPrices().add(productLevel3PriceResponse);
    }

    for (ProductLevel3ViewConfig productLevel3ViewConfig : source.getViewConfigs()) {
      ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
          new ProductLevel3ViewConfigResponse();
      BeanUtils.copyProperties(productLevel3ViewConfig, productLevel3ViewConfigResponse);
      response.getViewConfigs().add(productLevel3ViewConfigResponse);
    }

    for (ProductLevel3Image productLevel3Image : source.getImages()) {
      ProductLevel3ImageResponse productLevel3ImageResponse = new ProductLevel3ImageResponse();
      BeanUtils.copyProperties(productLevel3Image, productLevel3ImageResponse);
      response.getImages().add(productLevel3ImageResponse);
    }
    response.setProductDetailPageLink(toProductDetailPage(response.getItemSku()));
    response.setEnableEdit(!source.isForceReview());
    response.setArchived(source.getIsArchived());
    response.setPreOrder(source.isPreOrder());
    return response;
  }

  private String toProductDetailPage(String itemSKU) {
    return String.join("-", properties.getProductDetailPageUrlPrefix(), toProductDetailsSKU(itemSKU));
  }

  private String toProductDetailsSKU(String itemSKU) {
    return Optional.ofNullable(itemSKU)
      .map(sku -> sku.substring(0, itemSKU.lastIndexOf("-")))
      .map(sku -> sku.replace(SKU_SEPARATOR, DOT_SEPARATOR))
      .map(sku -> String.join(DOT_SEPARATOR, sku, "html"))
      .orElse(StringUtils.EMPTY);
  }

  @Override
  public ProductLevel3SummaryMinifiedResponse convertProductLevel3SummaryToProductLevel3SummaryMinifiedResponse(
      ProductLevel3SummaryMinified source) {
    ProductLevel3SummaryMinifiedResponse response = new ProductLevel3SummaryMinifiedResponse();
    BeanUtils.copyProperties(source, response, new String[] {"prices", "viewConfigs", "images"});
    response.setImages(new ArrayList<ProductLevel3ImageResponse>());
    for (ProductLevel3Image image : source.getImages()) {
      ProductLevel3ImageResponse imageTarget = new ProductLevel3ImageResponse();
      BeanUtils.copyProperties(image, imageTarget);
      response.getImages().add(imageTarget);
    }
    response.setPrices(new ArrayList<ProductLevel3PriceResponse>());
    for (ProductLevel3Price price : source.getPrices()) {
      ProductLevel3PriceResponse priceTarget = new ProductLevel3PriceResponse();
      BeanUtils.copyProperties(price, priceTarget);
      response.getPrices().add(priceTarget);
    }
    response.setViewConfigs(new ArrayList<ProductLevel3ViewConfigResponse>());
    for (ProductLevel3ViewConfig viewConfig : source.getViewConfigs()) {
      ProductLevel3ViewConfigResponse viewConfigTarget = new ProductLevel3ViewConfigResponse();
      BeanUtils.copyProperties(viewConfig, viewConfigTarget);
      response.getViewConfigs().add(viewConfigTarget);
    }
    return response;
  }

  @Override
  public ProductLevel3ItemSearchResponse convertProductLevel3ItemToProductLevel3ItemSearchResponse(
      ProductLevel3Item source) {
    ProductLevel3ItemSearchResponse target = new ProductLevel3ItemSearchResponse();
    BeanUtils.copyProperties(source, target);
    return target;
  }

  @Override
  public ProductLevel3SummaryDetailsResponse convertProductLevel3SummaryDetailsToProductLevel3SummaryDetailsResponse(
      ProductLevel3SummaryDetails productLevel3SummaryDetails) {
    ProductLevel3SummaryDetailsResponse productLevel3SummaryDetailsResponse = new ProductLevel3SummaryDetailsResponse();
    BeanUtils.copyProperties(productLevel3SummaryDetails, productLevel3SummaryDetailsResponse);
    return productLevel3SummaryDetailsResponse;
  }
}
