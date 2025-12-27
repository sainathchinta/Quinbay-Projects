package com.gdn.x.product.outbound.helper;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.PageImpl;

import com.gdn.x.product.model.solr.ProductSolr;

public class ResponseHelperTest {

  @Test
  public void validateSolrResponse() {
    boolean response = ResponseHelper.validateSolrResponse(new PageImpl<>(new ArrayList<>()));
    Assertions.assertFalse(response);
  }

  @Test
  public void validateSolrResponseNullPage() {
    boolean response = ResponseHelper.validateSolrResponse(null);
    Assertions.assertFalse(response);
  }

  @Test
  public void validateSolrResponseSuccesstest() {
    boolean response = ResponseHelper.validateSolrResponse(new PageImpl<>(Arrays.asList(new ProductSolr())));
    Assertions.assertTrue(response);
  }
}