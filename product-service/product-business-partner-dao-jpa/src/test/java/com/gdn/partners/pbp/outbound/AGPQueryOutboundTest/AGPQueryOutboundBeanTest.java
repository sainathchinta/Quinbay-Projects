package com.gdn.partners.pbp.outbound.AGPQueryOutboundTest;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.AgpResponseVo;
import com.gdn.partners.pbp.outbound.AGPOutboundBean.AGPQueryOutboundBean;
import com.gdn.partners.pbp.outbound.AGPQuery.AGPQueryFeign;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

class AGPQueryOutboundBeanTest {

  @Mock
  private AGPQueryFeign agpQueryFeign;

  @InjectMocks
  private AGPQueryOutboundBean agpQueryOutboundBean;

  private ObjectMapper objectMapper;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.initMocks(this);
    objectMapper = new ObjectMapper();
  }

  @Test
  void testFindNumberOfOrderByProductSkuList_NonEmptyList() throws Exception {
    Set<String> productSkuList = new HashSet<>(Arrays.asList("SKU1", "SKU2"));
    List<String> itemStatus = Arrays.asList("X", "DF");
    AgpResponseVo mockResponse = new AgpResponseVo();
    AgpResponseVo.Aggregations aggregations = new AgpResponseVo.Aggregations();
    AgpResponseVo.SkuTerms skuTerms = new AgpResponseVo.SkuTerms();
    skuTerms.setBuckets(
        Arrays.asList(new AgpResponseVo.Bucket("SKU1", 5), new AgpResponseVo.Bucket("SKU2", 3)));
    aggregations.setSkuTerms(skuTerms);
    mockResponse.setAggregations(aggregations);
    Map<String, Object> expectedObjectMap = new HashMap<>();
    expectedObjectMap.put("mockKey", "mockValue");
    when(agpQueryFeign.findNumberOfOrderByProductSkuList(Mockito.any())).thenReturn(mockResponse);
    Map<String, Boolean> result =
        agpQueryOutboundBean.findNumberOfOrderByProductSkuList(productSkuList);
    assertEquals(2, result.size());
    assertTrue(result.get("SKU1"));
    assertTrue(result.get("SKU2"));
  }

  @Test
  void testFindNumberOfOrderByProductSkuList_EmptyList() {
    Set<String> productSkuList = new HashSet<>();
    Map<String, Boolean> result =
        agpQueryOutboundBean.findNumberOfOrderByProductSkuList(productSkuList);
    assertTrue(result.isEmpty());
  }

  @Test
  void testFindNumberOfOrderByProductSkuList_Exception() throws Exception {
    Set<String> productSkuList = new HashSet<>(Collections.singletonList("SKU1"));
    when(agpQueryFeign.findNumberOfOrderByProductSkuList(anyMap())).thenThrow(
        new RuntimeException("Mock Exception"));
    Map<String, Boolean> result =
        agpQueryOutboundBean.findNumberOfOrderByProductSkuList(productSkuList);
    assertTrue(result.isEmpty());
    verify(agpQueryFeign, times(1)).findNumberOfOrderByProductSkuList(anyMap());
  }
}
