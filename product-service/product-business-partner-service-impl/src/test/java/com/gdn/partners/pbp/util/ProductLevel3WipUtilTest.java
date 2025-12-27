package com.gdn.partners.pbp.util;

import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.gdn.mta.product.entity.StateCountDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.entity.productlevel3.CountProductLevel3Wip;

public class ProductLevel3WipUtilTest {
  
  @InjectMocks
  private ProductLevel3WipUtil instance;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }
  
  private List<Object[]> generateCountProductLevel3WipRaws() throws Exception {
    List<Object[]> countProductLevel3WipRaws = new ArrayList<Object[]>();
    countProductLevel3WipRaws.add(new Object[] {true,"FAILED", 1L});
    countProductLevel3WipRaws.add(new Object[] {false,"NEED_CORRECTION", 1L});
    countProductLevel3WipRaws.add(new Object[] {false,"", 1L});
    return countProductLevel3WipRaws;
  }

  private List<Object[]> generateCountProductLevel3() throws Exception {
    List<Object[]> countProductLevel3WipRaws = new ArrayList<Object[]>();
    countProductLevel3WipRaws.add(new Object[] {"NEED_CORRECTION", 1L});
    countProductLevel3WipRaws.add(new Object[] {"IN_PROGRESS", 1L});
    return countProductLevel3WipRaws;
  }

  @AfterEach
  public void tearDown() throws Exception {}

  @Test
  public void testGenerateCountProductLevel3WipWithState() throws Exception {
    CountProductLevel3Wip response = instance.generateCountProductLevel3WipWithState(generateCountProductLevel3WipRaws());
    assertSame(3L, response.getTotalItems().longValue());
  }

  @Test
  public void testGenerateCountProductLevel3ByState() throws Exception {
    ProductLevel3CountResponse response = instance.generateCountProductLevel3ByState(generateCountProductLevel3());
    assertSame(2L, response.getTotalItems().longValue());
  }

  @Test
  public void generateProductLevel3CountByStateForInProgressTest(){
    List<StateCountDTO> stateCountDTOList = Collections.singletonList(
      StateCountDTO.builder().state(Constants.IN_PROGRESS_STATE).count(300L).build());
    ProductLevel3CountResponse productLevel3CountResponse =
      instance.generateProductLevel3CountByState(stateCountDTOList);
    Assertions.assertEquals(300L, productLevel3CountResponse.getTotalItems().longValue());
  }

  @Test
  public void generateProductLevel3CountByStateForNeedCorrectionTest(){
    List<StateCountDTO> stateCountDTOList = Collections.singletonList(
      StateCountDTO.builder().state(Constants.NEED_CORRECTION).count(300L).build());
    ProductLevel3CountResponse productLevel3CountResponse =
      instance.generateProductLevel3CountByState(stateCountDTOList);
    Assertions.assertEquals(300L, productLevel3CountResponse.getTotalItems().longValue());
  }

  @Test
  public void generateProductLevel3CountByStateForRandomTypeTest(){
    List<StateCountDTO> stateCountDTOList = Collections.singletonList(
      StateCountDTO.builder().state(Constants.AUTO_NEED_REVISION).count(300L).build());
    ProductLevel3CountResponse productLevel3CountResponse =
      instance.generateProductLevel3CountByState(stateCountDTOList);
    Assertions.assertEquals(300L, productLevel3CountResponse.getTotalItems().longValue());
  }
}
