package com.gdn.x.mta.distributiontask.service.impl.util;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;

import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorTaskInformationDTO;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class VendorUtilsTest {
  @InjectMocks
  private VendorUtils instance;

  private List<Object[]> createVendorInformationTaskNative() {
    List<Object[]> results = new ArrayList<>();
    Object[] result = new Object[6];
    result[0] = "id";
    result[1] = "vendorCode";
    result[2] = "name";
    result[3] = BigDecimal.valueOf(1L);
    result[4] = BigDecimal.valueOf(2L);
    result[5] = 10;
    results.add(result);
    return results;
  }

  private List<Object[]> createVendorCapacityNative() {
    Object[] result = new Object[6];
    result[0] = "id";
    result[1] = "vendorCode";
    result[2] = "name";
    result[3] = new Date();
    result[4] = new Date();
    result[5] = 10;
    Object[] result2 = new Object[6];
    result2[0] = "id";
    result2[1] = "vendorCode";
    result2[2] = "name";
    result2[3] = null;
    result2[4] = null;
    result2[5] = 10;
    Object[] result3 = new Object[6];
    result3[0] = "id";
    result3[1] = "vendorCode";
    result3[2] = "name";
    result3[3] = new Date();
    result3[4] = null;
    result3[5] = 10;
    List<Object[]> results = new ArrayList<>(Arrays.asList(result, result2, result3));
    return results;
  }

  @Test
   void convertToVendorCapacityDTOTest() {
    for (Object[] result : createVendorCapacityNative()) {
      VendorCapacityDTO resultAfter = instance.convertToVendorCapacityDTO(result);
      Assertions.assertNotNull(resultAfter);
    }
  }
  
  @Test
   void convertToVendorInformationDTOTest() {
    for (Object[] result : createVendorInformationTaskNative()) {
      VendorTaskInformationDTO resultAfter = instance.convertToVendorInformationDTO(result);
      Assertions.assertNotNull(resultAfter);
    }
  }
}
