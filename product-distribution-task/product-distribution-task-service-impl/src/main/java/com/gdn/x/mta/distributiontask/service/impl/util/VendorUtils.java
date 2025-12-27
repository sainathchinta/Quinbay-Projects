package com.gdn.x.mta.distributiontask.service.impl.util;

import java.math.BigDecimal;
import java.util.Date;

import org.springframework.stereotype.Component;

import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorTaskInformationDTO;

@Component
public class VendorUtils {
  public VendorCapacityDTO convertToVendorCapacityDTO(Object[] nativeVendorCapacityDTO) {
    Date startHoliday = null;
    Date endHoliday = null;
    if (nativeVendorCapacityDTO[3] != null && nativeVendorCapacityDTO[4] != null) {
      startHoliday = (Date) nativeVendorCapacityDTO[3];
      endHoliday = (Date) nativeVendorCapacityDTO[4];
    }
    return new VendorCapacityDTO((String) nativeVendorCapacityDTO[0],
        (String) nativeVendorCapacityDTO[1], (String) nativeVendorCapacityDTO[2], startHoliday,
        endHoliday, (Integer) nativeVendorCapacityDTO[5]);
  }

  public VendorTaskInformationDTO convertToVendorInformationDTO(
      Object[] nativeVendorTaskInformationDTO) {
    BigDecimal assignedCount = (BigDecimal)nativeVendorTaskInformationDTO[3];
    BigDecimal qcCount = (BigDecimal)nativeVendorTaskInformationDTO[4];
    return new VendorTaskInformationDTO((String) nativeVendorTaskInformationDTO[0],
        (String) nativeVendorTaskInformationDTO[1], (String) nativeVendorTaskInformationDTO[2],
        assignedCount.intValue(), qcCount.intValue(),
        (Integer) nativeVendorTaskInformationDTO[5]);
  }
}
