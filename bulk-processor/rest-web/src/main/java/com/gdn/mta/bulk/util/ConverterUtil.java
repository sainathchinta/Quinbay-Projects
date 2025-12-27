package com.gdn.mta.bulk.util;

import com.gdn.mta.bulk.service.util.BeanUtils;
import com.gdn.mta.bulk.dto.BulkAddCampaignProductDTO;
import com.gdn.mta.bulk.dto.BulkProcessAddCampaignProductRequest;

import java.util.Calendar;
import java.util.Date;

public class ConverterUtil {

  private ConverterUtil() {
  }

  /**
   * convert BulkProcessAddCampaignProductRequest to BulkAddCampaignProductDTO
   *
   * @param bulkProcessAddCampaignProductRequest
   * @return BulkAddCampaignProductDTO
   */
  public static BulkAddCampaignProductDTO convertToBulkAddCampaignProductDTO(
      BulkProcessAddCampaignProductRequest bulkProcessAddCampaignProductRequest) {
    BulkAddCampaignProductDTO bulkAddCampaignProductDTO = new BulkAddCampaignProductDTO();
    BeanUtils.copyProperties(bulkProcessAddCampaignProductRequest, bulkAddCampaignProductDTO);
    return bulkAddCampaignProductDTO;
  }

  public static Date getEarlierDateBySeconds(int seconds) {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.SECOND, -seconds);
    return calendar.getTime();
  }
}
