package com.gdn.partners.pbp.util;

import java.util.List;

import com.gdn.mta.product.entity.StateCountDTO;
import org.springframework.stereotype.Component;

import com.gdn.mta.product.commons.constant.ProductLevel3SummaryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.entity.productlevel3.CountProductLevel3Wip;

@Component
public class ProductLevel3WipUtil {


  public CountProductLevel3Wip generateCountProductLevel3WipWithState(
      List<Object[]> countProductLevel3WipRaws) throws Exception {
    CountProductLevel3Wip countProductLevel3Wip = new CountProductLevel3Wip();
    Long totalItems = 0L;
    countProductLevel3Wip.getTotalItemsByCriterias().put(ProductLevel3WipSummaryCriteria.FAILED,
        0L);
    countProductLevel3Wip.getTotalItemsByCriterias()
        .put(ProductLevel3WipSummaryCriteria.IN_PROGRESS, 0L);
    countProductLevel3Wip.getTotalItemsByCriterias()
        .put(ProductLevel3WipSummaryCriteria.NEED_CORRECTION, 0L);
    for (Object[] countProductLevel3WipRaw : countProductLevel3WipRaws) {
      boolean active = Boolean.parseBoolean(String.valueOf(countProductLevel3WipRaw[0]));
      String state = String.valueOf(countProductLevel3WipRaw[1]);
      long totalItemsByCriteria = Long.parseLong(String.valueOf(countProductLevel3WipRaw[2]));
      if (active) {
        countProductLevel3Wip.getTotalItemsByCriterias().put(ProductLevel3WipSummaryCriteria.FAILED,
            totalItemsByCriteria);
      } else if (ProductLevel3WipSummaryCriteria.NEED_CORRECTION.name().equals(state)) {
        countProductLevel3Wip.getTotalItemsByCriterias()
            .put(ProductLevel3WipSummaryCriteria.NEED_CORRECTION, totalItemsByCriteria);
      } else {
        countProductLevel3Wip.getTotalItemsByCriterias()
            .put(ProductLevel3WipSummaryCriteria.IN_PROGRESS, 
                countProductLevel3Wip.getTotalItemsByCriterias().get(ProductLevel3WipSummaryCriteria.IN_PROGRESS) + totalItemsByCriteria);
      }
      totalItems += totalItemsByCriteria;
    }
    countProductLevel3Wip.setTotalItems(totalItems);
    return countProductLevel3Wip;
  }

  public ProductLevel3CountResponse generateCountProductLevel3ByState(List<Object[]> productLevel3CountResponses) {
    ProductLevel3CountResponse productLevel3CountResponse = new ProductLevel3CountResponse();
    Long totalItems = 0L;
    productLevel3CountResponse.getTotalItemsByCriterias().put(ProductLevel3SummaryCriteria.IN_PROGRESS, 0L);
    productLevel3CountResponse.getTotalItemsByCriterias().put(ProductLevel3SummaryCriteria.NEED_CORRECTION, 0L);
    for (Object[] response : productLevel3CountResponses) {
      String state = String.valueOf(response[0]);
      long totalItemsByCriteria = Long.parseLong(String.valueOf(response[1]));
      if (ProductLevel3WipSummaryCriteria.NEED_CORRECTION.name().equals(state)) {
        productLevel3CountResponse.getTotalItemsByCriterias()
            .put(ProductLevel3SummaryCriteria.NEED_CORRECTION, totalItemsByCriteria);
      } else {
        productLevel3CountResponse.getTotalItemsByCriterias().put(ProductLevel3SummaryCriteria.IN_PROGRESS,
            productLevel3CountResponse.getTotalItemsByCriterias().get(ProductLevel3SummaryCriteria.IN_PROGRESS)
                + totalItemsByCriteria);
      }
      totalItems += totalItemsByCriteria;
    }
    productLevel3CountResponse.setTotalItems(totalItems);
    return productLevel3CountResponse;
  }

  public ProductLevel3CountResponse generateProductLevel3CountByState(
    List<StateCountDTO> productLevel3StateCountResponses) {
    ProductLevel3CountResponse productLevel3CountResponse = new ProductLevel3CountResponse();
    long totalCount = 0;
    productLevel3CountResponse.getTotalItemsByCriterias().put(ProductLevel3SummaryCriteria.IN_PROGRESS, 0L);
    productLevel3CountResponse.getTotalItemsByCriterias().put(ProductLevel3SummaryCriteria.NEED_CORRECTION, 0L);

    for (StateCountDTO stateCountDTO : productLevel3StateCountResponses) {
      String state = stateCountDTO.getState();
      long count = stateCountDTO.getCount();

      if (ProductLevel3WipSummaryCriteria.NEED_CORRECTION.name().equals(state)) {
        productLevel3CountResponse.getTotalItemsByCriterias()
          .put(ProductLevel3SummaryCriteria.NEED_CORRECTION, count);
      } else if (ProductLevel3WipSummaryCriteria.IN_PROGRESS.name().equals(state)) {
        long currentInProgressCount = productLevel3CountResponse.getTotalItemsByCriterias()
          .get(ProductLevel3SummaryCriteria.IN_PROGRESS);
        productLevel3CountResponse.getTotalItemsByCriterias()
          .put(ProductLevel3SummaryCriteria.IN_PROGRESS, currentInProgressCount + count);
      }
      totalCount += count;
    }

    productLevel3CountResponse.setTotalItems(totalCount);
    return productLevel3CountResponse;
  }

}
