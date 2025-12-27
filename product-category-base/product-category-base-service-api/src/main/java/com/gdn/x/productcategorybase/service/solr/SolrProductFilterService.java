package com.gdn.x.productcategorybase.service.solr;

import java.util.List;
import java.util.Set;
import com.gdn.x.productcategorybase.entity.solr.SolrProductModel;
import com.gdn.x.productcategorybase.entity.solr.SolrProductResponse;
import org.springframework.data.domain.Page;

/**
 * Created by Kesha on 02/05/16.
 */
public interface SolrProductFilterService {

  /**
   * Method to filter duplicate Products from solr
   * @param model
   * @param size
   */

  Set<SolrProductResponse> filterDuplicateProducts(SolrProductModel model, int size);

  /**
   * Method to get all active product codes from solr
   *
   * @param model request filter
   * @param page  requested page number
   * @param size  requested record count
   *
   * @return Matching products products
   */
  Page<SolrProductResponse> getActiveProductIds(SolrProductModel model, int page, int size);
}
