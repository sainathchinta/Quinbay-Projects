package com.gdn.aggregate.platform.module.product.listener.repository.processed;

import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;

import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

public interface SivaItemRepository extends MongoRepository<SivaItem, String> {

  Stream<SivaItem> streamAllByProductSku(String productSku);

  SivaItem findFirstByItemCodeAndOnL2OrderByIdAsc(String itemCode, boolean onL2);

  List<SivaItem> findByIdIn(Set<String> ids);

  void deleteByProductSku(String productSku);

  @Query(value = "{'productSku': ?0}", fields = "{'_id': 1}")
  List<SivaItem> findIdsByProductSku(String productSku);

}
