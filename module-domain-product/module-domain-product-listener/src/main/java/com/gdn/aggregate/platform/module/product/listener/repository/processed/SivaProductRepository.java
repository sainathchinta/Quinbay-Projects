package com.gdn.aggregate.platform.module.product.listener.repository.processed;

import java.util.List;
import java.util.Set;

import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface SivaProductRepository extends MongoRepository<SivaProduct, String> {

  boolean existsByFlashsale_ScheduleIdAndFlashsale_ActiveTrue(String scheduleId);

  boolean existsBySubFlashsale_ScheduleIdAndSubFlashsale_ActiveTrue(String scheduleId);

  boolean existsByFlashsale_ScheduleIdAndFlashsale_ActiveTrueAndFlashsale_GroupIds(String scheduleId, String groupId);

  boolean existsBySubFlashsale_ScheduleIdAndSubFlashsale_ActiveTrueAndSubFlashsale_GroupIds(String scheduleId, String groupId);

  List<SivaProduct> findByIdIn(Set<String> ids);
}
