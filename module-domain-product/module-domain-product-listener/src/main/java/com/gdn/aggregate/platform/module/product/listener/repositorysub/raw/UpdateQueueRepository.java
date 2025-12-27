package com.gdn.aggregate.platform.module.product.listener.repositorysub.raw;

import com.gdn.aggregate.platform.module.product.listener.model.raw.UpdateQueue;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.List;

public interface UpdateQueueRepository extends MongoRepository<UpdateQueue, String> {

  List<UpdateQueue> findAllByLevelAndTimestampBefore(int level, long timestamp, Pageable pageable);

}
