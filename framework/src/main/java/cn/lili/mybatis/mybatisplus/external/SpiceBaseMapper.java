package cn.lili.mybatis.mybatisplus.external;

import java.util.List;

/**
 * @author paulG
 * @since 2022/7/18
 **/
public interface SpiceBaseMapper<T> {

    /**
     * 批量插入
     * {@link com.baomidou.mybatisplus.extension.injector.methods.InsertBatchSomeColumn}
     *
     * @param entityList 要插入的数据
     * @return 成功插入的数据条数
     */
    long insertBatchSomeColumn(List<T> entityList);
}
