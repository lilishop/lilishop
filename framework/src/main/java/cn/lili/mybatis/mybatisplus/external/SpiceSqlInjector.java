package cn.lili.mybatis.mybatisplus.external;

import com.baomidou.mybatisplus.core.injector.AbstractMethod;
import com.baomidou.mybatisplus.core.injector.DefaultSqlInjector;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.extension.injector.methods.InsertBatchSomeColumn;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author paulG
 * @since 2022/7/18
 **/
@Component
public class SpiceSqlInjector extends DefaultSqlInjector {

    /**
     * 如果只需增加方法，保留mybatis plus自带方法，
     * 可以先获取super.getMethodList()，再添加add
     */
    @Override
    public List<AbstractMethod> getMethodList(Class<?> mapperClass, TableInfo tableInfo) {
        // 注意：此SQL注入器继承了DefaultSqlInjector(默认注入器)，调用了DefaultSqlInjector的getMethodList方法，保留了mybatis-plus的自带方法
        List<AbstractMethod> methodList = super.getMethodList(mapperClass, tableInfo);
        // 注入InsertBatchSomeColumn
        // 在!t.isLogicDelete()表示不要逻辑删除字段，!"update_time".equals(t.getColumn())表示不要字段名为 update_time 的字段,不对进行操作
        // methodList.add(new InsertBatchSomeColumn(t -> !t.isLogicDelete() && !"update_time".equals(t.getColumn())));
        // 要逻辑删除 t.isLogicDelete() 默认不要
        methodList.add(new InsertBatchSomeColumn(t -> !t.isLogicDelete()));
        return methodList;
    }
}
