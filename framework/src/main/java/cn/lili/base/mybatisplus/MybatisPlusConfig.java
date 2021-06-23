package cn.lili.base.mybatisplus;

import com.baomidou.mybatisplus.extension.plugins.PaginationInterceptor;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author Chopper
 */
@Configuration
@MapperScan({"cn.lili.modules.*.*.mapper", "cn.lili.modules.*.mapper"})
public class MybatisPlusConfig {
    /**
     * 分页插件，自动识别数据库类型
     */
    @Bean
    public PaginationInterceptor paginationInterceptor() {

        return new PaginationInterceptor();

        //阻断解析器，测试环境使用
//       PaginationInterceptor paginationInterceptor = new PaginationInterceptor();
//
//       List<ISqlParser> sqlParserList = new ArrayList<>();
//       //攻击 SQL 阻断解析器、加入解析链
//       sqlParserList.add(new BlockAttackSqlParser());
//       paginationInterceptor.setSqlParserList(sqlParserList);
//       return paginationInterceptor;
    }
}
